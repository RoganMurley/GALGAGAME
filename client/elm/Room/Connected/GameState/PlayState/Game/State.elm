module Game.State exposing (bareContextInit, contextInit, entitiesInit, gameInit, getFocus, getHoverIndex, hoverDamage, hoverInit, tick)

import Animation.State as Animation
import Animation.Types as Animation
import Assets.State as Assets
import Assets.Types as Assets
import Collision exposing (hitTest, hitTest3d)
import Connected.Messages as Connected
import Game.Types as Game exposing (ButtonEntity, Context, Entities, Feedback, HandEntity, StackEntity)
import GameState.Messages as GameState
import Hand.Entities as Hand
import Hand.State exposing (maxHandLength)
import Hover exposing (Hover(..), HoverBase, HoverSelf)
import List.Extra as List
import Main.Messages as Main
import Main.Types exposing (Flags)
import Math.Vector2 exposing (Vec2, vec2)
import Math.Vector3 exposing (Vec3)
import Maybe.Extra as Maybe
import Model.State as Model
import Model.Types as Model exposing (Model)
import PlayState.Messages as PlayState
import Render.Uniforms as Uniforms
import Resolvable.State as Resolvable exposing (activeAnim, activeAnimDamage, activeModel, activeStackCard, resolving)
import Resolvable.Types as Resolvable
import Room.Messages as Room
import Stack.Entities as Stack
import Stack.Types exposing (StackCard)
import Unproject
import Util exposing (message)
import Vfx.State as Vfx
import WhichPlayer.Types exposing (WhichPlayer(..))


gameInit : Model -> Game.Model
gameInit model =
    { res = Resolvable.init model []
    , focus = Nothing
    , hover = NoHover
    , otherHover = NoHover
    , entities = entitiesInit
    , passed = False
    , feedback = []
    , vfx = Vfx.init
    }


contextInit : ( Int, Int ) -> Resolvable.Model -> Assets.Model -> Maybe Vec2 -> Context
contextInit ( width, height ) res { textures, fonts } mouse =
    let
        coords =
            { w =
                toFloat width
            , h =
                toFloat height
            }

        radius =
            if coords.h < coords.w then
                0.8 * coords.h * 0.5

            else
                1.2 * coords.w * 0.5

        anim =
            activeAnim res

        animDamage =
            activeAnimDamage res

        perspective =
            Uniforms.perspective coords

        ortho =
            Uniforms.ortho coords
    in
    { w = coords.w
    , h = coords.h
    , radius = radius
    , anim = anim
    , animDamage = animDamage
    , model = activeModel res
    , stackCard = activeStackCard res
    , tick = res.tick
    , progress = Animation.progress anim res.tick
    , textures = textures
    , fonts = fonts
    , resolving = resolving res
    , mouse = mouse
    , mouseRay = Unproject.rayFromMouse mouse coords perspective Uniforms.camera3d
    , perspective = perspective
    , ortho = ortho
    , camera2d = Uniforms.camera2d
    , camera3d = Uniforms.camera3d
    }


bareContextInit : ( Int, Int ) -> Assets.Model -> Maybe Vec2 -> Context
bareContextInit dimensions assets mouse =
    let
        res =
            Resolvable.init Model.init []
    in
    contextInit dimensions res assets mouse


entitiesInit : Entities
entitiesInit =
    { stack = []
    , hand = []
    , otherHand = []
    , buttons = []
    }


hoverInit : Maybe Int -> Maybe Int -> HoverBase a -> Hover a
hoverInit handIndex stackIndex base =
    case ( handIndex, stackIndex ) of
        ( Just index, _ ) ->
            HoverHand { base | index = index }

        ( _, Just index ) ->
            HoverStack { base | index = index }

        _ ->
            NoHover


tick : Flags -> Float -> Game.Model -> ( Game.Model, Cmd PlayState.Msg )
tick { dimensions, mouse } dt model =
    let
        res =
            Resolvable.tick dt model.res

        ctx =
            contextInit dimensions res Assets.init mouse

        hoverHand =
            getHoverHand model ctx.mouseRay

        hoverStack =
            getHoverStack model ctx.mouseRay

        ( hover, hoverMsg ) =
            hoverUpdate model.hover hoverHand hoverStack dt

        otherHover =
            hoverTick model.otherHover dt

        focus =
            getFocus ctx hoverHand hoverStack

        feedback =
            feedbackTick model.feedback dt

        newModel =
            { model
                | res = res
                , hover = hover
                , otherHover = otherHover
                , entities =
                    { stack = Stack.entities ctx
                    , hand = Hand.entities model.hover ctx
                    , otherHand = Hand.otherEntities model.otherHover ctx
                    , buttons = buttonEntities model.passed mouse ctx
                    }
                , focus = focus
                , feedback = feedback
                , vfx = Vfx.tick dt model.vfx ctx
            }
    in
    ( newModel, hoverMsg )


getHoverIndex : Hover a -> Maybe Int
getHoverIndex hover =
    case hover of
        HoverHand { index } ->
            Just index

        HoverStack { index } ->
            Just index

        NoHover ->
            Nothing


hoverTick : Hover a -> Float -> Hover a
hoverTick hover dt =
    let
        baseTick : HoverBase a -> HoverBase a
        baseTick base =
            if base.tick < 70 then
                { base | tick = base.tick + dt }

            else
                { base | tick = 70 }
    in
    case hover of
        HoverHand hoverHand ->
            HoverHand <| baseTick hoverHand

        HoverStack hoverStack ->
            HoverStack <| baseTick hoverStack

        NoHover ->
            NoHover


hoverUpdate : HoverSelf -> Maybe HandEntity -> Maybe StackEntity -> Float -> ( HoverSelf, Cmd PlayState.Msg )
hoverUpdate oldHover handEntity stackEntity dt =
    let
        hoverHandIndex =
            Maybe.map .index handEntity

        hoverStackIndex =
            Maybe.map .index stackEntity

        hoverIndex =
            Maybe.or hoverHandIndex hoverStackIndex

        oldHoverIndex =
            getHoverIndex oldHover

        newHover =
            hoverTick oldHover dt
    in
    if hoverIndex == oldHoverIndex then
        ( newHover, Cmd.none )

    else
        let
            freshHover =
                hoverInit hoverHandIndex hoverStackIndex { index = 0, tick = 0, dmg = ( 0, 0 ) }
        in
        ( freshHover
        , message <|
            PlayState.PlayingOnly <|
                PlayState.HoverCard
                    freshHover
        )


hoverDamage : HoverSelf -> ( Model.Life, Model.Life ) -> HoverSelf
hoverDamage hover dmg =
    case hover of
        HoverHand hoverHand ->
            HoverHand { hoverHand | dmg = dmg }

        HoverStack hoverStack ->
            HoverStack { hoverStack | dmg = dmg }

        NoHover ->
            NoHover


getHoverHand : Game.Model -> Maybe { origin : Vec3, direction : Vec3 } -> Maybe HandEntity
getHoverHand { entities } mRay =
    mRay
        |> Maybe.andThen
            (\ray ->
                List.find
                    (hitTest3d ray 0.12)
                    entities.hand
            )


getHoverStack : Game.Model -> Maybe { origin : Vec3, direction : Vec3 } -> Maybe StackEntity
getHoverStack { entities } mRay =
    mRay
        |> Maybe.andThen
            (\ray ->
                List.find
                    (hitTest3d ray 0.18)
                    entities.stack
            )


getFocus : Context -> Maybe HandEntity -> Maybe StackEntity -> Maybe StackCard
getFocus { anim, stackCard } hoverHand hoverStack =
    let
        hoverCard =
            Maybe.or
                (Maybe.map (\{ card, owner } -> { owner = owner, card = card }) hoverStack)
                (Maybe.map (\{ card } -> { owner = PlayerA, card = card }) hoverHand)
    in
    case anim of
        Animation.Play _ _ _ ->
            Nothing

        _ ->
            Maybe.or stackCard hoverCard


feedbackTick : List Feedback -> Float -> List Feedback
feedbackTick feedback dt =
    Maybe.values <|
        List.map
            (\f ->
                let
                    progress =
                        f.progress - dt
                in
                if progress < 0 then
                    Nothing

                else
                    Just { f | progress = progress }
            )
            feedback


buttonEntities : Bool -> Maybe Vec2 -> Context -> List ButtonEntity
buttonEntities passed mouse { w, h, model, radius, resolving } =
    let
        position =
            vec2 (w * 0.5 + 0.64 * radius) (h * 0.5 + 0.67 * radius)

        handFull =
            List.length model.hand == maxHandLength

        yourTurn =
            model.turn == PlayerA

        isDisabled =
            handFull || not yourTurn || passed || resolving

        isHover =
            case mouse of
                Just mousePos ->
                    hitTest position 32 { position = mousePos }

                Nothing ->
                    False

        playMsg =
            Main.RoomMsg
                << Room.ConnectedMsg
                << Connected.GameStateMsg
                << GameState.PlayStateMsg
                << PlayState.PlayingOnly

        onClick : Maybe Main.Msg
        onClick =
            if not isDisabled then
                Just <| playMsg <| PlayState.TurnOnly PlayState.EndTurn

            else if handFull && not resolving then
                Just <| playMsg PlayState.IllegalPass

            else
                Nothing
    in
    [ { position = position
      , scale = 32
      , rotation = 0
      , text = "GO"
      , font = "Futura"
      , disabled = isDisabled
      , hover = isHover
      , onClick = onClick
      }
    ]
