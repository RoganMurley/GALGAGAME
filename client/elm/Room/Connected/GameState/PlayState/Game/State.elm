module Game.State exposing (bareContextInit, contextInit, entitiesInit, gameInit, getFocus, getHoverIndex, hold, hoverDamage, hoverInit, tick)

import Animation.State as Animation
import Animation.Types as Animation
import Assets.State as Assets
import Assets.Types as Assets
import Buttons.State as Buttons
import Buttons.Types as Buttons exposing (ButtonType(..), Buttons)
import Card.Types exposing (Card)
import Collision exposing (hitTest3d)
import Game.Types as Game exposing (Context, Entities, Feedback, HandEntity, StackEntity)
import Hand.Entities as Hand
import Hand.State exposing (maxHandLength)
import Holding.State as Holding
import Holding.Types exposing (Holding(..))
import Hover exposing (Hover(..), HoverBase, HoverSelf)
import List.Extra as List
import Main.Types exposing (Flags)
import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (Vec3, vec3)
import Maybe.Extra as Maybe
import Model.State as Model
import Model.Types as Model exposing (Life, Model)
import PlayState.Messages as PlayState
import Render.Uniforms as Uniforms
import Resolvable.State as Resolvable exposing (activeAnim, activeAnimDamage, activeModel, resolving)
import Resolvable.Types as Resolvable
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
    , buttons = Buttons.empty
    , holding = NoHolding
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
    , wheel = []
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
            hoverUpdate model.hover hoverHand hoverStack holding dt

        otherHover =
            hoverTick model.otherHover dt

        focus =
            getFocus ctx hoverHand hoverStack model.holding

        feedback =
            feedbackTick model.feedback dt

        holding =
            Holding.tick model.holding ctx.mouseRay dt

        newModel =
            { model
                | res = res
                , hover = hover
                , otherHover = otherHover
                , entities =
                    { stack = Stack.entities ctx
                    , hand = Hand.entities model.hover ctx
                    , otherHand = Hand.otherEntities model.otherHover ctx
                    , wheel = Stack.wheelEntities ctx
                    }
                , focus = focus
                , feedback = feedback
                , vfx = Vfx.tick dt model.vfx ctx
                , buttons = buttonEntities model.passed mouse dt model.buttons ctx
                , holding = holding
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


hoverUpdate : HoverSelf -> Maybe HandEntity -> Maybe StackEntity -> Holding -> Float -> ( HoverSelf, Cmd PlayState.Msg )
hoverUpdate oldHover handEntity stackEntity holding dt =
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
    case holding of
        NoHolding ->
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

        _ ->
            ( NoHover, Cmd.none )


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


getFocus : Context -> Maybe HandEntity -> Maybe StackEntity -> Holding -> Maybe StackCard
getFocus { anim, model } hoverHand hoverStack holding =
    let
        hoverCard =
            Maybe.or
                (Maybe.map (\{ card, owner } -> { owner = owner, card = card }) hoverStack)
                (Maybe.map (\{ card } -> { owner = PlayerA, card = card }) hoverHand)

        stackCard =
            model.stack.wheel0
    in
    case holding of
        NoHolding ->
            case anim of
                Animation.Play _ _ _ _ ->
                    Nothing

                _ ->
                    Maybe.or stackCard hoverCard

        Holding { card } ->
            Just { card = card, owner = PlayerA }


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


buttonEntities : Bool -> Maybe Vec2 -> Float -> Buttons -> Context -> Buttons
buttonEntities passed mouse dt buttons { w, h, model, radius, resolving } =
    let
        handFull =
            List.length model.hand == maxHandLength

        yourTurn =
            model.turn == PlayerA

        disabled =
            handFull || not yourTurn || passed || resolving

        x =
            w * 0.5 + 0.7 * radius

        y =
            h * 0.5 + 0.8 * radius

        scale =
            0.12 * radius
    in
    Buttons.fromList <|
        if not disabled then
            [ Buttons.entity
                "go"
                { x = x
                , y = y
                , width = scale
                , height = scale
                , btn =
                    TextButton
                        { font = "Futura"
                        , text = "GO?"
                        , textColor = vec3 (0 / 255) (0 / 255) (80 / 255)
                        , bgColor = vec3 (244 / 255) (241 / 255) (94 / 255)
                        , options = [ Buttons.HoverText "GO!", Buttons.Circular ]
                        }
                , disabled = disabled
                }
                dt
                mouse
                buttons
            ]

        else
            [ Buttons.entity
                (if handFull then
                    "goHandFull"

                 else
                    "goDisabled"
                )
                { x = x
                , y = y
                , width = scale
                , height = scale
                , btn =
                    TextButton
                        { font = "Futura"
                        , text = "GO?"
                        , textColor = vec3 (0 / 255) (0 / 255) (0 / 255)
                        , bgColor = vec3 (70 / 255) (70 / 255) (70 / 255)
                        , options = [ Buttons.Circular ]
                        }
                , disabled = disabled
                }
                dt
                mouse
                buttons
            ]


hold : Card -> Int -> Maybe Collision.Ray -> ( Life, Life ) -> Game.Model -> Game.Model
hold card handIndex ray dmg game =
    { game
        | holding = Holding.init card handIndex ray dmg
    }
