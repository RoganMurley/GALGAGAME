module Game.State exposing (bareContextInit, contextInit, entitiesInit, gameInit, getFocus, getHoverIndex, hold, hoverDamage, hoverInit, tick)

import Animation.State as Animation
import Animation.Types as Animation
import Assets.State as Assets
import Assets.Types as Assets
import Buttons.State as Buttons
import Buttons.Types as Buttons exposing (ButtonType(..), Buttons)
import Card.Types exposing (Card)
import Chat.Types as Chat
import Collision exposing (hitTest, hitTest3d)
import Game.Types as Game exposing (Context, Entities, Focus(..), HandEntity, OtherHandEntity, PlayerEntity, StackEntity)
import Hand.Entities as Hand
import Hand.State exposing (maxHandLength)
import Holding.State as Holding
import Holding.Types exposing (Holding(..))
import Hover exposing (Hover(..), HoverBase, HoverDamage(..), HoverSelf)
import List.Extra as List
import Main.Types exposing (Flags)
import Math.Vector2 exposing (Vec2, vec2)
import Math.Vector3 exposing (Vec3, vec3)
import Maybe.Extra as Maybe
import Model.State as Model
import Model.Types as Model exposing (Model)
import Mouse exposing (MouseState(..))
import PlayState.Messages as PlayState
import Render.Uniforms as Uniforms
import Resolvable.State as Resolvable exposing (activeAnim, activeAnimDamage, activeModel, resolving)
import Resolvable.Types as Resolvable
import Stack.Entities as Stack
import Stack.Types exposing (StackCard)
import Tutorial
import Unproject
import Util exposing (message)
import Vfx.State as Vfx
import Vfx.Types as Vfx
import WhichPlayer.Types exposing (WhichPlayer(..))


gameInit : Model -> Game.Model
gameInit model =
    { res = Resolvable.init model []
    , focus = NoFocus
    , hover = NoHover
    , otherHover = NoHover
    , entities = entitiesInit
    , passed = False
    , vfx = Vfx.init
    , buttons = Buttons.empty
    , holding = NoHolding
    , timeLeft = Nothing
    , tutorial = Tutorial.init
    , passive = False
    , cpu = False
    }


contextInit : ( Int, Int ) -> Resolvable.Model -> Assets.Model -> MouseState -> Vfx.Model -> Context
contextInit ( width, height ) res { textures, fonts } mouseState vfx =
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

        mouseVec =
            Mouse.getVec mouseState
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
    , mouse = mouseState
    , mouseRay = Unproject.rayFromMouse mouseVec coords perspective Uniforms.camera3d
    , perspective = perspective
    , ortho = ortho
    , camera2d = Uniforms.camera2d
    , camera3d = Uniforms.camera3d
    , vfx = vfx
    }


bareContextInit : ( Int, Int ) -> Assets.Model -> MouseState -> Context
bareContextInit dimensions assets mouseState =
    let
        res =
            Resolvable.init Model.init []
    in
    contextInit dimensions res assets mouseState Vfx.init


entitiesInit : Entities
entitiesInit =
    { stack = []
    , hand = []
    , otherHand = []
    , wheel = []
    , players = []
    }


hoverInit : Maybe Int -> Maybe Int -> Maybe Int -> HoverBase a -> Hover a
hoverInit handIndex otherHandIndex stackIndex base =
    case ( ( handIndex, otherHandIndex ), stackIndex ) of
        ( ( Just index, _ ), _ ) ->
            HoverHand { base | index = index }

        ( ( _, Just index ), _ ) ->
            HoverOtherHand { base | index = index }

        ( _, Just index ) ->
            HoverStack { base | index = index }

        _ ->
            NoHover


tick : Flags -> Float -> Game.Model -> Chat.Model -> ( Game.Model, Cmd PlayState.Msg )
tick { dimensions, mouse } dt model chat =
    let
        res =
            Resolvable.tick dt model.res

        ctx =
            contextInit dimensions res Assets.init mouse model.vfx

        hoverHand =
            getHoverHand model ctx.mouseRay

        hoverOtherHand =
            getHoverOtherHand model ctx.mouseRay

        hoverStack =
            getHoverStack model ctx.mouseRay

        focusPlayer =
            getFocusPlayer model (Mouse.getVec ctx.mouse)

        ( hover, hoverMsg ) =
            hoverUpdate model.hover hoverHand hoverOtherHand hoverStack holding dt

        otherHover =
            hoverTick model.otherHover dt

        focus =
            getFocus ctx hoverHand hoverOtherHand hoverStack model.holding focusPlayer

        holding =
            Holding.tick model.holding ctx.mouseRay dt

        timeLeft =
            Maybe.map (\t -> t - dt) model.timeLeft

        newModel =
            { model
                | res = res
                , hover = hover
                , otherHover = otherHover
                , entities =
                    { stack = Stack.entities ctx
                    , hand = Hand.entities model.hover holding ctx
                    , otherHand = Hand.otherEntities model.hover model.otherHover ctx
                    , wheel = Stack.wheelEntities ctx
                    , players = playerEntities ctx
                    }
                , focus = focus
                , vfx = Vfx.tick dt model.vfx timeLeft ctx
                , buttons =
                    buttonEntities
                        model.passed
                        mouse
                        dt
                        model.buttons
                        chat
                        model.tutorial
                        ctx
                , holding = holding
                , timeLeft = timeLeft
            }
    in
    ( newModel, hoverMsg )


getHoverIndex : Hover a -> Maybe Int
getHoverIndex hover =
    case hover of
        HoverHand { index } ->
            Just index

        HoverOtherHand { index } ->
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

        HoverOtherHand hoverOtherHand ->
            HoverOtherHand <| baseTick hoverOtherHand

        HoverStack hoverStack ->
            HoverStack <| baseTick hoverStack

        NoHover ->
            NoHover


hoverUpdate : HoverSelf -> Maybe HandEntity -> Maybe OtherHandEntity -> Maybe StackEntity -> Holding -> Float -> ( HoverSelf, Cmd PlayState.Msg )
hoverUpdate oldHover handEntity otherHandEntity stackEntity holding dt =
    let
        hoverHandIndex =
            Maybe.map .index handEntity

        hoverOtherHandIndex =
            Maybe.map .index otherHandEntity

        hoverStackIndex =
            Maybe.map .index stackEntity

        hoverIndex =
            List.foldr Maybe.or
                Nothing
                [ hoverHandIndex, hoverStackIndex, hoverOtherHandIndex ]

        oldHoverIndex =
            getHoverIndex oldHover
    in
    case holding of
        NoHolding ->
            if hoverIndex == oldHoverIndex then
                ( hoverTick oldHover dt, Cmd.none )

            else
                let
                    freshHover =
                        hoverInit
                            hoverHandIndex
                            hoverOtherHandIndex
                            hoverStackIndex
                            { index = 0
                            , tick = 0
                            , dmg = ( HoverDamage 0, HoverDamage 0 )
                            }
                in
                ( freshHover
                , message <|
                    PlayState.PlayingOnly <|
                        PlayState.HoverCard
                            freshHover
                )

        _ ->
            ( NoHover, Cmd.none )


hoverDamage : HoverSelf -> ( HoverDamage, HoverDamage ) -> HoverSelf
hoverDamage hover dmg =
    case hover of
        HoverHand hoverHand ->
            HoverHand { hoverHand | dmg = dmg }

        HoverOtherHand hoverOtherHand ->
            HoverOtherHand { hoverOtherHand | dmg = dmg }

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
                    (hitTest3d ray 0.1)
                <|
                    List.reverse entities.hand
            )


getHoverOtherHand : Game.Model -> Maybe { origin : Vec3, direction : Vec3 } -> Maybe OtherHandEntity
getHoverOtherHand { entities } mRay =
    mRay
        |> Maybe.andThen
            (\ray ->
                List.find
                    (hitTest3d ray 0.1)
                <|
                    List.reverse entities.otherHand
            )
        |> Maybe.andThen
            (\entity -> Maybe.map (always entity) entity.mCard)


getHoverStack : Game.Model -> Maybe { origin : Vec3, direction : Vec3 } -> Maybe StackEntity
getHoverStack { entities } mRay =
    mRay
        |> Maybe.andThen
            (\ray ->
                List.find
                    (hitTest3d ray 0.18)
                    entities.stack
            )


getFocusPlayer : Game.Model -> Maybe Vec2 -> Maybe WhichPlayer
getFocusPlayer { entities } mMousePos =
    mMousePos
        |> Maybe.andThen
            (\mousePos ->
                List.find
                    (\entity -> hitTest mousePos entity.scale entity)
                    entities.players
                    |> Maybe.map .which
            )


getFocus : Context -> Maybe HandEntity -> Maybe OtherHandEntity -> Maybe StackEntity -> Holding -> Maybe WhichPlayer -> Focus
getFocus { anim, model } hoverHand hoverOtherHand hoverStack holding player =
    let
        hoverCard =
            List.foldr
                Maybe.or
                Nothing
                [ Maybe.map (\{ card, owner } -> { owner = owner, card = card }) hoverStack
                , Maybe.map (\{ card } -> { owner = PlayerA, card = card }) hoverHand
                , hoverOtherHand
                    |> Maybe.andThen .mCard
                    |> Maybe.andThen
                        (\card -> Just { card = card, owner = PlayerB })
                ]

        stackCard =
            model.stack.wheel0

        cardFocus : Maybe StackCard -> Focus
        cardFocus mStackCard =
            case mStackCard of
                Just sc ->
                    FocusCard sc

                Nothing ->
                    NoFocus
    in
    case holding of
        NoHolding ->
            case anim of
                Animation.Play _ _ _ _ ->
                    NoFocus

                _ ->
                    case player of
                        Just which ->
                            FocusPlayer which

                        Nothing ->
                            cardFocus <| Maybe.or stackCard hoverCard

        Holding { card } ->
            FocusCard { card = card, owner = PlayerA }


buttonEntities : Bool -> MouseState -> Float -> Buttons -> Chat.Model -> Tutorial.Model -> Context -> Buttons
buttonEntities passed mouseState dt buttons chat tutorial { w, h, model, radius, resolving } =
    let
        handFull =
            List.length model.hand == maxHandLength

        yourTurn =
            model.turn == PlayerA

        disabled =
            handFull
                || not yourTurn
                || passed
                || resolving

        x =
            w * 0.5 + 0.65 * radius

        y =
            h * 0.5 + 0.8 * radius

        scale =
            0.12 * radius

        showChat =
            case tutorial.step of
                Just _ ->
                    False

                Nothing ->
                    True
    in
    Buttons.fromList <|
        (if not disabled then
            Buttons.entity
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
                mouseState
                buttons

         else
            Buttons.entity
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
                mouseState
                buttons
        )
            :: (if showChat then
                    [ Buttons.entity "toggleChat"
                        { x = w * 0.5 - 0.65 * radius
                        , y = y
                        , width = scale
                        , height = scale
                        , btn =
                            TextButton
                                { font = "Futura"
                                , text =
                                    if chat.visible then
                                        "chatClose"

                                    else
                                        "chat"
                                , textColor = vec3 (0 / 255) (0 / 255) (0 / 255)
                                , bgColor = vec3 (244 / 255) (241 / 255) (94 / 255)
                                , options = [ Buttons.Circular, Buttons.IsIcon ]
                                }
                        , disabled = False
                        }
                        dt
                        mouseState
                        buttons
                    ]

                else
                    []
               )


hold : Card -> Int -> Maybe Collision.Ray -> ( HoverDamage, HoverDamage ) -> Game.Model -> Game.Model
hold card handIndex ray dmg game =
    { game
        | holding = Holding.init card handIndex ray dmg
    }


playerEntities : Context -> List PlayerEntity
playerEntities ctx =
    let
        { anim, w, h, radius } =
            ctx

        scale =
            0.15 * radius

        ( xOffset, yOffset ) =
            ( 0.65 * radius, 0.875 * radius )

        shake =
            Animation.animShake anim PlayerA ctx.tick

        otherShake =
            Animation.animShake anim PlayerB ctx.tick
    in
    [ { scale = scale
      , position =
            Math.Vector2.add
                (vec2 (w * 0.5 + xOffset) (h * 0.5 - yOffset))
                (vec2 shake shake)
      , rotation = 0
      , which = PlayerA
      }
    , { scale = scale
      , position =
            Math.Vector2.add
                (vec2 (w * 0.5 - xOffset) (h * 0.5 - yOffset))
                (vec2 -otherShake otherShake)
      , rotation = 0
      , which = PlayerB
      }
    ]
