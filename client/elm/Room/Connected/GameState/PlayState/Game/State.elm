module Game.State exposing (bareContextInit, contextInit, entitiesInit, gameInit, getFocus, getHoverIndex, hitTest, hoverDamage, hoverInit, tick)

import Animation.State as Animation
import Game.Types as Game exposing (Context, Entities, HandEntity, Hover(..), HoverBase, HoverSelf, StackEntity)
import Hand.Entities as Hand
import List.Extra as List
import Main.Types exposing (Flags)
import Math.Vector2 exposing (Vec2)
import Maybe.Extra as Maybe
import Model.State as Model
import Model.Types as Model exposing (Model)
import PlayState.Messages as PlayState
import Resolvable.State as Resolvable exposing (activeAnim, activeAnimDamage, activeModel, activeStackCard, resolving)
import Resolvable.Types as Resolvable
import Stack.Entities as Stack
import Stack.Types exposing (StackCard)
import Texture.State as Texture
import Texture.Types as Texture
import Util exposing (message)
import WhichPlayer.Types exposing (WhichPlayer(..))


gameInit : Model -> Game.Model
gameInit model =
    { res = Resolvable.init model []
    , focus = Nothing
    , mouse = Nothing
    , hover = NoHover
    , otherHover = NoHover
    , entities = { hand = [], otherHand = [], stack = [] }
    , passed = False
    }


contextInit : ( Int, Int ) -> Resolvable.Model -> Texture.Model -> Context
contextInit ( width, height ) res textures =
    let
        w =
            toFloat width

        h =
            toFloat height

        radius =
            if h < w then
                0.8 * h * 0.5

            else
                1.2 * w * 0.5

        anim =
            activeAnim res

        animDamage =
            activeAnimDamage res
    in
    { w = w
    , h = h
    , radius = radius
    , anim = anim
    , animDamage = animDamage
    , model = activeModel res
    , stackCard = activeStackCard res
    , tick = res.tick
    , progress = Animation.progress anim res.tick
    , textures = textures
    , resolving = resolving res
    }


bareContextInit : ( Int, Int ) -> Texture.Model -> Context
bareContextInit dimensions textures =
    let
        res =
            Resolvable.init Model.init []
    in
    contextInit dimensions res textures


entitiesInit : Entities
entitiesInit =
    { stack = []
    , hand = []
    , otherHand = []
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
tick { dimensions } dt model =
    let
        res =
            Resolvable.tick dt model.res

        ctx =
            contextInit dimensions res Texture.init

        hoverHand =
            getHoverHand model

        hoverStack =
            getHoverStack model

        ( hover, hoverMsg ) =
            hoverUpdate model.hover hoverHand hoverStack dt

        otherHover =
            hoverTick model.otherHover dt

        focus =
            getFocus ctx hoverHand hoverStack

        newModel =
            { model
                | res = res
                , hover = hover
                , otherHover = otherHover
                , entities =
                    { stack = Stack.entities ctx
                    , hand = Hand.entities model.hover ctx
                    , otherHand = Hand.otherEntities model.otherHover ctx
                    }
                , focus = focus
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


hitTest : Vec2 -> Float -> { a | position : Vec2 } -> Bool
hitTest pos dist { position } =
    Math.Vector2.distance position pos < dist


getHoverHand : Game.Model -> Maybe HandEntity
getHoverHand { entities, mouse } =
    Maybe.andThen
        (\pos -> List.find (hitTest pos 32) entities.hand)
        mouse


getHoverStack : Game.Model -> Maybe StackEntity
getHoverStack { entities, mouse } =
    Maybe.andThen
        (\pos -> List.find (hitTest pos 64) entities.stack)
        mouse


getFocus : Context -> Maybe HandEntity -> Maybe StackEntity -> Maybe StackCard
getFocus { stackCard } hoverHand hoverStack =
    let
        hoverCard =
            Maybe.or
                (Maybe.map (\{ card, owner } -> { owner = owner, card = card }) hoverStack)
                (Maybe.map (\{ card } -> { owner = PlayerA, card = card }) hoverHand)
    in
    Maybe.or stackCard hoverCard
