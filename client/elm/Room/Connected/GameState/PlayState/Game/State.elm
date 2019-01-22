module Game.State exposing (bareContextInit, contextInit, entitiesInit, gameInit, getFocus, hitTest, hoverDamage, hoverInit, tick)

import Animation.State as Animation
import Game.Types as Game exposing (Context, Entities, HandEntity, Hover, StackEntity)
import Hand.Entities as Hand
import List.Extra as List
import Main.Types exposing (Flags)
import Math.Vector2 exposing (Vec2)
import Maybe.Extra as Maybe
import Model.State as Model
import Model.Types exposing (Model)
import PlayState.Messages as PlayState
import Resolvable.State as Resolvable exposing (activeAnim, activeModel, activeStackCard)
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
    , hover = Nothing
    , otherHover = Nothing
    , entities = { hand = [], otherHand = [], stack = [] }
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
    in
    { w = w
    , h = h
    , radius = radius
    , anim = anim
    , model = activeModel res
    , stackCard = activeStackCard res
    , tick = res.tick
    , progress = Animation.progress anim res.tick
    , textures = textures
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


hoverInit : Maybe Int -> { a | index : Int, tick : Float } -> Hover a
hoverInit mIndex base =
    Maybe.map (\index -> { base | index = index }) mIndex


tick : Flags -> Float -> Game.Model -> ( Game.Model, Cmd PlayState.Msg )
tick { dimensions } dt model =
    let
        res =
            Resolvable.tick dt model.res

        ctx =
            contextInit dimensions res Texture.init

        hoverHand =
            getHoverHand model

        ( hover, hoverMsg ) =
            hoverUpdate model.hover hoverHand dt

        otherHover =
            hoverTick model.otherHover dt

        focus =
            getFocus ctx model hoverHand

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


hoverTick : Hover a -> Float -> Hover a
hoverTick hover dt =
    Maybe.map
        (\h ->
            if h.tick < 70 then
                { h | tick = h.tick + dt }

            else
                { h | tick = 70 }
        )
        hover


hoverUpdate : Hover { dmg : Int } -> Maybe HandEntity -> Float -> ( Hover { dmg : Int }, Cmd PlayState.Msg )
hoverUpdate oldHover hoverHand dt =
    let
        hoverIndex =
            Maybe.map .index hoverHand

        oldHoverIndex =
            Maybe.map .index oldHover

        newHover =
            hoverTick oldHover dt
    in
    if hoverIndex == oldHoverIndex then
        ( newHover, Cmd.none )

    else
        ( hoverInit hoverIndex { index = 0, tick = 0, dmg = 0 }
        , message <|
            PlayState.PlayingOnly <|
                PlayState.HoverCard
                    hoverIndex
        )


hoverDamage : Hover { dmg : Int } -> Int -> Hover { dmg : Int }
hoverDamage hover dmg =
    Maybe.map (\h -> { h | dmg = dmg }) hover


hitTest : Vec2 -> Float -> { a | position : Vec2 } -> Bool
hitTest pos dist { position } =
    Math.Vector2.distance position pos < dist


getHoverHand : Game.Model -> Maybe HandEntity
getHoverHand { entities, mouse } =
    Maybe.andThen
        (\pos -> List.find (hitTest pos 28) entities.hand)
        mouse


getHoverStack : Game.Model -> Maybe StackEntity
getHoverStack { entities, mouse } =
    Maybe.andThen
        (\pos -> List.find (hitTest pos 64) entities.stack)
        mouse


getFocus : Context -> Game.Model -> Maybe HandEntity -> Maybe StackCard
getFocus { stackCard } game hoverHand =
    let
        hoverCard =
            Maybe.or
                (Maybe.map (\{ card, owner } -> { owner = owner, card = card }) <|
                    getHoverStack game
                )
                (Maybe.map (\{ card } -> { owner = PlayerA, card = card }) <|
                    hoverHand
                )
    in
    Maybe.or stackCard hoverCard
