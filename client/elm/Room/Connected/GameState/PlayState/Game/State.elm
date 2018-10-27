module Game.State exposing (contextInit, entitiesInit, gameInit, getFocus, hitTest, tick)

import Animation.State as Animation
import Game.Types as Game exposing (Context, Entities)
import Hand.Entities as Hand
import List.Extra as List
import Main.Types exposing (Flags)
import Math.Vector2 exposing (Vec2)
import Maybe.Extra as Maybe
import Model.Types exposing (Model)
import Resolvable.State as Resolvable exposing (activeAnim, activeModel, activeStackCard)
import Resolvable.Types as Resolvable
import Stack.Entities as Stack
import Stack.Types exposing (StackCard)
import Texture.State as Texture
import Texture.Types as Texture
import WhichPlayer.Types exposing (WhichPlayer(..))


gameInit : Model -> Game.Model
gameInit model =
    { focus = Nothing
    , mouse = Nothing
    , entities = { hand = [], otherHand = [], stack = [] }
    , res = Resolvable.init model []
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


entitiesInit : Entities
entitiesInit =
    { stack = []
    , hand = []
    , otherHand = []
    }


tick : Flags -> Float -> Game.Model -> Game.Model
tick { dimensions } dt model =
    let
        res =
            Resolvable.tick dt model.res

        ctx =
            contextInit dimensions res Texture.init
    in
    { model
        | res = res
        , entities =
            { stack = Stack.entities ctx
            , hand = Hand.entities ctx
            , otherHand = Hand.otherEntities ctx
            }
        , focus = getFocus ctx model
    }


hitTest : Vec2 -> Float -> { a | position : Vec2 } -> Bool
hitTest pos dist { position } =
    Math.Vector2.distance position pos < dist


getFocus : Context -> Game.Model -> Maybe StackCard
getFocus { stackCard } { entities, mouse } =
    Maybe.map
        mouse
        (\pos ->
            let
                hoverCard =
                    Maybe.or
                        (Maybe.map (\{ card, owner } -> { owner = owner, card = card }) <|
                            List.find (hitTest pos 64) entities.stack
                        )
                        (Maybe.map (\{ card } -> { owner = PlayerA, card = card }) <|
                            List.find (hitTest pos 28) entities.hand
                        )
            in
            Maybe.or stackCard hoverCard
        )
