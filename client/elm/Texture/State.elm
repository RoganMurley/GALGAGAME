module Texture.State exposing (..)

import Dict
import Task
import Texture.Messages exposing (Msg(..))
import Texture.Types exposing (Model)
import WebGL.Texture exposing (Texture)


init : Model
init =
    { textures = Dict.empty }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg m =
    case msg of
        TexturesLoaded textures ->
            ( saveList m textures, Cmd.none )

        TexturesError error ->
            let
                _ =
                    Debug.log "Error loading textures: " error
            in
                ( m, Cmd.none )


save : Model -> ( String, Texture ) -> Model
save model ( name, tex ) =
    { model | textures = Dict.insert name tex model.textures }


saveList : Model -> List ( String, Texture ) -> Model
saveList model textures =
    { model
        | textures =
            Dict.union (Dict.fromList textures) model.textures
    }


load : Model -> String -> Maybe Texture
load { textures } name =
    Dict.get name textures


defaultOptions : WebGL.Texture.Options
defaultOptions =
    WebGL.Texture.defaultOptions


fetchTextures : Cmd Msg
fetchTextures =
    let
        loader : ( String, String ) -> Task.Task WebGL.Texture.Error ( String, Texture )
        loader ( name, texturePath ) =
            let
                task : Task.Task WebGL.Texture.Error Texture
                task =
                    WebGL.Texture.loadWith
                        { defaultOptions
                            | magnify = WebGL.Texture.nearest
                            , minify = WebGL.Texture.nearest
                        }
                        texturePath
            in
                Task.map
                    (\texture -> ( name, texture ))
                    task
    in
        texturePaths
            |> List.map loader
            |> Task.sequence
            |> Task.attempt
                (\result ->
                    case result of
                        Err error ->
                            TexturesError error

                        Ok textures ->
                            TexturesLoaded textures
                )


texturePaths : List ( String, String )
texturePaths =
    [ ( "cross", "/img/textures/cross.png" )
    , ( "feint", "/img/textures/feint.png" )
    , ( "noise", "/img/textures/noise.png" )
    , ( "striker/dagger.svg", "/img/textures/dagger.png" )
    , ( "striker/fireball.svg", "/img/textures/fireball.png" )
    , ( "striker/offering.svg", "/img/textures/offering.png" )
    , ( "striker/confound.svg", "/img/textures/confound.png" )
    , ( "bouncer/boomerang.svg", "/img/textures/boomerang.png" )
    , ( "bouncer/overwhelm.svg", "/img/textures/overwhelm.png" )
    , ( "bouncer/echo.svg", "/img/textures/echo.png" )
    , ( "bouncer/feint.svg", "/img/textures/feint.png" )
    , ( "shielder/sword.svg", "/img/textures/sword.png" )
    , ( "shielder/potion.svg", "/img/textures/potion.png" )
    , ( "shielder/reflect.svg", "/img/textures/reflect.png" )
    , ( "watcher/staff.svg", "/img/textures/staff.png" )
    , ( "watcher/surge.svg", "/img/textures/surge.png" )
    , ( "watcher/imitate.svg", "/img/textures/mimic.png" )
    , ( "watcher/prophecy.svg", "/img/textures/prophecy.png" )
    , ( "drinker/scythe.svg", "/img/textures/scythe.png" )
    , ( "drinker/bloodsucker.svg", "/img/textures/bloodsucker.png" )
    , ( "drinker/serpent.svg", "/img/textures/serpent.png" )
    , ( "drinker/reversal.svg", "/img/textures/reversal.png" )
    , ( "drinker/bad-apple.svg", "/img/textures/bad-apple.png" )
    , ( "collector/relicblade.svg", "/img/textures/relicblade.png" )
    , ( "collector/greed.svg", "/img/textures/greed.png" )
    , ( "collector/alchemy.svg", "/img/textures/alchemy.png" )
    , ( "collector/gold.svg", "/img/textures/gold.png" )
    , ( "breaker/hammer.svg", "/img/textures/hammer.png" )
    , ( "breaker/lightning.svg", "/img/textures/lightning.png" )
    , ( "breaker/hubris.svg", "/img/textures/hubris.png" )
    , ( "balancer/katana.svg", "/img/textures/katana.png" )
    , ( "balancer/curse.svg", "/img/textures/curse.png" )
    , ( "balancer/bless.svg", "/img/textures/bless.png" )
    , ( "balancer/balance.svg", "/img/textures/balance.png" )
    , ( "the_end.svg", "/img/textures/the-end.png" )
    ]


null : Model -> WebGL.Texture.Texture
null m =
    case load m "null" of
        Just t ->
            t

        Nothing ->
            Debug.crash "Failed to load null texture"
