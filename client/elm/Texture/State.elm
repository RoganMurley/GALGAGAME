module Texture.State exposing (defaultOptions, fetchTextures, init, load, null, save, saveList, texturePaths, update)

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
    , ( "striker/fireball.svg", "/img/textures/experiments/fireball.png" )
    , ( "striker/offering.svg", "/img/textures/experiments/offering.png" )
    , ( "striker/confound.svg", "/img/textures/experiments/confound.png" )
    , ( "bouncer/overwhelm.svg", "/img/textures/experiments/envy.png" )
    , ( "bouncer/echo.svg", "/img/textures/experiments/echo.png" )
    , ( "bouncer/feint.svg", "/img/textures/experiments/feint.png" )
    , ( "shielder/potion.svg", "/img/textures/experiments/potion.png" )
    , ( "shielder/reflect.svg", "/img/textures/experiments/reflect.png" )
    , ( "watcher/staff.svg", "/img/textures/experiments/staff.png" )
    , ( "watcher/surge.svg", "/img/textures/experiments/brainbomb.png" )
    , ( "watcher/prophecy.svg", "/img/textures/experiments/prophecy.png" )
    , ( "drinker/scythe.svg", "/img/textures/experiments/harvest.png" )
    , ( "drinker/bloodsucker.svg", "/img/textures/experiments/feast.png" )
    , ( "drinker/serpent.svg", "/img/textures/experiments/beguile.png" )
    , ( "drinker/reversal.svg", "/img/textures/experiments/reverse.png" )
    , ( "drinker/bad-apple.svg", "/img/textures/experiments/badapple.png" )
    , ( "collector/greed.svg", "/img/textures/experiments/greed.png" )
    , ( "collector/alchemy.svg", "/img/textures/experiments/alchemy.png" )
    , ( "breaker/hammer.svg", "/img/textures/experiments/strike.png" )
    , ( "breaker/lightning.svg", "/img/textures/experiments/lightning.png" )
    , ( "breaker/hubris.svg", "/img/textures/experiments/hubris.png" )
    , ( "balancer/katana.svg", "/img/textures/experiments/katana.png" )
    , ( "balancer/curse.svg", "/img/textures/experiments/curse.png" )
    , ( "balancer/bless.svg", "/img/textures/experiments/bless.png" )
    , ( "balancer/balance.svg", "/img/textures/experiments/balance.png" )
    , ( "the_end.svg", "/img/textures/experiments/end.png" )
    , ( "honeymaker/sting.svg", "/img/textures/experiments/sting.png" )
    , ( "honeymaker/gold.svg", "/img/textures/experiments/gold.png" )
    , ( "honeymaker/waxworks.svg", "/img/textures/experiments/mimic.png" )
    , ( "symbol.png", "/img/textures/experiments/symbol.png" )
    , ( "reflect-symbol.png", "/img/textures/experiments/reflect.png" )
    , ( "triforce.png", "/img/textures/experiments/triforce.png" )
    , ( "missile.png", "/img/textures/experiments/missile.png" )
    , ( "grudge.png", "/img/textures/experiments/grudge.png" )

    -- Charselect stuff
    , ( "textures/experiments/hubris.png", "/img/textures/experiments/hubris.png" )
    , ( "textures/experiments/confound.png", "/img/textures/experiments/confound.png" )
    , ( "textures/experiments/reverse.png", "/img/textures/experiments/reverse.png" )
    , ( "textures/experiments/prophecy.png", "/img/textures/experiments/prophecy.png" )
    , ( "textures/experiments/reflect.png", "/img/textures/experiments/reflect.png" )
    , ( "textures/experiments/balance.png", "/img/textures/experiments/balance.png" )
    , ( "textures/experiments/alchemy.png", "/img/textures/experiments/alchemy.png" )

    -- Ring
    , ( "ring.png", "/img/textures/experiments/ring.png" )
    ]


null : Model -> WebGL.Texture.Texture
null m =
    case load m "null" of
        Just t ->
            t

        Nothing ->
            Debug.crash "Failed to load null texture"
