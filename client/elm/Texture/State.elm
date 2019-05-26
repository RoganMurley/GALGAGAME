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
    [ -- Ring
      ( "ring.png", "/img/textures/ring.png" )
    , ( "lifeclaw.png", "/img/textures/lifeclaw.png" )
    , ( "cursor.png", "/img/textures/cursor.png" )

    -- Cards
    , ( "noise", "/img/textures/noise.png" )
    , ( "striker/fireball.png", "/img/textures/fireball.png" )
    , ( "striker/offering.png", "/img/textures/offering.png" )
    , ( "striker/confound.png", "/img/textures/confound.png" )
    , ( "bouncer/overwhelm.png", "/img/textures/envy.png" )
    , ( "bouncer/echo.png", "/img/textures/echo.png" )
    , ( "bouncer/feint.png", "/img/textures/feint.png" )
    , ( "shielder/potion.png", "/img/textures/potion.png" )
    , ( "shielder/reflect.png", "/img/textures/reflect.png" )
    , ( "watcher/staff.png", "/img/textures/staff.png" )
    , ( "watcher/surge.png", "/img/textures/brainbomb.png" )
    , ( "watcher/prophecy.png", "/img/textures/prophecy.png" )
    , ( "drinker/scythe.png", "/img/textures/harvest.png" )
    , ( "drinker/bloodsucker.png", "/img/textures/feast.png" )
    , ( "drinker/serpent.png", "/img/textures/beguile.png" )
    , ( "drinker/reversal.png", "/img/textures/reverse.png" )
    , ( "drinker/bad-apple.png", "/img/textures/badapple.png" )
    , ( "collector/greed.png", "/img/textures/greed.png" )
    , ( "collector/alchemy.png", "/img/textures/alchemy.png" )
    , ( "breaker/hammer.png", "/img/textures/strike.png" )
    , ( "breaker/lightning.png", "/img/textures/lightning.png" )
    , ( "breaker/hubris.png", "/img/textures/hubris.png" )
    , ( "balancer/katana.png", "/img/textures/katana.png" )
    , ( "balancer/curse.png", "/img/textures/curse.png" )
    , ( "balancer/bless.png", "/img/textures/bless.png" )
    , ( "balancer/balance.png", "/img/textures/balance.png" )
    , ( "the_end.png", "/img/textures/end.png" )
    , ( "honeymaker/sting.png", "/img/textures/sting.png" )
    , ( "honeymaker/gold.png", "/img/textures/gold.png" )
    , ( "honeymaker/waxworks.png", "/img/textures/mimic.png" )
    , ( "symbol.png", "/img/textures/symbol.png" )
    , ( "reflect-symbol.png", "/img/textures/reflect.png" )
    , ( "triforce.png", "/img/textures/triforce.png" )
    , ( "missile.png", "/img/textures/missile.png" )
    , ( "grudge.png", "/img/textures/grudge.png" )
    ]


null : Model -> WebGL.Texture.Texture
null m =
    case load m "null" of
        Just t ->
            t

        Nothing ->
            Debug.crash "Failed to load null texture"
