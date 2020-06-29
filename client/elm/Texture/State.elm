module Texture.State exposing (defaultOptions, fetchTextures, init, load, save, texturePaths, update, with, with2, with3, with4, with5)

import Dict
import Font.State exposing (fontPaths)
import Ports exposing (log)
import Task
import Texture.Messages exposing (Msg(..))
import Texture.Types exposing (Model)
import WebGL.Texture exposing (Error(..), Texture)


init : Model
init =
    { textures = Dict.empty }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg m =
    case msg of
        TexturesLoaded textures ->
            ( save m textures, Cmd.none )

        TexturesError error ->
            let
                errorStr : String
                errorStr =
                    case error of
                        LoadError ->
                            "Load error"

                        SizeError w h ->
                            "Size error " ++ String.fromInt w ++ ", " ++ String.fromInt h
            in
            ( m, log <| "Error loading textures: " ++ errorStr )


save : Model -> ( String, Texture ) -> Model
save model ( name, tex ) =
    { model | textures = Dict.insert name tex model.textures }


load : Model -> String -> Maybe Texture
load { textures } name =
    Dict.get name textures


defaultOptions : WebGL.Texture.Options
defaultOptions =
    WebGL.Texture.defaultOptions


fetchTextures : List (Cmd Msg)
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

        handler : Result WebGL.Texture.Error ( String, Texture ) -> Msg
        handler result =
            case result of
                Err error ->
                    TexturesError error

                Ok textures ->
                    TexturesLoaded textures

        fontTexturePaths : List ( String, String )
        fontTexturePaths =
            List.map (\{ name, texturePath } -> ( name, texturePath )) fontPaths
    in
    List.map
        (loader >> Task.attempt handler)
        (fontTexturePaths ++ texturePaths)


texturePaths : List ( String, String )
texturePaths =
    [ -- Testing
      ( "radial.png", "/img/textures/radial.png" )
    , ( "yourTurn.png", "/img/textures/yourTurn.png" )
    , ( "theirTurn.png", "/img/textures/theirTurn.png" )
    , ( "pass.png", "/img/textures/pass.png" )
    , ( "cardBack.png", "/img/textures/cardBackNegative.png" )
    , ( "cardBackRed.png", "/img/textures/cardBackRed.png" )
    , ( "cardBackBack.png", "/img/textures/cardBackBack.png" )
    , ( "cardOrb.png", "/img/textures/cardOrb.png" )
    , ( "cardOrbOther.png", "/img/textures/cardOrbOther.png" )
    , ( "stain.png", "/img/textures/stain.png" )
    , ( "tea.png", "/img/textures/tea.png" )

    -- Ring
    , ( "ring.png", "/img/textures/ring.png" )
    , ( "lifeclaw.png", "/img/textures/lifeclaw.png" )
    , ( "cursor.png", "/img/textures/cursor.png" )

    -- VFX
    , ( "noise.png", "/img/textures/noise.png" )

    -- Cards
    , ( "fireball.png", "/img/textures/fireball.png" )
    , ( "offering.png", "/img/textures/offering.png" )
    , ( "confound.png", "/img/textures/confound.png" )
    , ( "overwhelm.png", "/img/textures/envy.png" )
    , ( "echo.png", "/img/textures/echo.png" )
    , ( "feint.png", "/img/textures/feint.png" )
    , ( "potion.png", "/img/textures/potion.png" )
    , ( "reflect.png", "/img/textures/reflect.png" )
    , ( "staff.png", "/img/textures/staff.png" )
    , ( "surge.png", "/img/textures/brainbomb.png" )
    , ( "prophecy.png", "/img/textures/prophecy.png" )
    , ( "scythe.png", "/img/textures/harvest.png" )
    , ( "bloodsucker.png", "/img/textures/feast.png" )
    , ( "serpent.png", "/img/textures/beguile.png" )
    , ( "reverse.png", "/img/textures/reverse.png" )
    , ( "bad-apple.png", "/img/textures/badapple.png" )
    , ( "greed.png", "/img/textures/greed.png" )
    , ( "alchemy.png", "/img/textures/alchemy.png" )
    , ( "hammer.png", "/img/textures/strike.png" )
    , ( "lightning.png", "/img/textures/lightning.png" )
    , ( "hubris.png", "/img/textures/hubris.png" )
    , ( "katana.png", "/img/textures/katana.png" )
    , ( "curse.png", "/img/textures/curse.png" )
    , ( "bless.png", "/img/textures/bless.png" )
    , ( "balance.png", "/img/textures/balance.png" )
    , ( "end.png", "/img/textures/end.png" )
    , ( "sting.png", "/img/textures/sting.png" )
    , ( "gold.png", "/img/textures/gold.png" )
    , ( "waxworks.png", "/img/textures/mimic.png" )
    , ( "missile.png", "/img/textures/missile.png" )
    , ( "grudge.png", "/img/textures/grudge.png" )

    -- Dailies
    , ( "subjugate.png", "/img/textures/subjugate.png" )
    , ( "avarice.png", "/img/textures/avarice.png" )
    , ( "goldrush.png", "/img/textures/goldrush.png" )
    , ( "telepathy.png", "/img/textures/telepathy.png" )
    , ( "ritual.png", "/img/textures/ritual.png" )
    , ( "unravel.png", "/img/textures/unravel.png" )
    , ( "respite.png", "/img/textures/respite.png" )
    , ( "voidbeam.png", "/img/textures/voidbeam.png" )
    , ( "feud.png", "/img/textures/feud.png" )
    , ( "inevitable.png", "/img/textures/inevitable.png" )
    ]


with : Model -> String -> (Texture -> List a) -> List a
with model path func =
    case load model path of
        Just texture ->
            func texture

        Nothing ->
            []


with2 : Model -> String -> String -> (Texture -> Texture -> List a) -> List a
with2 model pathA pathB func =
    case ( load model pathA, load model pathB ) of
        ( Just textureA, Just textureB ) ->
            func textureA textureB

        _ ->
            []


with3 : Model -> String -> String -> String -> (Texture -> Texture -> Texture -> List a) -> List a
with3 model pathA pathB pathC func =
    case ( load model pathA, load model pathB, load model pathC ) of
        ( Just textureA, Just textureB, Just textureC ) ->
            func textureA textureB textureC

        _ ->
            []


with4 : Model -> String -> String -> String -> String -> (Texture -> Texture -> Texture -> Texture -> List a) -> List a
with4 model pathA pathB pathC pathD func =
    case ( load model pathA, load model pathB ) of
        ( Just textureA, Just textureB ) ->
            case ( load model pathC, load model pathD ) of
                ( Just textureC, Just textureD ) ->
                    func textureA textureB textureC textureD

                _ ->
                    []

        _ ->
            []


with5 : Model -> String -> String -> String -> String -> String -> (Texture -> Texture -> Texture -> Texture -> Texture -> List a) -> List a
with5 model pathA pathB pathC pathD pathE func =
    case ( load model pathA, load model pathB ) of
        ( Just textureA, Just textureB ) ->
            case ( load model pathC, load model pathD ) of
                ( Just textureC, Just textureD ) ->
                    case load model pathE of
                        Just textureE ->
                            func textureA textureB textureC textureD textureE

                        Nothing ->
                            []

                _ ->
                    []

        _ ->
            []
