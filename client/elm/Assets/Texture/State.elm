module Texture.State exposing (defaultOptions, fetch, init, load, save, texturePaths, update, with, with2, with3, with4, with5)

import Assets.Fetch as Assets
import Assets.Types as Assets
import Dict
import Font.State exposing (fontPaths)
import Main.Messages as Main
import Ports exposing (log)
import Texture.Messages exposing (Msg(..))
import Texture.Types exposing (Model)
import WebGL.Texture exposing (Error(..), Texture)


init : Model
init =
    { textures = Dict.empty }


update : Msg -> Model -> ( Model, Cmd Main.Msg )
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


fetch : List (Cmd Msg)
fetch =
    let
        loader : Assets.Loader Texture WebGL.Texture.Error
        loader { path } =
            WebGL.Texture.loadWith
                { defaultOptions
                    | magnify = WebGL.Texture.nearest
                    , minify = WebGL.Texture.nearest
                }
                path

        handler : Assets.Handler Texture WebGL.Texture.Error Msg
        handler result =
            case result of
                Err error ->
                    TexturesError error

                Ok textures ->
                    TexturesLoaded textures

        paths : List Assets.Path
        paths =
            texturePaths
                ++ List.map
                    (\{ name, texturePath } -> { name = name, path = texturePath })
                    fontPaths
    in
    Assets.fetch loader handler paths


texturePaths : List Assets.Path
texturePaths =
    [ -- Testing
      { name = "radial.png", path = "/img/textures/radial.png" }
    , { name = "cardBack.png", path = "/img/textures/cardBackNegative.png" }
    , { name = "cardBackRed.png", path = "/img/textures/cardBackRed.png" }
    , { name = "cardBackBack.png", path = "/img/textures/cardBackBack.png" }
    , { name = "cardOrb.png", path = "/img/textures/cardOrb.png" }
    , { name = "cardOrbOther.png", path = "/img/textures/cardOrbOther.png" }

    -- VFX
    , { name = "noise.png", path = "/img/textures/noise.png" }

    -- Cards
    , { name = "fireball.png", path = "/img/textures/fireball.png" }
    , { name = "offering.png", path = "/img/textures/offering.png" }
    , { name = "confound.png", path = "/img/textures/confound.png" }
    , { name = "overwhelm.png", path = "/img/textures/envy.png" }
    , { name = "echo.png", path = "/img/textures/echo.png" }
    , { name = "feint.png", path = "/img/textures/feint.png" }
    , { name = "potion.png", path = "/img/textures/potion.png" }
    , { name = "reflect.png", path = "/img/textures/reflect.png" }
    , { name = "staff.png", path = "/img/textures/staff.png" }
    , { name = "surge.png", path = "/img/textures/brainbomb.png" }
    , { name = "prophecy.png", path = "/img/textures/prophecy.png" }
    , { name = "scythe.png", path = "/img/textures/harvest.png" }
    , { name = "bloodsucker.png", path = "/img/textures/feast.png" }
    , { name = "serpent.png", path = "/img/textures/beguile.png" }
    , { name = "reverse.png", path = "/img/textures/reverse.png" }
    , { name = "bad-apple.png", path = "/img/textures/badapple.png" }
    , { name = "greed.png", path = "/img/textures/greed.png" }
    , { name = "alchemy.png", path = "/img/textures/alchemy.png" }
    , { name = "hammer.png", path = "/img/textures/strike.png" }
    , { name = "lightning.png", path = "/img/textures/lightning.png" }
    , { name = "hubris.png", path = "/img/textures/hubris.png" }
    , { name = "katana.png", path = "/img/textures/katana.png" }
    , { name = "curse.png", path = "/img/textures/curse.png" }
    , { name = "bless.png", path = "/img/textures/bless.png" }
    , { name = "balance.png", path = "/img/textures/balance.png" }
    , { name = "end.png", path = "/img/textures/end.png" }
    , { name = "sting.png", path = "/img/textures/sting.png" }
    , { name = "gold.png", path = "/img/textures/gold.png" }
    , { name = "waxworks.png", path = "/img/textures/mimic.png" }
    , { name = "missile.png", path = "/img/textures/missile.png" }
    , { name = "grudge.png", path = "/img/textures/grudge.png" }

    -- Dailies
    , { name = "subjugate.png", path = "/img/textures/subjugate.png" }
    , { name = "avarice.png", path = "/img/textures/avarice.png" }
    , { name = "goldrush.png", path = "/img/textures/goldrush.png" }
    , { name = "telepathy.png", path = "/img/textures/telepathy.png" }
    , { name = "ritual.png", path = "/img/textures/ritual.png" }
    , { name = "unravel.png", path = "/img/textures/unravel.png" }
    , { name = "respite.png", path = "/img/textures/respite.png" }
    , { name = "voidbeam.png", path = "/img/textures/voidbeam.png" }
    , { name = "feud.png", path = "/img/textures/feud.png" }
    , { name = "inevitable.png", path = "/img/textures/inevitable.png" }
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
