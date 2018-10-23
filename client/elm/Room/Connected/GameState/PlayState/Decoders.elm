module PlayState.Decoders exposing (decoder, endedDecoder, playingDecoder)

import Game.State exposing (gameInit)
import Json.Decode as Json exposing (Decoder, field, maybe)
import Model.Decoders as Model
import Model.Types exposing (Model)
import PlayState.Types exposing (PlayState(..))
import WhichPlayer.Decoders as WhichPlayer
import WhichPlayer.Types exposing (WhichPlayer)


decoder : Decoder PlayState
decoder =
    Json.oneOf
        [ playingDecoder
        , endedDecoder
        ]


playingDecoder : Decoder PlayState
playingDecoder =
    let
        playingInit : Model -> PlayState
        playingInit model =
            Playing { game = gameInit model }
    in
    Json.map playingInit <|
        field "playing" <|
            Model.decoder


endedDecoder : Decoder PlayState
endedDecoder =
    let
        endedInit : Maybe WhichPlayer -> Model -> PlayState
        endedInit winner model =
            Ended
                { winner = winner
                , game = gameInit model
                , replayId = Nothing
                }
    in
    Json.map2 endedInit
        (field "winner" <| maybe WhichPlayer.decoder)
        (field "final" <| Model.decoder)
