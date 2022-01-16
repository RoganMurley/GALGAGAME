module League.Decoders exposing (leagueErrorDecoder)

import Json.Decode as Json exposing (Decoder, field, string)
import League.Types exposing (LeagueError)


leagueErrorDecoder : Decoder LeagueError
leagueErrorDecoder =
    Json.map LeagueError (field "error" string)
