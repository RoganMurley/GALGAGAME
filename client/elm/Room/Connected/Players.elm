module Players exposing (Players, decode, shouldRematch)

import Json.Decode as Json exposing (Decoder, index, maybe, string)


type alias Players =
    { pa : Maybe String
    , pb : Maybe String
    }


shouldRematch : Players -> Bool
shouldRematch { pa, pb } =
    -- Need to switch this to a proper datatype, not a name match.
    case ( pa, pb ) of
        ( Just "CPU", _ ) ->
            False

        ( _, Just "CPU" ) ->
            False

        ( Just _, Just _ ) ->
            True

        _ ->
            False


decode : String -> Result Json.Error Players
decode msg =
    Json.decodeString decoder msg


decoder : Decoder Players
decoder =
    Json.map2 Players
        (index 0 <| maybe string)
        (index 1 <| maybe string)
