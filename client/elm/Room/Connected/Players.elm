module Players exposing (Player, Players, decode, shouldRematch)

import Json.Decode as Json exposing (Decoder, field, index, int, maybe, string)


type alias Players =
    { pa : Maybe Player
    , pb : Maybe Player
    }


type alias Player =
    { name : String
    , xp : Int
    }


shouldRematch : Players -> Bool
shouldRematch { pa, pb } =
    -- Need to switch this to a proper datatype, not a name match.
    case ( pa, pb ) of
        ( Just { name }, _ ) ->
            name == "CPU"

        ( _, Just { name } ) ->
            name == "CPU"

        _ ->
            True


decode : String -> Result Json.Error Players
decode msg =
    Json.decodeString decoder msg


decoder : Decoder Players
decoder =
    Json.map2 Players
        (index 0 <| maybe playerDecoder)
        (index 1 <| maybe playerDecoder)


playerDecoder : Decoder Player
playerDecoder =
    Json.map2 Player
        (field "name" string)
        (field "xp" int)
