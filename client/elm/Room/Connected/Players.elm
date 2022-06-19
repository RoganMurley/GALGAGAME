module Players exposing (Player, Players, decode, shouldRematch)

import Json.Decode as Json exposing (Decoder, field, index, int, list, maybe, string)
import Set exposing (Set)


type alias Players =
    { pa : Maybe Player
    , pb : Maybe Player
    }


type alias Player =
    { name : String
    , xp : Int
    , unlocks : Set String
    }


shouldRematch : Players -> Bool
shouldRematch { pb } =
    -- Need to switch this to a proper datatype, not a name match.
    case pb of
        Just { name } ->
            name /= "CPU"

        _ ->
            False


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
    let
        makePlayer : String -> Int -> List String -> Player
        makePlayer name xp unlocks =
            Player name xp (Set.fromList unlocks)
    in
    Json.map3 makePlayer
        (field "name" string)
        (field "xp" int)
        (field "unlocks" <| list string)
