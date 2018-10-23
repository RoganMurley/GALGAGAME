module Room.Generators exposing (generate, roomID, usernameNumber)

import Main.Types exposing (Seed)
import Random
import Random.Char exposing (char)
import Random.String exposing (string)
import Tuple exposing (first)


roomID : Random.Generator String
roomID =
    string 8 Random.Char.english


usernameNumber : Random.Generator String
usernameNumber =
    string 3 <| char 48 57


generate : Random.Generator a -> Seed -> a
generate generator seed =
    first <| Random.step generator <| Random.initialSeed seed
