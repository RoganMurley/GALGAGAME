module Form exposing (Error(..), validate)

import Maybe.Extra as Maybe


type Error
    = Error String


validate : List (model -> Maybe ( field, Error )) -> model -> Maybe ( field, Error )
validate validators model =
    List.foldl Maybe.or Nothing <| List.map ((|>) model) validators
