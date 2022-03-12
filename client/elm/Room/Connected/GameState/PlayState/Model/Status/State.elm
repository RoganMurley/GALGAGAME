module Status.State exposing (collapse)

import Status.Types exposing (Status(..))


collapse : List Status -> List Status
collapse statuses =
    let
        f : Status -> List Status -> List Status
        f status acc =
            case acc of
                head :: _ ->
                    case ( head, status ) of
                        ( StatusBlighted, StatusBlighted ) ->
                            acc

                        _ ->
                            status :: acc

                _ ->
                    [ status ]
    in
    List.foldl f [] statuses
