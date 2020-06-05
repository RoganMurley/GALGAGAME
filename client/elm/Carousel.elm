module Carousel exposing (Carousel, backward, forward, init)


type alias Carousel a =
    { previous : List a
    , selected : a
    , remaining : List a
    }


init : a -> List a -> Carousel a
init selected remaining =
    { previous = []
    , selected = selected
    , remaining = remaining
    }


forward : Carousel a -> Carousel a
forward { previous, selected, remaining } =
    case remaining of
        next :: leftovers ->
            { previous = selected :: previous
            , selected = next
            , remaining = leftovers
            }

        _ ->
            forward
                { previous = []
                , selected = selected
                , remaining = List.reverse previous
                }


backward : Carousel a -> Carousel a
backward { previous, selected, remaining } =
    case previous of
        prev :: leftovers ->
            { previous = leftovers
            , selected = prev
            , remaining = selected :: remaining
            }

        _ ->
            backward
                { previous = List.reverse remaining
                , selected = selected
                , remaining = []
                }
