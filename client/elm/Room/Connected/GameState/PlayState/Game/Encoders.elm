module Game.Encoders exposing (encodeHoverSelf)

import Game.Types exposing (Hover(..), HoverSelf)
import Json.Encode as Json exposing (encode, int, null, object)


encodeHoverSelf : HoverSelf -> String
encodeHoverSelf hover =
    let
        value : Json.Value
        value =
            case hover of
                HoverHand { index } ->
                    object [ ( "hand", int index ) ]

                HoverStack { index } ->
                    object [ ( "stack", int index ) ]

                NoHover ->
                    null
    in
    encode 0 value
