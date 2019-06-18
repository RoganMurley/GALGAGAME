module Game.Decoders exposing (decodeHoverOther)

import Game.Types exposing (Hover(..), HoverOther)
import Json.Decode as Json exposing (Decoder, field, int, null, oneOf)


decodeHoverOther : String -> Result Json.Error HoverOther
decodeHoverOther msg =
    let
        hoverDecoder : Decoder HoverOther
        hoverDecoder =
            oneOf
                [ Json.map
                    (\index -> HoverHand { index = index, tick = 0 })
                  <|
                    field "hand" int
                , Json.map
                    (\index -> HoverStack { index = index, tick = 0 })
                  <|
                    field "stack" int
                , null NoHover
                ]
    in
    Json.decodeString hoverDecoder msg
