module Chat.Decoders exposing (chatDragEventDecoder)

import Chat.Messages exposing (Msg(..))
import Drag exposing (Drag(..))
import Json.Decode as Json exposing (Decoder)


chatDragEventDecoder : Decoder Msg
chatDragEventDecoder =
    let
        toMsg : Drag -> Msg
        toMsg drag =
            case drag of
                Drag pos ->
                    DragStart pos

                NoDrag ->
                    NoOp
    in
    Json.map toMsg Drag.decoder
