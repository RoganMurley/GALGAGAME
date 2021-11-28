module Chat.Decoders exposing (chatDragEventDecoder)

import Chat.Messages exposing (Msg(..))
import Json.Decode as Json exposing (Decoder)
import Mouse


type alias Dimensions =
    { width : Float
    , height : Float
    }


chatDragEventDecoder : Decoder Msg
chatDragEventDecoder =
    let
        makeEvent : Mouse.Position -> Dimensions -> Dimensions -> Msg
        makeEvent pos target offset =
            if
                (offset.width > target.width * 0.9)
                    && (offset.height > target.height * 0.9)
            then
                NoOp

            else
                DragStart pos
    in
    Json.map3 makeEvent Mouse.decoder targetDimensionsDecoder offsetDecoder


targetDimensionsDecoder : Decoder Dimensions
targetDimensionsDecoder =
    Json.field "target"
        (Json.map2 Dimensions
            (Json.field "clientWidth" Json.float)
            (Json.field "clientHeight" Json.float)
        )


offsetDecoder : Decoder Dimensions
offsetDecoder =
    Json.map2 Dimensions
        (Json.field "offsetX" Json.float)
        (Json.field "offsetY" Json.float)
