module Drag.State exposing (dragAt, dragEnd, dragStart, getPosition, update)

import Drag.Messages exposing (Msg(..))
import Drag.Types exposing (Drag, Draggable, Position)


getPosition : Draggable a -> Position
getPosition { pos, drag } =
    case drag of
        Nothing ->
            pos

        Just { start, current } ->
            { x = pos.x + current.x - start.x
            , y = pos.y + current.y - start.y
            }


dragStart : Position -> Draggable a -> Draggable a
dragStart pos model =
    { model | drag = Just (Drag pos pos) }


dragAt : Position -> Draggable a -> Draggable a
dragAt pos model =
    { model | drag = Maybe.map (\{ start } -> Drag start pos) model.drag }


dragEnd : Draggable a -> Draggable a
dragEnd model =
    { model | pos = getPosition model, drag = Nothing }


update : Msg -> Draggable a -> Draggable a
update msg =
    case msg of
        Start pos ->
            dragStart pos

        At pos ->
            dragAt pos

        End _ ->
            dragEnd
