module Clock.Stack exposing (..)

import Clock.Card exposing (CardEntity, cardEntity)
import Clock.Types exposing (ClockParams, GameEntity)
import Texture.Types as Texture
import WebGL


view : ClockParams -> List CardEntity -> Texture.Model -> List WebGL.Entity
view params entities textures =
    List.concat <| List.map (cardEntity params textures) entities
