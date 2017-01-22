module Messages exposing (GameMsg(..), MenuMsg(..), Msg(..), CharSelectMsg(..))

import Mouse exposing (Position)
import Time exposing (Time)


type Msg
    = Input String
    | Send String
    | Receive String
    | DragStart Position
    | DragAt Position
    | DragEnd Position
    | DrawCard
    | EndTurn
    | PlayCard String
    | NewChatMsg String
    | GameStateMsg GameMsg
    | ConnectError String
    | Spectate
    | Play
    | KeyPress Int
    | Rematch
    | Tick Time
    | ResolveStep
    | HoverCard (Maybe String)
    | MainMenuMsg MenuMsg
    | SelectAllInput String
    | CopyInput String
    | SelectCharacter String


type GameMsg
    = Sync String
    | HoverOutcome (Maybe Int)
    | ResolveOutcome String
    | SelectingMsg CharSelectMsg


type MenuMsg
    = MenuCustom


type CharSelectMsg
    = SelectingHover String



-- | SelectingSelect String
-- | SelectingDeselect String
