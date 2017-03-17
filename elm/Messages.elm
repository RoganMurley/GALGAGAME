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
    | PlayCard Int
    | NewChatMsg String
    | GameStateMsg GameMsg
    | ConnectError String
    | Spectate
    | Play
    | KeyPress Int
    | Rematch
    | Tick Time
    | Frame Float
    | Resize Int Int
    | ResolveStep
    | HoverCard (Maybe Int)
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
    | MenuComputer


type CharSelectMsg
    = SelectingHover String



-- | SelectingSelect String
-- | SelectingDeselect String
