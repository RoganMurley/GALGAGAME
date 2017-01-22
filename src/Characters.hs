module Characters where

import Data.Aeson (ToJSON(..), (.=), object)
import Data.Text (Text)
import Cards
import Model


-- TYPES

type CharacterCards = (Card, Card, Card, Card)

data SelectedCharacters
    = NoneSelected
    | OneSelected   Character
    | TwoSelected   Character Character
    | ThreeSelected Character Character Character
    deriving (Eq, Show)

instance ToJSON SelectedCharacters where
  toJSON s = toJSON . toList $ s


data Character = Character
  { character_name  :: Text
  , character_cards :: CharacterCards
  } deriving (Eq, Show)

instance ToJSON Character where
  toJSON (Character name cards) =
      object [
        "name"  .= name
      , "cards" .= cards
      ]


data CharModel =
  CharModel {
    charmodel_pa         :: SelectedCharacters
  , charmodel_pb         :: SelectedCharacters
  , charmodel_characters :: [Character]
  } deriving (Eq, Show)

instance ToJSON CharModel where
  toJSON (CharModel selected _ characters) =
    object [
      "selecting" .= characters
    , "selected"  .= selected
    ]


characterModelReverso :: CharModel -> CharModel
characterModelReverso (CharModel pa pb cs) =
  CharModel pb pa cs


initCharModel :: CharModel
initCharModel = CharModel NoneSelected NoneSelected allCharacters


allCharacters :: [Character]
allCharacters = [
    protector
  , apollo
  , nemesis
  , drinker
  , oracle
  ]


selectChar :: CharModel -> WhichPlayer -> Text -> CharModel
selectChar model@(CharModel { charmodel_pa = m }) PlayerA name =
  model { charmodel_pa = selectIndChar name m }
selectChar model@(CharModel { charmodel_pb = m }) PlayerB name =
  model { charmodel_pb = selectIndChar name m }


selectIndChar :: Text -> SelectedCharacters -> SelectedCharacters
selectIndChar name selected =
  case selected of
    NoneSelected ->
      OneSelected char
    OneSelected a ->
      TwoSelected a char
    TwoSelected a b ->
      ThreeSelected a b char
    ThreeSelected a b c  ->
      ThreeSelected a b c
  where
    char :: Character
    char = head . (filter (\(Character n _) -> n == name )) $ allCharacters



-- textToCharacters :: (Maybe Text, Maybe Text, Maybe Text) -> SelectedCharacters
-- textToCharacters x =
--   case x of
--     (Nothing, Nothing, Nothing) ->
--       NoneSelected
--     (Just a,  Nothing, Nothing) ->
--       OneSelected (f a)
--     (Just a,  Just b,  Nothing) ->
--       TwoSelected (f a) (f b)
--     (Just a,  Just b,  Just c)  ->
--       ThreeSelected (f a) (f b) (f c)
--     _ ->
--       NoneSelected
--   where
--     f :: Text -> Character
--     f t = head . (filter (\(Character n _) -> n == t )) $ allCharacters


toList :: SelectedCharacters -> [Character]
toList NoneSelected          = []
toList (OneSelected a)       = [ a ]
toList (TwoSelected a b)     = [ a, b ]
toList (ThreeSelected a b c) = [ a, b, c ]


-- CHARACTERS

cardPlaceholder :: Card
cardPlaceholder = Card "Placeholder" "For testing!" "goat.svg" (\_ _ m -> m)

protector :: Character
protector = Character "Protector" (cardBoomerang, cardAgility, cardPotion, cardReflect)

apollo :: Character
apollo = Character "Apollo" (cardDagger, cardFireball, cardOffering, cardConfound)

nemesis :: Character
nemesis = Character "Nemesis" (cardHammer, cardPlaceholder, cardSickness, cardHubris)

drinker :: Character
drinker = Character "Drinker" (cardVampire, cardSuccubus, cardSiren, cardReversal)

oracle :: Character
oracle = Character "Oracle" (cardObscurer, cardPlaceholder, cardZen, cardProphecy)
