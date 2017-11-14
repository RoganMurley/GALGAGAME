module Card.Types exposing (Anim(..), Card)


type alias Card =
    { name : String
    , desc : String
    , imgURL : String
    , sfxURL : String
    , anim : Maybe Anim
    }


type Anim
    = Slash
    | Heal
    | Obliterate
    | Custom String
