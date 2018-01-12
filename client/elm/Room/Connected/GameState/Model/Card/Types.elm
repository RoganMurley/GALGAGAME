module Card.Types exposing (Anim(..), Card)


type alias Card =
    { name : String
    , desc : String
    , imgURL : String
    , sfxURL : String
    }


type Anim
    = Slash
    | Heal
    | Obliterate
    | Custom String
