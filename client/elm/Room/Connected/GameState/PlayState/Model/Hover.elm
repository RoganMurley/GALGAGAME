module Hover exposing (Hover(..), HoverBase, HoverDamage(..), HoverOther, HoverSelf, HoverStack, damageDecoder, decodeHoverOther, encodeHoverSelf, getDmg)

import Json.Decode exposing (Decoder)
import Json.Encode
import Model.Types exposing (Life)


type alias HoverSelf =
    Hover { dmg : ( HoverDamage, HoverDamage ) }


type alias HoverOther =
    Hover {}


type alias HoverStack =
    Hover { dmg : ( HoverDamage, HoverDamage ) }


type alias HoverBase a =
    { a
        | index : Int
        , tick : Float
    }


type Hover a
    = HoverHand (HoverBase a)
    | HoverStack (HoverBase a)
    | NoHover


type HoverDamage
    = HoverDamage Life
    | HoverDamageUncertain


getDmg : HoverSelf -> ( HoverDamage, HoverDamage )
getDmg hover =
    case hover of
        HoverHand { dmg } ->
            dmg

        HoverStack { dmg } ->
            dmg

        NoHover ->
            ( HoverDamage 0, HoverDamage 0 )


damageDecoder : Decoder HoverDamage
damageDecoder =
    Json.Decode.oneOf
        [ Json.Decode.map HoverDamage Json.Decode.int
        , Json.Decode.string
            |> Json.Decode.andThen
                (\s ->
                    if s == "?" then
                        Json.Decode.succeed HoverDamageUncertain

                    else
                        Json.Decode.fail ("Unknown hover damage value: " ++ s)
                )
        ]


encodeHoverSelf : HoverSelf -> String
encodeHoverSelf hover =
    let
        value : Json.Encode.Value
        value =
            case hover of
                HoverHand { index } ->
                    Json.Encode.object [ ( "hand", Json.Encode.int index ) ]

                HoverStack { index } ->
                    Json.Encode.object [ ( "stack", Json.Encode.int index ) ]

                NoHover ->
                    Json.Encode.null
    in
    Json.Encode.encode 0 value


decodeHoverOther : String -> Result Json.Decode.Error HoverOther
decodeHoverOther msg =
    let
        hoverDecoder : Json.Decode.Decoder HoverOther
        hoverDecoder =
            Json.Decode.oneOf
                [ Json.Decode.map
                    (\index -> HoverHand { index = index, tick = 0 })
                  <|
                    Json.Decode.field "hand" Json.Decode.int
                , Json.Decode.map
                    (\index -> HoverStack { index = index, tick = 0 })
                  <|
                    Json.Decode.field "stack" Json.Decode.int
                , Json.Decode.null NoHover
                ]
    in
    Json.Decode.decodeString hoverDecoder msg
