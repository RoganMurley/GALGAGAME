module Hover exposing (Hover(..), HoverBase, HoverDamage(..), HoverOther, HoverSelf, HoverStack, damageDecoder, decodeHoverOther, encodeHoverSelf, getDmg, map)

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
    | HoverOtherHand (HoverBase a)
    | HoverStack (HoverBase a)
    | HoverAuto (HoverBase a)
    | NoHover


type HoverDamage
    = HoverDamage Life
    | HoverDamageUncertain


getDmg : HoverSelf -> ( HoverDamage, HoverDamage )
getDmg hover =
    case hover of
        HoverHand { dmg } ->
            dmg

        HoverOtherHand { dmg } ->
            dmg

        HoverStack { dmg } ->
            dmg

        HoverAuto { dmg } ->
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

                HoverOtherHand { index } ->
                    Json.Encode.object [ ( "otherHand", Json.Encode.int index ) ]

                HoverStack { index } ->
                    Json.Encode.object [ ( "stack", Json.Encode.int index ) ]

                HoverAuto { index } ->
                    Json.Encode.object [ ( "auto", Json.Encode.int index ) ]

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
                    Json.Decode.field "hand"
                        Json.Decode.int
                , Json.Decode.map
                    (\index -> HoverOtherHand { index = index, tick = 0 })
                  <|
                    Json.Decode.field "otherHand" Json.Decode.int
                , Json.Decode.map
                    (\index -> HoverStack { index = index, tick = 0 })
                  <|
                    Json.Decode.field "stack" Json.Decode.int
                , Json.Decode.null NoHover
                ]
    in
    Json.Decode.decodeString hoverDecoder msg


map : (HoverBase a -> HoverBase b) -> Hover a -> Hover b
map f hover =
    case hover of
        HoverHand h ->
            HoverHand <| f h

        HoverOtherHand h ->
            HoverOtherHand <| f h

        HoverStack h ->
            HoverStack <| f h

        HoverAuto h ->
            HoverAuto <| f h

        NoHover ->
            NoHover
