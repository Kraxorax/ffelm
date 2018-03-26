module Dugme exposing (..)

import Color exposing (Color)
import Html exposing (..)
import Html.Attributes exposing (style)
import Color.Convert exposing (colorToHex)
import Html.Events exposing (onMouseEnter, onMouseLeave, onClick)


type alias Dugme =
    { prva : Color
    , druga : Color
    , text : String
    , state : BojaDugmeta
    }


type BojaDugmeta
    = Prva
    | Druga


init : Color -> Color -> String -> Dugme
init prva druga text =
    Dugme prva druga text Prva


type Msg
    = Klick
    | PromeniDugme


update : Msg -> Dugme -> Dugme
update msg dugme =
    case msg of
        PromeniDugme ->
            { dugme | state = toggleBoja dugme.state }

        Klick ->
            dugme


toggleBoja : BojaDugmeta -> BojaDugmeta
toggleBoja b =
    case b of
        Prva ->
            Druga

        Druga ->
            Prva


view : Dugme -> Html Msg
view dugme =
    button
        [ style [ ( "background-color", (stringboje dugme) ) ]
        , onMouseEnter PromeniDugme
        , onMouseLeave PromeniDugme
        , onClick Klick
        ]
        [ text dugme.text ]


bojaDugmeta : Dugme -> Color
bojaDugmeta d =
    case d.state of
        Prva ->
            d.prva

        Druga ->
            d.druga


stringboje : Dugme -> String
stringboje =
    bojaDugmeta >> colorToHex
