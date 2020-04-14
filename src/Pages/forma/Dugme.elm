module Pages.Forma.Dugme exposing (view, update, init, Dugme, Msg(..))

import Color exposing (Color, toCssString)
import Html
import Html.Attributes exposing (style)
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


view : Dugme -> Html.Html Msg
view dugme =
    Html.button
        [ style  "background-color" (stringboje dugme)
        , onMouseEnter PromeniDugme
        , onMouseLeave PromeniDugme
        , onClick Klick
        ]
        [ Html.text dugme.text ]


bojaDugmeta : Dugme -> Color
bojaDugmeta d =
    case d.state of
        Prva ->
            d.prva

        Druga ->
            d.druga


stringboje : Dugme -> String
stringboje =
    bojaDugmeta >> toCssString >> \_-> "a"
