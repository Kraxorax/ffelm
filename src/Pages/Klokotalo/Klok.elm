module Pages.Klokotalo.Klok exposing (Klok, Msg(..), init, update, view)

import Html
import Html.Events exposing (onClick)
import Util exposing (makeId)


type alias Klok =
    { id : String
    , i : Int
    }


type Msg
    = Dole String
    | Gore String


init : Int -> Int -> Klok
init i index =
    Klok (makeId "klok" index) i


update : Msg -> Klok -> Klok
update msg m =
    case msg of
        Dole _ ->
            { m | i = m.i - 1 }

        Gore _ ->
            { m | i = m.i + 1 }


view : Klok -> Html.Html Msg
view m =
    Html.div []
        [ Html.button [ onClick (Dole m.id) ] [ Html.text "-" ]
        , Html.text (String.fromInt m.i)
        , Html.button [ onClick (Gore m.id) ] [ Html.text "+" ]
        , Html.text (" - " ++ m.id)
        ]
