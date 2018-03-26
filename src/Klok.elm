module Klok exposing (..)

import Html exposing (..)
import Html.Events exposing (on, onInput, onClick)

import Util exposing (..)



type alias Klok = 
  { id: String
  , i: Int
  }


type Msg
  = Dole String
  | Gore String


init: Int -> Int -> Klok
init i index = Klok (makeId "klok" index) i


update: Msg -> Klok -> Klok
update msg m =
  case msg of
    Dole s -> { m | i = m.i - 1}
    Gore s -> { m | i = m.i + 1}


view: Klok -> Html Msg
view m =
  div []
    [ button [ onClick ( Dole m.id ) ] [ text "Dole" ]
    , text (toString m.i)
    , button [ onClick ( Gore m.id ) ] [ text "Gore" ]
    , text (" - " ++ m.id)
    ]
