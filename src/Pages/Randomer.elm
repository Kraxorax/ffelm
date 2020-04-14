module Pages.Randomer exposing (Msg(..), update, view, Model, init)

import Html exposing (Html)
import Element exposing(el, text, row)

type alias Model = 
  { inputString: String
  , result: String
  }

type Msg
  = ParseDefinition

init : Model
init = 
  { inputString = "", result = "kucaj nesto" }

update :  Msg -> Model -> Model
update msg model =
  case msg of
    ParseDefinition -> model

view : Model -> Html Msg
view _ =
  Element.layout []
    (row [] [el [] (text "Randomer stranica")])