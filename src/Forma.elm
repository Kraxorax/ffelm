module Forma exposing (..)

import Dugme
import Color exposing (Color, toCssString)
import ColorPicker exposing (hex2Color)
import Html exposing (..)
import Html.Attributes exposing (value)
import Html.Events exposing (onInput, onSubmit)


submitDugme : Dugme.Dugme
submitDugme =
    Dugme.init Color.lightPurple Color.yellow "Hajmo"


type Msg
    = SubmitDugme Dugme.Msg
    | SetPrva String
    | SetDruga String
    | SetTitle String
    | Submit (Maybe BojaForma)


type alias Model =
    { sdugme : Dugme.Dugme
    , prvu : String
    , drugu : String
    , title : String
    , parsd : Maybe BojaForma
    }


type alias BojaForma =
    { p : Color
    , d : Color
    , t : String
    }


init : Int -> Model
init a =
    { sdugme = submitDugme
    , prvu = ""
    , drugu = ""
    , title = ""
    , parsd = Nothing
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SubmitDugme dmsg ->
            ({ model
                | sdugme = Dugme.update dmsg model.sdugme
            }
                , Cmd.none)

        SetPrva s ->
            ((parseForm { model | prvu = s }) , Cmd.none)

        SetDruga s ->
            ((parseForm { model | drugu = s }) , Cmd.none)

        SetTitle s ->
            ((parseForm { model | title = s }) , Cmd.none)

        Submit _ ->
            ((init 0) , Cmd.none)


parseForm : Model -> Model
parseForm model =
    let
        parsd =
            Maybe.map3 BojaForma
                (hex2Color model.prvu)
                (hex2Color model.drugu)
                (Just model.title)
    in
        { model | parsd = parsd }


view : Model -> Html Msg
view model =
    let
        dugme =
            Html.map SubmitDugme <| Dugme.view model.sdugme

        frm = model.parsd
    in
        form [ onSubmit (Submit frm) ]
            [ label []
                [ text "Prva"
                , input [ onInput SetPrva, value model.prvu ] []
                ]
            , label []
                [ text "Druga"
                , input [ onInput SetDruga, value model.drugu ] []
                ]
            , label []
                [ text "Title"
                , input [ onInput SetTitle, value model.title ] []
                ]
            , dugme
            ]
