module Forma exposing (..)

import Dugme
import Color exposing (Color)
import Html exposing (..)
import Html.Attributes exposing (value)
import Html.Events exposing (onInput, onSubmit)
import Color.Convert exposing (hexToColor)


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
    , parsd : Result String BojaForma
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
    , parsd = Err "Prazan Input"
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SubmitDugme dmsg ->
            { model
                | sdugme = Dugme.update dmsg model.sdugme
            }
                ! []

        SetPrva s ->
            (parseForm { model | prvu = s }) ! []

        SetDruga s ->
            (parseForm { model | drugu = s }) ! []

        SetTitle s ->
            (parseForm { model | title = s }) ! []

        Submit _ ->
            (init 0) ! []


parseForm : Model -> Model
parseForm model =
    let
        parsd =
            Result.map3 BojaForma
                (hexToColor model.prvu)
                (hexToColor model.drugu)
                (Ok model.title)
    in
        { model | parsd = parsd }


view : Model -> Html Msg
view model =
    let
        dugme =
            Html.map SubmitDugme <| Dugme.view model.sdugme

        frm =
            case model.parsd of
                Err _ ->
                    Nothing

                Ok f ->
                    Just f
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
