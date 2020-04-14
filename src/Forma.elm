module Forma exposing (view, update, Model, Msg(..), init)

import Dugme
import Color exposing (Color)
import Hex
import Html.Attributes exposing (value)
import Html.Events exposing (onInput, onSubmit)
import Html


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
init _ =
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
            (parseForm { model | prvu = s } , Cmd.none)

        SetDruga s ->
            (parseForm { model | drugu = s } , Cmd.none)

        SetTitle s ->
            (parseForm { model | title = s } , Cmd.none)

        Submit _ ->
            (init 0 , Cmd.none)


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

hex2Color : String -> Maybe Color
hex2Color hex =
    let
        r = String.slice 0 1 hex
        g = String.slice 2 3 hex
        b = String.slice 4 5 hex

        red  = Hex.fromString r |> Result.withDefault 0 |> toFloat
        green  = Hex.fromString g |> Result.withDefault 0 |> toFloat
        blue = Hex.fromString b |> Result.withDefault 0 |> toFloat
    in
        Just (Color.rgb red green blue)


view : Model -> Html.Html Msg
view model =
    let
        dugme =
            Html.map SubmitDugme <| Dugme.view model.sdugme

        frm = model.parsd
    in
        Html.form [ onSubmit (Submit frm) ]
            [ Html.label []
                [ Html.text "Prva"
                , Html.input [ onInput SetPrva, value model.prvu ] []
                ]
            , Html.label []
                [ Html.text "Druga"
                , Html.input [ onInput SetDruga, value model.drugu ] []
                ]
            , Html.label []
                [ Html.text "Title"
                , Html.input [ onInput SetTitle, value model.title ] []
                ]
            , dugme
            ]
