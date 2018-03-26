module Klokotalo exposing (..)

import Html exposing (..)
import Html.Events exposing (on, onInput, onClick)
import Debug exposing (..)
import Klok


type alias Model =
    { klokovi : List Klok.Klok
    , operacija : Operacija
    , rezultat : Maybe Float
    }


type Operacija
    = Sabiranje
    | Mnozenje
    | Oduzimanje
    | Deljenje


type Msg
    = Racunaj
    | PostaviOperaciju Operacija
    | DodajKlok
    | Klik String Klok.Msg


init : Model
init =
    { klokovi = [ Klok.init 0 2, Klok.init 0 1, Klok.init 0 0 ]
    , operacija = Sabiranje
    , rezultat = Nothing
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        PostaviOperaciju op ->
            { model | operacija = op }

        Racunaj ->
            case model.operacija of
                Sabiranje ->
                    { model
                        | rezultat =
                            Just
                                (List.foldl (+)
                                    0
                                    (List.map (\k -> toFloat k.i) model.klokovi)
                                )
                    }

                Mnozenje ->
                    { model
                        | rezultat =
                            Just
                                ((List.map (\k -> toFloat k.i)
                                    >> List.foldl (*) 1
                                 )
                                 <|
                                    model.klokovi
                                )
                    }

                Oduzimanje ->
                    let
                        vals =
                            List.map (\k -> k.i) model.klokovi

                        first =
                            List.head vals |> Maybe.withDefault 0

                        rest =
                            List.tail vals |> Maybe.withDefault []

                        --   log "first" first
                        --   log "rest" rest
                    in
                        { model
                            | rezultat = Just (toFloat (List.foldl (\r f -> (log "f" f) - (log "r" r)) (log "first" first) (log "rest" rest)))
                        }

                Deljenje ->
                    case model.klokovi of
                        [] ->
                            { model | rezultat = Just (1.0) }

                        h :: t ->
                            { model
                                | rezultat =
                                    Just
                                        (List.foldl (\x y -> y / x) (toFloat h.i) (List.map (\k -> toFloat k.i) t))
                            }

        DodajKlok ->
            { model
                | klokovi = Klok.init 0 (List.length model.klokovi) :: model.klokovi
            }

        Klik id klik ->
            let
                k =
                    model.klokovi
                        |> List.map
                            (\k ->
                                if k.id == id then
                                    Klok.update klik k
                                else
                                    k
                            )
            in
                { model | klokovi = k }


view : Model -> Html Msg
view model =
    let
        ks =
            model.klokovi

        rezultat =
            (model.rezultat |> Maybe.withDefault 0) |> toString

        operacija =
            model.operacija

        novaop =
            case model.operacija of
                Sabiranje ->
                    Mnozenje

                Mnozenje ->
                    Oduzimanje

                Oduzimanje ->
                    Deljenje

                Deljenje ->
                    Sabiranje
    in
        div []
            [ button [ onClick DodajKlok ] [ text "dodaj klok" ]
            , div [] (crtajKlokove ks)
            , button
                [ onClick (PostaviOperaciju novaop) ]
                [ text ("promeni u " ++ (novaop |> toString)) ]
            , div [] [ text (operacija |> toString) ]
            , button [ onClick Racunaj ] [ text "racunaj" ]
            , div [] [ text rezultat ]
            ]


crtajKlokove : List Klok.Klok -> List (Html Msg)
crtajKlokove =
    List.map (\k -> Html.map (Klik k.id) (Klok.view k))
