module Klokotalo exposing (Model, Msg(..), Operacija(..), crtajKlokove, init, update, view)

import Debug exposing (log)
import Html as Html
import Html.Events exposing (onClick)
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
                    in
                    { model
                        | rezultat = Just (toFloat (List.foldl (\r f -> log "f" f - log "r" r) (log "first" first) (log "rest" rest)))
                    }

                Deljenje ->
                    case model.klokovi of
                        [] ->
                            { model | rezultat = Just 1.0 }

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
                ks =
                    model.klokovi
                        |> List.map
                            (\k ->
                                if k.id == id then
                                    Klok.update klik k

                                else
                                    k
                            )
            in
            { model | klokovi = ks }


view : Model -> Html.Html Msg
view model =
    let
        ks =
            model.klokovi

        rezultat =
            model.rezultat |> Maybe.withDefault 0

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
    Html.div []
        [ Html.button [ onClick DodajKlok ] [ Html.text "dodaj klok" ]
        , Html.div [] (crtajKlokove ks)
        , Html.button
            [ onClick (PostaviOperaciju novaop) ]
            [ Html.text ("promeni u " ++ (novaop |> Debug.toString)) ]
        , Html.div [] [ Html.text (operacija |> Debug.toString) ]
        , Html.button [ onClick Racunaj ] [ Html.text "racunaj" ]
        , Html.div [] [ Html.text (rezultat |> String.fromFloat) ]
        ]


crtajKlokove : List Klok.Klok -> List (Html.Html Msg)
crtajKlokove =
    List.map (\k -> Html.map (Klik k.id) (Klok.view k))
