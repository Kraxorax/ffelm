module Main exposing (main)

import Html exposing (Html)
import Html.Attributes exposing (href)
import AnimationFrame
import Time exposing (Time)
import Random exposing (pair, list, int, generate, Generator)
import Dugme exposing (Dugme)
import Color
import Forma
import Klok
import Klokotalo
import Navigation
import Model exposing (Model)
import Routing exposing (Route(..))
import GejmOfLajf


main : Program Never Model Msg
main =
    Navigation.program UrlUpdate
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    let
        route =
            Routing.routeLocation location
    in
        ( { dugmici =
                [ Dugme.init Color.darkGray Color.orange "Kara"
                , Dugme.init Color.green Color.red "Klasik"
                ]
          , naziv = Nothing
          , formica = Forma.init 0
          , klok = Klok.init 0 0
          , klokotalo = Klokotalo.init
          , route = route
          , clock = 0
          , counter = 0
          , gol =
                GejmOfLajf.init
                    []
          }
        , generate RandomGen randomBrojevi
        )


type Msg
    = AMsg
    | Dugmici Int Dugme.Msg
    | NasaForma Forma.Msg
    | Klok Klok.Msg
    | Klokotalo Klokotalo.Msg
    | UrlUpdate Navigation.Location
    | Gol GejmOfLajf.Msg
    | Animate Time
    | RandomGen (List ( Int, Int ))


randomBrojevi : Generator (List ( Int, Int ))
randomBrojevi =
    list ((toFloat GejmOfLajf.boardSize ^ 2 / 4) |> floor)
        <| pair (int 0 GejmOfLajf.boardSize) (int 0 GejmOfLajf.boardSize)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlUpdate location ->
            let
                route =
                    Routing.routeLocation location

                cmd =
                    case route of
                        Prva ->
                            Navigation.newUrl "#druga/666"

                        _ ->
                            Cmd.none
            in
                { model | route = route } ! [ cmd ]

        AMsg ->
            model ! []

        Dugmici i dmsg ->
            let
                ( ndugmici, maybstr ) =
                    model.dugmici
                        |> List.indexedMap
                            (\index dug ->
                                if index == i then
                                    case dmsg of
                                        Dugme.Klick ->
                                            ( dug, Just dug.text )

                                        _ ->
                                            let
                                                nd =
                                                    Dugme.update dmsg dug
                                            in
                                                ( nd, model.naziv )
                                else
                                    ( dug, Nothing )
                            )
                        |> List.unzip

                noviNaziv =
                    maybstr
                        |> List.filterMap identity
                        |> List.head
            in
                { model | dugmici = ndugmici, naziv = noviNaziv } ! []

        NasaForma fmsg ->
            let
                ( nf, cmd ) =
                    Forma.update fmsg model.formica

                dugmici =
                    case fmsg of
                        Forma.Submit (Just { p, d, t }) ->
                            let
                                nd =
                                    Dugme.init p d t
                            in
                                nd :: model.dugmici

                        _ ->
                            model.dugmici
            in
                { model | formica = nf, dugmici = dugmici } ! [ Cmd.map NasaForma cmd ]

        Klok kmsg ->
            { model | klok = Klok.update kmsg model.klok } ! []

        Klokotalo kt ->
            { model | klokotalo = Klokotalo.update kt model.klokotalo } ! []

        Animate diff ->
            { model
                | clock = diff + model.clock
                , gol = GejmOfLajf.tick diff model.gol
            }
                ! []

        Gol golMsg ->
            { model
                | gol = GejmOfLajf.update golMsg model.gol
            } ! []

        RandomGen zivi ->
            { model
                | gol = GejmOfLajf.init zivi
            }
                ! []


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ AnimationFrame.diffs Animate ]


view : Model -> Html Msg
view model =
    let
        naziv =
            case model.naziv of
                Just t ->
                    Html.h1 [] [ Html.text t ]

                Nothing ->
                    Html.div [] []

        levo =
            naziv :: (dugmici model.dugmici)

        desno =
            Forma.view model.formica
                |> Html.map NasaForma

        stabre =
            case model.route of
                Prva ->
                    levo ++ [ desno ]

                Druga i ->
                    [ Html.h1 [] [ Html.text <| toString i ]
                    , Klok.view model.klok |> Html.map Klok
                    , Klokotalo.view model.klokotalo |> Html.map Klokotalo
                    , Html.a [ href ("#druga/" ++ (toString (i + 72))) ] [ Html.text "NEXT!" ]
                    ]

                GOL ->
                    [ Html.map (\golMsg -> Gol golMsg) (GejmOfLajf.view model.gol) ]
    in
        Html.div []
            [ Html.h3 [] [ Html.text (model.clock |> toString) ]
            , Html.div [] stabre
            ]


dugmici : List Dugme -> List (Html Msg)
dugmici =
    List.indexedMap
        (\i dug ->
            Html.map (Dugmici i) (Dugme.view dug)
        )
