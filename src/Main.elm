module Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Browser.Navigation exposing (load, pushUrl)
import Color
import Html exposing (Html)
import Html.Attributes exposing (href)
import Model exposing (Model)
import Random exposing (generate)
import Routing exposing (Route(..))
import Url
import Pages.Forma.Dugme as Dugme exposing (Dugme)
import Pages.Forma.Forma as Forma
import Pages.GejmOfLajf as GejmOfLajf
import Pages.Randomer as Randomer
import Pages.Klokotalo.Klok as Klok
import Pages.Klokotalo.Klokotalo as Klokotalo


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlUpdate -- \url -> UrlUpdate url
        , onUrlRequest = RequestedUrl --\urlReq -> RequestedUrl urlReq
        }


init : () -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init _ location key =
    let
        route =
            Routing.routeLocation location
    in
    ( { navKey = key
      , dugmici =
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

        , randomer = Randomer.init
      }
    , generate RandomGen (GejmOfLajf.randomBrojevi GejmOfLajf.defaultBoardSize)
    )


type Msg
    = Dugmici Int Dugme.Msg
    | NasaForma Forma.Msg
    | Klokotalo Klokotalo.Msg
    | UrlUpdate Url.Url
    | RequestedUrl Browser.UrlRequest
    | Gol GejmOfLajf.Msg
    | Rndmr Randomer.Msg
    | Animate Float
    | RandomGen (List ( Int, Int ))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlUpdate location ->
            let
                route =
                    Routing.routeLocation location
            in
            ( { model | route = route }, Cmd.none )

        RequestedUrl urlReq ->
            case urlReq of
                Browser.Internal url ->
                    ( model, pushUrl model.navKey (Url.toString url) )

                Browser.External href ->
                    ( model, load href )

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
            ( { model | dugmici = ndugmici, naziv = noviNaziv }, Cmd.none )

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
            ( { model | formica = nf, dugmici = dugmici }, Cmd.map NasaForma cmd )

        Klokotalo kt ->
            ( { model | klokotalo = Klokotalo.update kt model.klokotalo }, Cmd.none )

        Animate diff ->
            ( { model
                | clock = model.clock
                , gol = GejmOfLajf.tick diff model.gol
              }
            , Cmd.none
            )

        Gol golMsg ->
            let
                modCmd =
                    GejmOfLajf.update golMsg model.gol
            in
            ( { model
                | gol = Tuple.first modCmd
              }
            , Cmd.map (\gm -> Gol gm) (Tuple.second modCmd)
            )

        RandomGen zivi ->
            ( { model
                | gol = GejmOfLajf.init zivi
              }
            , Cmd.none
            )

        Rndmr rndMsg ->
            
            ( { model
                | randomer = Randomer.update rndMsg model.randomer }, Cmd.none)


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch [ Browser.Events.onAnimationFrameDelta Animate ]


pageByRoute : Model -> Html Msg
pageByRoute model =
    case model.route of
        Forma ->
            let
                naziv =
                    case model.naziv of
                        Just t ->
                            Html.h1 [] [ Html.text t ]

                        Nothing ->
                            Html.div [] []

                levo =
                    naziv :: dugmad model.dugmici

                desno =
                    Forma.view model.formica
                        |> Html.map NasaForma
            in
            Html.div [] (levo ++ [ desno ])

        Klokotala ->
            Html.div []
                [ Html.h1 [] [ Html.text <| "klokotala" ]
                , Klokotalo.view model.klokotalo |> Html.map Klokotalo
                ]

        GOL ->
            Html.map (\golMsg -> Gol golMsg) (GejmOfLajf.view model.gol)

        Randomer ->
            Html.map (\rndmrMsg -> Rndmr rndmrMsg) (Randomer.view model.randomer)


view : Model -> Browser.Document Msg
view model =
    let
        stabre =
            pageByRoute model
    in
    Browser.Document
        "AAAA"
        [ Html.div []
            [ navView
            , Html.h6 [] [ Html.text (model.clock |> String.fromFloat) ]
            , Html.div [] [ stabre ]
            ]
        ]


navView : Html Msg
navView =
    Html.ul []
        [ Html.li []
            [ Html.a [ Html.Attributes.href "/forma" ] [ Html.text "forma" ] ]
        , Html.li []
            [ Html.a [ Html.Attributes.href "/klokotala" ] [ Html.text "klokotala" ] ]
        , Html.li []
            [ Html.a [ Html.Attributes.href "/gol" ] [ Html.text "game of life" ] ]
        , Html.li []
            [ Html.a [ Html.Attributes.href "/randomer" ] [ Html.text "the randomer generative creator supreme" ] ]
        ]


dugmad : List Dugme -> List (Html Msg)
dugmad =
    List.indexedMap
        (\i dug ->
            Html.map (Dugmici i) (Dugme.view dug)
        )
