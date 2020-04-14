module Pages.GejmOfLajf exposing (Celija(..), Model, Msg(..), Tabla, boardPxWH, celToEle, celToString, cellSize, defaultBoardSize, defaultRefreshTime, enlarge, flyer, init, isZiva, istocifra, matrixToBoard, modulo, novoStanje, numbOfZive, ozivi, randomBrojevi, step, survival, tick, toggleCell, topologyToString, trimList, update, view)

import Array
import Html exposing (..)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Matrix exposing (Matrix, concatHorizontal, concatVertical, generate, indexedMap, repeat)
import Neighbours exposing (MatrixTopology(..), neighbours)
import Random exposing (Generator, generate, int, list, pair)


defaultBoardSize : Int
defaultBoardSize =
    60


boardPxWH : Float
boardPxWH =
    700


defaultRefreshTime : Float
defaultRefreshTime =
    30


type Celija
    = Ziva
    | Mrtva


type Msg
    = Tick Float
    | ToggleRunning
    | Reseed
    | Recreate (List ( Int, Int ))
    | Zoom Int
    | KillAll
    | Klik ( Int, Int ) Celija
    | Step
    | Accelerate Float
    | ChangeTopology MatrixTopology


type alias Tabla =
    Matrix Celija


type alias Model =
    { matrica : Tabla
    , boardSize : Int
    , clock : Int
    , counter : Int
    , genNumb : Int
    , running : Bool
    , refreshTime : Float
    , topology : MatrixTopology
    }


flyer : List ( Int, Int )
flyer =
    [ ( 5, 5 )
    , ( 6, 5 )
    , ( 7, 5 )
    , ( 7, 6 )
    , ( 6, 7 )
    ]


init : List ( Int, Int ) -> Model
init zivi =
    Model
        (ozivi zivi defaultBoardSize)
        defaultBoardSize
        0
        0
        0
        True
        defaultRefreshTime
        Torus


ozivi : List ( Int, Int ) -> Int -> Matrix Celija
ozivi zivi boardSize =
    repeat boardSize boardSize Mrtva
        |> indexedMap
            (\x y _ ->
                if List.member ( x, y ) zivi then
                    Ziva

                else
                    Mrtva
            )


resize : Tabla -> Int -> Tabla
resize t d =
    if d > 0 then
        enlarge t d

    else if d < 0 then
        ensmallen t (abs d)

    else
        t


enlarge : Tabla -> Int -> Tabla
enlarge t d =
    let
        hsides =
            repeat d (Matrix.height t) Mrtva

        nt =
            Result.withDefault t
                (concatHorizontal hsides t)

        nt1 =
            Result.withDefault t
                (concatHorizontal nt hsides)

        vsides =
            repeat (Matrix.width nt1) d Mrtva

        nt2 =
            Result.withDefault t
                (concatVertical vsides nt1)

        nt3 =
            Result.withDefault nt2
                (concatVertical nt2 vsides)
    in
    nt3


ensmallen : Tabla -> Int -> Tabla
ensmallen t delta =
    let
        d =
            abs delta

        ts =
            Matrix.width t

        r =
            ts - 2 * d

        sm =
            Matrix.generate r r (\x y -> Matrix.get (x + d) (y + d) t |> Result.withDefault Mrtva)
    in
    sm


trimList : Int -> List a -> List a
trimList d l =
    List.drop d l |> List.take (List.length l - 2 * d)


numbOfZive : Int -> Int -> Model -> Int
numbOfZive x y m =
    neighbours m.topology x y m.matrica |> Array.toList |> List.filter isZiva |> List.length


modulo : Int -> Int -> Int
modulo a m =
    if a < 0 then
        m + a

    else if a >= m then
        m - a

    else
        a


novoStanje : Model -> Tabla
novoStanje m =
    m.matrica
        |> indexedMap
            (\x y c ->
                let
                    komsije =
                        numbOfZive x y m
                in
                survival komsije c
            )


survival : Int -> Celija -> Celija
survival komsije celija =
    if komsije < 2 then
        Mrtva

    else if komsije == 2 then
        celija

    else if komsije == 3 then
        Ziva

    else
        Mrtva


isZiva : Celija -> Bool
isZiva c =
    case c of
        Ziva ->
            True

        Mrtva ->
            False


toggleCell : Celija -> Celija
toggleCell c =
    case c of
        Ziva ->
            Mrtva

        Mrtva ->
            Ziva


tick : Float -> Model -> Model
tick dt model =
    let
        t =
            toFloat model.counter + dt

        t1 =
            if t > model.refreshTime then
                0

            else
                t

        m =
            if t1 == 0 then
                novoStanje model

            else
                model.matrica

        gn =
            if t1 == 0 then
                model.genNumb + 1

            else
                model.genNumb
    in
    if not model.running then
        model

    else
        { model
            | counter = round t1
            , clock = model.clock + round dt
            , genNumb = gn
            , matrica = m
        }


step : Model -> Model
step m =
    { m
        | matrica = novoStanje m
        , genNumb = m.genNumb + 1
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleRunning ->
            ( { model | running = not model.running }, Cmd.none )

        Tick t ->
            ( tick t model, Cmd.none )

        Reseed ->
            ( model, generate Recreate (randomBrojevi model.boardSize) )

        Recreate zivi ->
            ( { model
                | matrica = ozivi zivi model.boardSize
                , genNumb = 0
                , counter = 0
              }
            , Cmd.none
            )

        Zoom d ->
            let
                newBoardSize =
                    model.boardSize + d * 2
            in
            ( { model
                | matrica = resize model.matrica d
                , boardSize = newBoardSize
              }
            , Cmd.none
            )

        Klik ( x, y ) _ ->
            ( { model
                | matrica =
                    indexedMap
                        (\cx cy c ->
                            if (cx == x) && (cy == y) then
                                toggleCell c

                            else
                                c
                        )
                        model.matrica
              }
            , Cmd.none
            )

        KillAll ->
            ( { model
                | matrica = repeat model.boardSize model.boardSize Mrtva
              }
            , Cmd.none
            )

        Step ->
            ( step model
            , Cmd.none
            )

        Accelerate d ->
            ( { model
                | refreshTime = model.refreshTime + d
              }
            , Cmd.none
            )

        ChangeTopology t ->
            ( { model
                | topology = t
              }
            , Cmd.none
            )


celToString : Celija -> String
celToString cel =
    case cel of
        Mrtva ->
            "Mrtva"

        Ziva ->
            "Ziva"


celToEle : Celija -> ( Int, Int ) -> List (Html.Attribute Msg) -> Html Msg
celToEle c ( x, y ) pos =
    let
        attrs =
            List.append
                [ class (celToString c ++ " celija")
                , onClick (Klik ( x, y ) c)
                ]
                pos
    in
    div attrs
        []


cellSize : Int -> Float
cellSize boardSize =
    boardPxWH / toFloat boardSize


matrixToBoard : Int -> Int -> Int -> Celija -> Html Msg
matrixToBoard boardSize x y c =
    let
        x_ =
            toFloat x * cellSize boardSize

        y_ =
            toFloat y * cellSize boardSize

        bgColor =
            case c of
                Ziva ->
                    "lawngreen"

                Mrtva ->
                    "rgb(160, 160, 160)"

        cellStyle =
            [ style "border" "1px solid lightgray"
            , style "position" "absolute"
            , style "top" (String.fromFloat y_ ++ "px")
            , style "left" (String.fromFloat x_ ++ "px")
            , style "width" (String.fromFloat (cellSize boardSize) ++ "px")
            , style "height" (String.fromFloat (cellSize boardSize) ++ "px")
            , style "background-color" bgColor
            ]
    in
    celToEle c ( x, y ) cellStyle


istocifra : String -> Float -> String
istocifra a rt =
    let
        pr =
            logBase 10 (rt + 1) |> ceiling
    in
    String.padLeft pr '0' a


randomBrojevi : Int -> Generator (List ( Int, Int ))
randomBrojevi boardSize =
    list ((toFloat boardSize ^ 2 / 4) |> floor) <|
        pair (int 0 boardSize) (int 0 boardSize)


topologyToString : MatrixTopology -> String
topologyToString mt =
    case mt of
        Plane ->
            "Plane"

        Torus ->
            "Torus"

        StripHorizontal ->
            "Horizontal Strip"

        StripVertical ->
            "Vertical Strip"


view : Model -> Html Msg
view model =
    let
        boardStyle =
            [ style "position" "relative"
            , style "width" "700px"
            , style "height" "700px"
            ]
    in
    div []
        [ div boardStyle
            (indexedMap
                (matrixToBoard model.boardSize)
                model.matrica
                |> Matrix.toArray
                |> Array.toList
            )
        , table []
            [ tr []
                [ td []
                    [ text
                        ("gen: "
                            ++ String.fromInt model.genNumb
                            ++ ":"
                            ++ istocifra (String.fromInt model.counter) model.refreshTime
                            ++ "/"
                            ++ String.fromFloat model.refreshTime
                        )
                    ]
                , td [] []
                , td []
                    [ text ("size: " ++ String.fromInt model.boardSize ++ " ^2")
                    ]
                , td [] [ text (topologyToString model.topology) ]
                ]
            , tr []
                [ td []
                    [ button [ onClick ToggleRunning ]
                        [ text
                            (if not model.running then
                                "START"

                             else
                                "STOP"
                            )
                        ]
                    ]
                , td []
                    [ button [ onClick Step ] [ text "Step" ]
                    ]
                , td []
                    [ button [ onClick (Zoom 1) ] [ text "Zoom Out" ]
                    ]
                , td []
                    [ button [ onClick (ChangeTopology Torus) ] [ text "Torus" ]
                    ]
                , td []
                    [ button [ onClick (ChangeTopology Plane) ] [ text "Plane" ]
                    ]
                ]
            , tr []
                [ td []
                    [ button [ onClick KillAll ] [ text "KILL ALL" ]
                    ]
                , td []
                    [ button [ onClick (Accelerate -150) ] [ text "Spd +" ]
                    , button [ onClick (Accelerate 150) ] [ text "Spd -" ]
                    ]
                , td []
                    [ button [ onClick (Zoom -1) ] [ text "Zoom In" ]
                    ]
                , td []
                    [ button [ onClick (ChangeTopology StripVertical) ] [ text "Vertical Strip" ]
                    ]
                , td []
                    [ button [ onClick (ChangeTopology StripHorizontal) ] [ text "Horizontal Strip" ]
                    ]
                ]
            , tr []
                [ td []
                    [ button [ onClick Reseed ] [ text "RESEED" ]
                    ]
                ]
            ]
        ]
