module GejmOfLajf exposing (..)

import Matrix exposing (..)
import Matrix.Extra exposing (neighbours)
import Random exposing (pair, list, int, generate, Generator)
import Time exposing (Time)
import Html exposing (..)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Array.Hamt as Array
import Tuple


defaultBoardSize : Int
defaultBoardSize = 30


boardPxWH : Int
boardPxWH = 700

refreshTime : Float
refreshTime = 150

type Celija
    = Ziva
    | Mrtva


type Msg
    = Tick Time
    | ToggleRunning
    | Reseed
    | Recreate (List(Int, Int))
    | Zoom Int


type alias Tabla =
    Matrix Celija


type alias Model =
    { matrica : Tabla
    , boardSize: Int
    , clock : Time
    , counter : Time
    , genNumb : Int
    , running : Bool
    , refreshTime: Float
    }


init : List ( Int, Int ) -> Model
init zivi =
    Model (ozivi zivi defaultBoardSize) defaultBoardSize 0 0 0 True refreshTime

ozivi : List (Int, Int) -> Int -> Matrix Celija
ozivi zivi boardSize =
    (repeat boardSize boardSize Mrtva)
        |> indexedMap
            (\x y c ->
                if List.member ( x, y ) zivi then
                    Ziva
                else
                    Mrtva
            )


resize : Tabla -> Int -> Tabla
resize t d =
    let
        hsides = (repeat d (Matrix.height t)  Mrtva)

        nt = Maybe.withDefault t
            (concatHorizontal hsides t)
        nt1 = Maybe.withDefault t
            (concatHorizontal nt hsides)

        vsides = (repeat (Matrix.width nt1) d Mrtva)  

        nt2 = Maybe.withDefault t
            (concatVertical vsides nt1)
        nt3 = Maybe.withDefault nt2
            (concatVertical nt2 vsides)
    in
        nt3
        


numbOfZive : Int -> Int -> Tabla -> Int
numbOfZive x y t =
    (okoloTorus x y t) |> List.filter isZiva |> List.length


okoloTorus : Int -> Int -> Tabla -> List Celija
okoloTorus x y t =
    let
        mx = Matrix.height t
        my = Matrix.width t

        locs =  [   (-1, -1), (0, -1), (1, -1)
                ,   (-1,  0),          (1,  0)
                ,   (-1,  1), (0,  1), (1,  1)
                ]

        l = List.map (\pos ->
            let
                dx = Tuple.first pos
                dy = Tuple.second pos

                xn = modulo (dx + x) mx
                yn = modulo (dy + y) my

                n = Maybe.withDefault Mrtva (Matrix.get xn yn t)
            in
                n
            ) locs
    in
        l

modulo: Int -> Int -> Int
modulo a m =
    if a < 0
        then m + a
    else if a >= m
        then m - a
    else a

novoStanje : Tabla -> Tabla
novoStanje t =
    t
        |> indexedMap
            (\x y c ->
                let
                    komsije =
                        numbOfZive x y t
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


tick : Time -> Model -> Model
tick dt model =
    let
        t = model.counter + dt
        t1 = if t > refreshTime then 0 else t
        m = if t1 == 0 then novoStanje model.matrica else model.matrica
        gn = if t1 == 0 then model.genNumb + 1 else model.genNumb
    in
        if not model.running then model
        else
        { model
            | counter = t1
            , clock = model.clock + dt
            , genNumb = gn
            , matrica = m }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ToggleRunning ->
            { model | running = not model.running } ! []
        Tick t ->
            tick t model ! []
        Reseed ->
            model ! [generate Recreate (randomBrojevi model.boardSize)]
        Recreate zivi ->
            {   model
            |   matrica = ozivi zivi model.boardSize
            ,   genNumb = 0
            ,   counter = 0
            } ! []
        Zoom d ->
            let
                newBoardSize = model.boardSize + d * 2
            in
                {   model
                |   matrica = resize model.matrica d
                ,   boardSize = newBoardSize
                } ! []


celToEle : Celija -> List ( String, String ) -> Html msg
celToEle c pos =
    div
        [ class ((toString c) ++ " celija")
        , style pos
        ]
        []


cellSize : Int -> Int
cellSize boardSize =
    boardPxWH // boardSize


matrixToBoard : Int -> Int -> Int -> Celija -> Html msg
matrixToBoard boardSize x y c  =
    let
        x_ =
            x * (cellSize boardSize)

        y_ =
            y * (cellSize boardSize)

        pos =
            [ ( "top", (toString y_) ++ "px" ), ( "left", (toString x_) ++ "px" ) ]

        size =
            [ ( "width", (toString (cellSize boardSize)) ++ "px" )
            , ( "height", (toString (cellSize boardSize)) ++ "px" )
            ]
    in
        celToEle c (pos ++ size)

istocifra: String -> String
istocifra a =
    let
        pr = logBase 10 (refreshTime + 1) |> ceiling
    in
        String.padLeft pr '0' a


randomBrojevi : Int -> Generator (List ( Int, Int ))
randomBrojevi boardSize =
    list ((toFloat boardSize ^ 2 / 4) |> floor)
        <| pair (int 0 boardSize) (int 0 boardSize)


view : Model -> Html Msg
view model =
    let
        board = (indexedMap
                (matrixToBoard model.boardSize)
                model.matrica
            )
            |> toIndexedArray
    
    in
        
    div []
    [   div [ class "board" ]
            ((indexedMap
                (matrixToBoard model.boardSize)
                model.matrica
            )
                |> toIndexedArray
                |> Array.map (\((_),c) -> c)
                |> Array.toList
            )
    ,   table []
        [   tr [] [
                td [] [ text ("gen: " ++ (toString model.genNumb)
                                ++ ":" ++ (istocifra (toString model.counter))
                                ++ "/" ++ (toString refreshTime))
                ]
            ]
        ,   tr [] [
                td [] [
                    button [ onClick ToggleRunning ] [text (if not model.running then "START" else "STOP")]
                ]
            ,   td [] [
                    button [ onClick Reseed ] [text "RESTART"]
                ]
            ,   td [] [
                    button [ onClick (Zoom 1) ] [text "Zoom Out"]
                ]
            ,   td [] [
                    button [ onClick (Zoom -1), Html.Attributes.disabled True ] [text "Zoom In"]
                ]

            ]
        ,   tr [] [
                td [] []
            ]
        ]
    ]
