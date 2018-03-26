module GejmOfLajf exposing (..)

import Matrix exposing (..)
import Matrix.Extra exposing (neighbours)
import Time exposing (Time)
import Html exposing (..)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Array
import Tuple


boardSize : Int
boardSize = 60

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


type alias Tabla =
    Matrix Celija


type alias Model =
    { matrica : Tabla
    , clock : Time
    , counter : Time
    , genNumb : Int
    , running : Bool
    , refreshTime: Float
    }


init : List ( Int, Int ) -> Model
init zivi =
    let
        matrica =
            (repeat boardSize boardSize Mrtva)
                |> indexedMap
                    (\x y c ->
                        if List.member ( x, y ) zivi then
                            Ziva
                        else
                            Mrtva
                    )
    in
        Model matrica 0 0 0 True refreshTime


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
                xn = modOp x mx ((+)(Tuple.first pos))
                yn = modOp y my ((+)(Tuple.second pos))
                n = Maybe.withDefault Mrtva (Matrix.get xn yn t)
            in
                n
            ) locs
    in
        l

modOp : Int -> Int -> (Int -> Int) -> Int
modOp a m o =
    let
        r = o a
        rm = 
            if r >= m 
                then r - m
            else if r < 0
                then r + m
            else r
    in
        rm



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

update : Msg -> Model -> Model
update msg model =
    case msg of
        ToggleRunning ->
            { model | running = not model.running }
        Tick t ->
            tick t model
        Reseed ->
            model

celToEle : Celija -> List ( String, String ) -> Html msg
celToEle c pos =
    div
        [ class ((toString c) ++ " celija")
        , style pos
        ]
        []


cellSize : Int
cellSize =
    (toFloat boardPxWH / toFloat boardSize) |> floor


matrixToBoard : Int -> Int -> Celija -> Html msg
matrixToBoard x y c =
    let
        x_ =
            x * cellSize

        y_ =
            y * cellSize

        pos =
            [ ( "top", (toString y_) ++ "px" ), ( "left", (toString x_) ++ "px" ) ]

        size =
            [ ( "width", (toString cellSize) ++ "px" )
            , ( "height", (toString cellSize) ++ "px" )
            ]
    in
        celToEle c (pos ++ size)

istocifra: String -> String
istocifra a =
    let
        pr = logBase 10 (refreshTime + 1) |> ceiling
    in
        String.padLeft pr '0' a


view : Model -> Html Msg
view model =
    div []
    [   div [ class "board" ]
            ((indexedMap
                matrixToBoard
                model.matrica
            )
                |> toIndexedArray
                |> Array.map Tuple.second
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
                    button [ onClick ToggleRunning ] [text "RESTART"]
                ]
            ]
        ,   tr [] [
                td [] []
            ]
        ]
    ]
