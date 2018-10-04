module GejmOfLajf exposing (..)

import Matrix exposing (..)
import Random exposing (pair, list, int, generate, Generator)
import Time exposing (Posix)
import Html exposing (..)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Tuple


defaultBoardSize : Int
defaultBoardSize = 30

boardPxWH : Float
boardPxWH = 700

defaultRefreshTime : Float
defaultRefreshTime = 150

type Celija
    = Ziva
    | Mrtva


type Msg
    = Tick Posix
    | ToggleRunning
    | Reseed
    | Recreate (List(Int, Int))
    | Zoom Int
    | KillAll
    | Klik (Int, Int) Celija
    | Step 
    | Accelerate Float



type alias Tabla =
    Matrix Celija


type alias Model =
    { matrica : Tabla
    , boardSize: Int
    , clock : Posix
    , counter : Posix
    , genNumb : Int
    , running : Bool
    , refreshTime: Float
    }


init : List ( Int, Int ) -> Model
init zivi =
    Model 
        (ozivi zivi defaultBoardSize) 
        defaultBoardSize 
        (Time.millisToPosix 0) 
        (Time.millisToPosix 0) 
        0 
        True 
        defaultRefreshTime


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
    if d > 0 
        then enlarge t d
    else if d < 0 
        then ensmallen t (abs d)
    else t


enlarge : Tabla -> Int -> Tabla
enlarge t d =
    let
        hsides = (repeat d (matrixHeight t)  Mrtva)

        nt = Maybe.withDefault t
            (concatHorizontal hsides t)
        nt1 = Maybe.withDefault t
            (concatHorizontal nt hsides)

        vsides = (repeat (matrixWidth nt1) d Mrtva)

        nt2 = Maybe.withDefault t
            (concatVertical vsides nt1)
        nt3 = Maybe.withDefault nt2
            (concatVertical nt2 vsides)
    in
        nt3


ensmallen : Tabla -> Int -> Tabla
ensmallen t delta =
    let
        d = abs delta
        ts = matrixWidth t
        r = ts - 2 * d

        rng = List.repeat r r

        l = List.map (\p -> Matrix.getRow (p+d) t 
                            |> Maybe.map (trimList d)
                            |> Maybe.withDefault []
                    ) rng

        m = l -- Matrix.fromList l |> Maybe.withDefault t
    in
        m


trimList : Int -> List a -> List a
trimList d l =
    List.drop d l |> List.take ((List.length l) - 2*d)


numbOfZive : Int -> Int -> Tabla -> Int
numbOfZive x y t =
    (okoloTorus x y t) |> List.filter isZiva |> List.length


okoloTorus : Int -> Int -> Tabla -> List Celija
okoloTorus x y t =
    let
        mx = matrixHeight t
        my = matrixWidth t

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

                n = Maybe.withDefault Mrtva (matrixGet xn yn t)
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


toggleCell : Celija -> Celija
toggleCell c =
    case c of
        Ziva ->
            Mrtva
        Mrtva ->
            Ziva


tick : Posix -> Model -> Model
tick dt model =
    let
        t = (Time.posixToMillis model.counter) + (Time.posixToMillis dt)
        t1 = if (toFloat t) > model.refreshTime then 0 else t
        m = if t1 == 0 then novoStanje model.matrica else model.matrica
        gn = if t1 == 0 then model.genNumb + 1 else model.genNumb
    in
        if not model.running then model
        else
        { model
            | counter = Time.millisToPosix t1
            -- lol @ Posix
            , clock = Time.millisToPosix ((Time.posixToMillis model.clock) + (Time.posixToMillis dt))
            , genNumb = gn
            , matrica = m }

step : Model -> Model
step m =
    { m 
    | matrica = novoStanje m.matrica
    , genNumb = m.genNumb + 1
    }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ToggleRunning ->
            ({ model | running = not model.running } , Cmd.none)
        Tick t ->
            (tick t model , Cmd.none)
        Reseed ->
            (model , generate Recreate (randomBrojevi model.boardSize))
        Recreate zivi ->
            ({   model
            |   matrica = ozivi zivi model.boardSize
            ,   genNumb = 0
            ,   counter = Time.millisToPosix 0
            } , Cmd.none)
        Zoom d ->
            let
                newBoardSize = model.boardSize + d * 2
            in
                ({   model
                |   matrica = resize model.matrica d
                ,   boardSize = newBoardSize
                } , Cmd.none)
        Klik (x, y) cl ->
            ({ model
            | matrica = indexedMap
                (\cx cy c ->
                    if (cx == x) && (cy == y) then
                        toggleCell c
                    else
                        c
                )
                model.matrica
            } , Cmd.none)
        KillAll ->
            ({ model
            | matrica = repeat model.boardSize model.boardSize Mrtva 
            } , Cmd.none)
        Step ->
            (step model
            , Cmd.none)
        Accelerate d ->
            ({ model
            | refreshTime = model.refreshTime + d 
            } , Cmd.none)


celToString : Celija -> String
celToString cel =
    case cel of
        Mrtva -> "Mrtva"
        Ziva -> "Ziva"

celToEle : Celija -> (Int, Int) -> List ( Html.Attribute Msg ) -> Html Msg
celToEle c (x, y) pos =
    let
        attrs = List.append
            [ class ((celToString c) ++ " celija")
            , onClick (Klik (x, y) c)
            ]
            pos
    in
    
        div attrs
            []


cellSize : Int -> Float
cellSize boardSize =
    boardPxWH / (toFloat boardSize)


matrixToBoard : Int -> Int -> Int -> Celija -> Html Msg
matrixToBoard boardSize x y c  =
    let
        x_ =
            toFloat x * (cellSize boardSize)

        y_ =
            toFloat y * (cellSize boardSize)

        pos =
            [ style  "top" ((String.fromFloat y_) ++ "px") , style "left" ((String.fromFloat x_) ++ "px" ) ]

        size =
            [ style "width" ((String.fromFloat (cellSize boardSize)) ++ "px" )
            , style "height" ((String.fromFloat (cellSize boardSize)) ++ "px" )
            ]
    in
        celToEle c (x, y) (List.append pos size)


istocifra: String -> Float -> String
istocifra a rt =
    let
        pr = logBase 10 (rt + 1) |> ceiling
    in
        String.padLeft pr '0' a


randomBrojevi : Int -> Generator (List ( Int, Int ))
randomBrojevi boardSize =
    list ((toFloat boardSize ^ 2 / 4) |> floor)
        <| pair (int 0 boardSize) (int 0 boardSize)


view : Model -> Html Msg
view model =
    -- let
    --     board = (indexedMap
    --             (matrixToBoard model.boardSize)
    --             model.matrica
    --         )
    --         |> toIndexedArray
    
    -- in
        
    div []
    [   div [ class "board" ]
            ((indexedMap
                (matrixToBoard model.boardSize)
                model.matrica
            )   |> Matrix.toList
                -- |> toIndexedArray
                -- |> Array.map (\((_),c) -> c)
                -- |> Array.toList
            )
    ,   table []
        [   tr [] [
                td [] [ text ("gen: " ++ (String.fromInt model.genNumb)
                                ++ ":" ++ (istocifra (String.fromInt (Time.posixToMillis model.counter)) model.refreshTime)
                                ++ "/" ++ (String.fromFloat model.refreshTime))
                ]
            ,   td [] [ text ("size: " ++ (String.fromInt model.boardSize) ++ " ^2")
                ]
            ]
        ,   tr [] [
                td [] [
                    button [ onClick ToggleRunning ] [text (if not model.running then "START" else "STOP")]
                ]
            ,   td [] [
                button [ onClick Step ] [text "Step"]
            ]
            ,   td [] [
                    button [ onClick (Zoom 1) ] [text "Zoom Out"]
                ]
            ]
        ,   tr [] [
               td [] [
                    button [ onClick KillAll ] [text "KILL ALL"]
                ]
            ,   td [] [
                    button [ onClick (Accelerate -25) ] [text "Spd +"]
                ,   button [ onClick (Accelerate 25) ] [text "Spd -"]
            ]
            ,   td [] [
                    button [ onClick (Zoom -1) ] [text "Zoom In"]
                ]
            ]
        ,   tr [] [
               td [] [
                    button [ onClick Reseed ] [text "RESEED"]
                ]
            ]
        ]
    ]
