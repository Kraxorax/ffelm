module Matrix exposing 
    ( ..
    )

import List as L exposing (..)


{--
        Public
--}

type alias Matrix a = List (List a)

type MatrixTopology 
    = Plane 
    | Torus
    | StripHorizontal
    | StripVertical
    -- | MobiusStrip

repeat : Int -> Int -> a -> Matrix a
repeat w h = L.repeat w >> L.repeat h

generate : Int -> Int -> (Int -> Int -> a) -> Matrix a
generate w h f = 
    L.map 
        (\y -> L.map
            (\x -> f x y) 
            (L.range 0 (w-1))
        ) 
        (L.range 0 (h-1))

indexedMap : (Int -> Int -> a -> b) -> Matrix a -> Matrix b
indexedMap f = 
    List.indexedMap 
        (\y row -> 
            List.indexedMap 
                (\x ele -> f x y ele) 
                row
        )

map : (a -> b) -> Matrix a -> Matrix b
map f =
    List.map
        (\row ->
            L.map
                (\ele -> f ele)
                row
        )

foldr : (a -> b -> b) -> b -> Matrix a -> b
foldr f =
    L.foldr
        (\row bc -> 
            L.foldr
                (\ele bcc -> f ele bcc)
                bc
                row
        )

foldl : (a -> b -> b) -> b -> Matrix a -> b
foldl f =
    L.foldl
        (\row bc -> 
            L.foldl
                (\ele bcc -> f ele bcc)
                bc
                row
        )


height : Matrix a -> Int
height = L.length

width : Matrix a -> Int
width = head >> Maybe.withDefault [] >> L.length

concatHorizontal : Matrix a -> Matrix a -> Maybe (Matrix a)
concatHorizontal m n = 
    if ((height m) == (height n))
    then
        Just <| L.indexedMap
                    (\i mrow -> (getRow i n) |> Maybe.withDefault [] |> L.append mrow)
                    m
    else
        Nothing

concatVertical : Matrix a -> Matrix a -> Maybe (Matrix a)
concatVertical m n =
    if ((width m) == width n)
    then
        Just <| L.append m n
    else
        Nothing

first : Matrix a -> Maybe a
first = List.head >> Maybe.withDefault [] >> List.head 

get : Int -> Int -> Matrix a -> Maybe a
get x y = L.drop y >> L.head >> Maybe.withDefault [] >> L.drop x >> L.head

getRow : Int -> Matrix a -> Maybe (List a)
getRow y = 
    if y >= 0
    then L.drop y >> head
    else \_ -> Nothing


neighboursOnTorus : Int -> Int -> Matrix a -> List a
neighboursOnTorus x y m =
    let
        mw = width m
        mh = height m

        r1 = Maybe.withDefault [] (getRow (modulo (y - 1) mh) m)
        r2 = Maybe.withDefault [] (getRow (y) m)
        r3 = Maybe.withDefault [] (getRow (modulo (y + 1) mh) m)

        start = modulo (x - 1) mw
        end = modulo (x + 1) mw

    in
        (unboundHorizontalSide start end r1 ) 
        |> L.append (unboundHorizontalCenter start end r2 ) 
        |> L.append (unboundHorizontalSide start end r3)

neighboursOnPlane : Int -> Int -> Matrix a -> List a
neighboursOnPlane x y m =
    let
        startRow = max (y - 1) 0
        numToTakeRows = if (y - 1) < 0 then 3 + (y - 1) else 3
        rows = L.drop startRow m |> L.take numToTakeRows

        start = x - 1
        end = x + 1
        numToTakeCells = if start < 0 then 3 + start else 3

    in 
        rows |> L.indexedMap (\yc row ->
            if startRow + yc == y
            then
                boundHorizontalCenter start end row
            else
                boundHorizontalSide start end row
        ) |> L.concat


neigbursOnVerticalStrip : Int -> Int -> Matrix a -> List a
neigbursOnVerticalStrip x y m =
    let
        mw = width m
        mh = height m

        r1 = Maybe.withDefault [] (getRow (modulo (y - 1) mh) m)
        r2 = Maybe.withDefault [] (getRow (y) m)
        r3 = Maybe.withDefault [] (getRow (modulo (y + 1) mh) m)

        startRow = max (y - 1) 0
        numToTakeRows = if (y - 1) < 0 then 3 + (y - 1) else 3
        rows = L.drop startRow m |> L.take numToTakeRows

        start = x - 1
        end = x + 1
        numToTakeCells = if start < 0 then 3 + start else 3

    in
        (r1 :: r3 :: []) 
            |> L.map (\row ->
                boundHorizontalSide start end row
            ) 
            |> L.append (boundHorizontalCenter start end [r2]) 
            |> L.concat


neigbursOnHorizontalStrip : Int -> Int -> Matrix a -> List a
neigbursOnHorizontalStrip x y m =
    let
        mw = width m
        mh = height m

        rowCenter = getRow y m |> Maybe.withDefault []
        rowSides = (getRow (y - 1) m :: [getRow (y + 1) m])
            |> L.map (Maybe.withDefault [])

        start = modulo (x - 1) mw
        end = modulo (x + 1) mw

    in
        unboundHorizontalCenter start end rowCenter
        |> L.append 
            (rowSides 
                |> L.map (unboundHorizontalSide start end) 
                |> L.concat
            )

    


neighbours : MatrixTopology -> Int -> Int -> Matrix a -> List a
neighbours mt =
    case mt of
        Plane -> neighboursOnPlane
        Torus -> neighboursOnTorus
        StripVertical -> neigbursOnVerticalStrip
        StripHorizontal -> neigbursOnHorizontalStrip


toList : Matrix a -> List a
toList = L.concat


{--
        Private
--}


modulo: Int -> Int -> Int
modulo a m =
    if a < 0
        then m + a
    else if a >= m
        then m - a
    else a


unboundHorizontalSide : Int -> Int -> List a -> List a
unboundHorizontalSide start end l =
    if (end < start)
    then
        (L.take (end + 1) l) 
            |> L.append (
                (L.drop start l) 
                    |> L.take 3
            )
    else
        L.drop start l |> L.take 3

unboundHorizontalCenter : Int -> Int -> List a -> List a
unboundHorizontalCenter start end l =
    L.drop start l |> L.take 1 
    |> L.append (L.drop end l |> L.take 1)

boundHorizontalCenter : Int -> Int -> List a -> List a
boundHorizontalCenter start end l =
    if start < 0
        then
            (L.drop end l |> L.take 1)
        else
            L.drop start l |> L.take 1 
            |> L.append (L.drop end l |> L.take 1)

boundHorizontalSide : Int -> Int -> List a -> List a
boundHorizontalSide start toTake l =
    L.drop start l |> L.take toTake


{-- 
        Primer
--}
tm : Matrix (Int, Int, String)
tm = generate 5 10 (\x y -> (x, y, "x"++(String.fromInt x)++" y"++(String.fromInt y)))