module Matrix exposing
    ( Matrix
    , concatHorizontal
    , concatVertical
    , first
    , foldl
    , foldr
    , generate
    , get
    , getColumn
    , getRow
    , height
    , indexedMap
    , map
    , put
    , repeat
    , tm
    , toList
    , width
    )

import List as L exposing (..)
import Maybe.Extra as ME


type alias Matrix a =
    List (List a)


repeat : Int -> Int -> a -> Matrix a
repeat w h =
    L.repeat w >> L.repeat h


generate : Int -> Int -> (Int -> Int -> a) -> Matrix a
generate w h f =
    L.map
        (\y ->
            L.map
                (\x -> f x y)
                (L.range 0 (w - 1))
        )
        (L.range 0 (h - 1))


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
height =
    L.length


width : Matrix a -> Int
width =
    head >> Maybe.withDefault [] >> L.length


concatHorizontal : Matrix a -> Matrix a -> Maybe (Matrix a)
concatHorizontal m n =
    if height m == height n then
        Just <|
            L.indexedMap
                (\i mrow -> getRow i n |> Maybe.withDefault [] |> L.append mrow)
                m

    else
        Nothing


concatVertical : Matrix a -> Matrix a -> Maybe (Matrix a)
concatVertical m n =
    if width m == width n then
        Just <| L.append m n

    else
        Nothing


first : Matrix a -> Maybe a
first =
    List.head >> Maybe.withDefault [] >> List.head


get : Int -> Int -> Matrix a -> Maybe a
get x y =
    L.drop y >> L.head >> Maybe.withDefault [] >> L.drop x >> L.head


put : Int -> Int -> a -> Matrix a -> Matrix a
put x y a =
    indexedMap
        (\xc yc ele ->
            if x == xc && y == yc then
                a

            else
                ele
        )


getRow : Int -> Matrix a -> Maybe (List a)
getRow y =
    if y >= 0 then
        L.drop y >> head

    else
        \_ -> Nothing


getColumn : Int -> Matrix a -> Maybe (List a)
getColumn x m =
    L.foldr
        (\row c -> (L.drop x row |> L.head) :: c)
        []
        m
        |> ME.combine


toList : Matrix a -> List a
toList =
    L.concat



{--
        Primer
--}


tm : Matrix ( Int, Int, String )
tm =
    generate 5 10 (\x y -> ( x, y, "x" ++ String.fromInt x ++ " y" ++ String.fromInt y ))


lma2mla : List (Maybe a) -> Maybe (List a)
lma2mla =
    List.foldr
        (\a r ->
            case a of
                Just x ->
                    Just (x :: r)

                Nothing ->
                    Nothing
        )
        (Just [])
