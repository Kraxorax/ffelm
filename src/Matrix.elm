module Matrix exposing (..)

type alias Matrix a = List (List a)

repeat : Int -> Int -> a -> Matrix a
repeat w h a = List.repeat h (List.repeat w a)

indexedMap : (Int -> Int -> a -> b) -> Matrix a -> Matrix b
indexedMap f = 
    List.indexedMap 
        (\x row -> 
            List.indexedMap 
                (\y ele -> f x y ele) 
                row
        )   
        

matrixHeight : Matrix a -> Int
matrixHeight m = 1

matrixWidth : Matrix a -> Int
matrixWidth m = 1

concatHorizontal : Matrix a -> Matrix a -> Maybe (Matrix a)
concatHorizontal m n = Just m

concatVertical : Matrix a -> Matrix a -> Maybe (Matrix a)
concatVertical m n = Just m

matrixGet : Int -> Int -> Matrix a -> Maybe a
matrixGet x y m = List.head (Maybe.withDefault [] (List.head m))

getRow : Int -> Matrix a -> Maybe (List a)
getRow y m = Just (Maybe.withDefault [] (List.head m))

toList : Matrix a -> List a
toList m = Maybe.withDefault [] (List.head m)