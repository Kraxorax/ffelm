module Util exposing (makeId)


makeId: String -> Int -> String
makeId s i = s ++ "_" ++ String.fromInt i
