module Util exposing (..)

type alias CompId = { id: String }

makeId: String -> Int -> String
makeId s i = s ++ "_" ++ toString i
