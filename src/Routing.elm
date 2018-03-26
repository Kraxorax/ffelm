module Routing exposing (..)

import UrlParser exposing (..)
import Navigation


type Route
    = Prva
    | Druga Int
    | GOL


routeLocation : Navigation.Location -> Route
routeLocation =
    parseRoute >> routeFromResult


parseRoute : Navigation.Location -> Maybe Route
parseRoute =
    UrlParser.parseHash matchers


routeFromResult : Maybe Route -> Route
routeFromResult =
    Maybe.withDefault Prva


matchers : Parser (Route -> a) a
matchers =
    oneOf
        [ map GOL top
        , map Prva (s "prva")
        , map Druga (s "druga" </> int)
        , map (Druga 0) (s "druga")
        , map (\broj divlje -> Druga broj) (s "druga" </> int </> divlji)
        ]


divlji : Parser (String -> a) a
divlji =
    custom "DIVLJI" Ok
