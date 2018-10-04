module Routing exposing (..)


import Url
import Url.Parser exposing (Parser, parse, int, map, oneOf, s, top)
import Url.Builder exposing (absolute)
import Browser

type Route
    = Prva
    | Druga
    | GOL
    -- | NotFoundRoute

routeFromUrlRequest : Browser.UrlRequest -> Route
routeFromUrlRequest ur = 
    case ur of
        Browser.Internal url -> routeLocation url
        Browser.External href -> stringToRoute href

stringToRoute : String -> Route
stringToRoute s = Prva

routeLocation : Url.Url -> Route
routeLocation =
    parseRoute >> routeFromResult


parseRoute : Url.Url -> Maybe Route
parseRoute =
    parse route


routeFromResult : Maybe Route -> Route
routeFromResult =
    Maybe.withDefault Druga


route : Parser (Route -> a) a
route =
    oneOf
        [ map GOL (s "gol")
        , map Prva (s "prva")
        , map Druga (s "druga")
        ]
