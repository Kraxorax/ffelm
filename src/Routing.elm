module Routing exposing (routeFromUrlRequest, Route(..), routeLocation)


import Url
import Url.Parser exposing (Parser, parse, map, oneOf, s)
import Browser

type Route
    = Forma
    | Klokotala
    | GOL
    | Randomer

routeFromUrlRequest : Browser.UrlRequest -> Route
routeFromUrlRequest ur = 
    case ur of
        Browser.Internal url -> routeLocation url
        Browser.External href -> stringToRoute href

stringToRoute : String -> Route
stringToRoute _ = GOL

routeLocation : Url.Url -> Route
routeLocation =
    parseRoute >> routeFromResult


parseRoute : Url.Url -> Maybe Route
parseRoute =
    parse route


routeFromResult : Maybe Route -> Route
routeFromResult =
    Maybe.withDefault Randomer


route : Parser (Route -> a) a
route =
    oneOf
        [ map GOL (s "gol")
        , map Randomer (s "randomer")
        , map Forma (s "forma")
        , map Klokotala (s "klokotala")
        ]
