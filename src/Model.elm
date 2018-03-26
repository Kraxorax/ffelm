module Model exposing (Model)

import Dugme exposing (Dugme)
import Forma
import Klok
import Klokotalo
import Routing exposing (Route)
import GejmOfLajf


type alias Model =
    { dugmici : List Dugme
    , naziv : Maybe String
    , formica : Forma.Model
    , klok : Klok.Klok
    , klokotalo : Klokotalo.Model
    , route : Route
    , clock : Float
    , counter : Float
    , gol : GejmOfLajf.Model
    }
