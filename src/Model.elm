module Model exposing (Model)

import Dugme exposing (Dugme)
import Forma
import Klok
import Klokotalo
import Routing exposing (Route)
import Browser.Navigation exposing (Key)
import GejmOfLajf


type alias Model =
    { navKey : Key 
    , dugmici : List Dugme
    , naziv : Maybe String
    , formica : Forma.Model
    , klok : Klok.Klok
    , klokotalo : Klokotalo.Model
    , route : Route
    , clock : Float
    , counter : Float
    , gol : GejmOfLajf.Model
    }
