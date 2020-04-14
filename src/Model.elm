module Model exposing (Model)

import Routing exposing (Route)
import Browser.Navigation exposing (Key)
import Pages.Forma.Forma as Forma
import Pages.Forma.Dugme exposing (Dugme)
import Pages.Klokotalo.Klok as Klok
import Pages.Klokotalo.Klokotalo as Klokotalo
import Pages.GejmOfLajf as GejmOfLajf
import Pages.Randomer as Randomer


type alias Model =
    { navKey : Key 
    , dugmici : List Dugme
    , naziv : Maybe String
    , route : Route
    , clock : Float
    , counter : Float
    , formica : Forma.Model
    , klok : Klok.Klok
    , klokotalo : Klokotalo.Model
    , gol : GejmOfLajf.Model
    , randomer : Randomer.Model
    }
