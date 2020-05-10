module Score exposing (BoxLocation, Display, DisplayBox, DisplayRow, Pad, active, inert, initialPad, rowOrder)

import Array exposing (Array)
import Dice
import Dict exposing (Dict)
import ScoreTags exposing (..)


rowOrder : List String
rowOrder =
    [ ones, twos, threes, fours, fives, sixes, upperTotal, bonus, threeOfAKind, fourOfAKind, fullHouse, smallStraight, largeStraight, fiveOfAKind, chance, total, weighted, grandTotal ]


type alias Display =
    List DisplayRow


type alias DisplayRow =
    { tag : String
    , boxes : List DisplayBox
    }


type alias BoxLocation =
    String


type alias DisplayBox =
    { text : String
    , onClick : Maybe BoxLocation
    }


type Pad
    = Pad


initialPad : Pad
initialPad =
    Pad


inert : Pad -> Display
inert _ =
    List.map (\tag -> { tag = tag, boxes = List.repeat 3 { text = "", onClick = Nothing } }) rowOrder


active : Pad -> Display
active _ =
    List.map (\tag -> { tag = tag, boxes = List.repeat 3 { text = "", onClick = Nothing } }) rowOrder
