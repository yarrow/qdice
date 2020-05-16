module Score exposing (BoxLocation, Display, DisplayBox, DisplayRow, Pad, active, inert, initialPad, rowOrder)

import Array exposing (Array)
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


type alias Tag =
    String


type alias ScoreRow =
    Array Int


type Pad
    = Pad (Dict String ScoreRow)


upperTags : List Tag
upperTags =
    [ ones, twos, threes, fours, fives, sixes ]


lowerTags : List Tag
lowerTags =
    [ threeOfAKind, fourOfAKind, fullHouse, smallStraight, largeStraight, fiveOfAKind, chance ]


activeTags : List Tag
activeTags =
    upperTags ++ lowerTags


initialPad : Pad
initialPad =
    let
        zeros =
            Array.repeat 3 0
    in
    Pad (Dict.fromList (List.map (\tag -> ( tag, zeros )) activeTags))


inertRow : Tag -> DisplayRow
inertRow tag =
    { tag = tag, boxes = List.repeat 3 { text = "", onClick = Nothing } }


inert : Pad -> Display
inert _ =
    List.map inertRow rowOrder


activeRow : Tag -> DisplayRow
activeRow tag =
    { tag = tag, boxes = List.repeat 3 { text = "", onClick = Just "peachy" } }


active : Pad -> Display
active _ =
    List.concat
        [ List.map activeRow upperTags
        , List.map inertRow [ upperTotal, bonus ]
        , List.map activeRow lowerTags
        , List.map inertRow [ total, weighted, grandTotal ]
        ]
