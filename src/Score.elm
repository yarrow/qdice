module Score exposing (DisplayBox, DisplayRow, ScoreDisplay, ScorePad, inert, initialScorePad, scoreOrder)

import Array exposing (Array)
import Dice
import Dict exposing (Dict)
import ScoreTags exposing (..)


scoreOrder : List String
scoreOrder =
    [ ones, twos, threes, fours, fives, sixes, upperTotal, bonus, threeOfAKind, fourOfAKind, fullHouse, smallStraight, largeStraight, fiveOfAKind, chance, total, weighted, grandTotal ]


type alias ScoreDisplay =
    List DisplayRow


type alias DisplayRow =
    { tag : String
    , boxes : List DisplayBox
    }


type alias ScoreIndex =
    String


type alias DisplayBox =
    { text : String
    , onClick : Maybe ScoreIndex
    }


type ScorePad
    = ScorePad


initialScorePad : ScorePad
initialScorePad =
    ScorePad


inert : ScorePad -> ScoreDisplay
inert pad =
    []
