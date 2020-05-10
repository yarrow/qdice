module Score exposing (scoreOrder)

import Array exposing (Array)
import Dice
import Dict exposing (Dict)
import ScoreTags exposing (..)


scoreOrder : List String
scoreOrder =
    [ ones, twos, threes, fours, fives, sixes, upperTotal, bonus, threeOfAKind, fourOfAKind, fullHouse, smallStraight, largeStraight, fiveOfAKind, chance, total, weighted, grandTotal ]
