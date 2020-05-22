module Rank exposing (Rank(..), allRanks, caption, numberOfRanks, tally, toInt)

import Dice exposing (PipsList)


type Rank
    = Ones
    | Twos
    | Threes
    | Fours
    | Fives
    | Sixes
    | ThreeOfAKind
    | FourOfAKind
    | FullHouse
    | SmallStraight
    | LargeStraight
    | FiveOfAKind
    | Chance


allRanks : List Rank
allRanks =
    [ Ones
    , Twos
    , Threes
    , Fours
    , Fives
    , Sixes
    , ThreeOfAKind
    , FourOfAKind
    , FullHouse
    , SmallStraight
    , LargeStraight
    , FiveOfAKind
    , Chance
    ]


tally : Rank -> PipsList -> Int
tally =
    Debug.todo "tally"



{-
   goodness : Rank -> Int -> Order
   goodness =
       ...
-}


caption : Rank -> String
caption rank =
    case rank of
        Ones ->
            "Ones"

        Twos ->
            "Twos"

        Threes ->
            "Threes"

        Fours ->
            "Fours"

        Fives ->
            "Fives"

        Sixes ->
            "Sixes"

        ThreeOfAKind ->
            "3 of a kind"

        FourOfAKind ->
            "4 of a kind"

        FullHouse ->
            "Full House"

        SmallStraight ->
            "Sm Strght"

        LargeStraight ->
            "Lg Strght"

        FiveOfAKind ->
            "5 of a kind"

        Chance ->
            "Chance"


toInt : Rank -> Int
toInt rank =
    case rank of
        Ones ->
            0

        Twos ->
            1

        Threes ->
            2

        Fours ->
            3

        Fives ->
            4

        Sixes ->
            5

        ThreeOfAKind ->
            6

        FourOfAKind ->
            7

        FullHouse ->
            8

        SmallStraight ->
            9

        LargeStraight ->
            10

        FiveOfAKind ->
            11

        Chance ->
            12


numberOfRanks : Int
numberOfRanks =
    13
