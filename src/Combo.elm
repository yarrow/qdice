module Combo exposing (Combo(..), scoreFn, toString)

import Array exposing (Array)


type Combo
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


toString : Combo -> String
toString combo =
    case combo of
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
            "Sm Straight"

        LargeStraight ->
            "Lg Straight"

        FiveOfAKind ->
            "5 of a kind"

        Chance ->
            "Chance"


meh : Array Int -> Int
meh _ =
    0


valueTimesCount : Int -> Array Int -> Int
valueTimesCount value counter =
    value * Maybe.withDefault 0 (Array.get value counter)


sumDice : Array Int -> Int
sumDice count =
    Array.foldl (+) 0 (Array.indexedMap (*) count)


atLeast : Int -> Array Int -> Bool
atLeast min counted =
    List.any (\n -> n >= min) (Array.toList counted)


scoreFn : Combo -> (Array Int -> Int)
scoreFn combo =
    case combo of
        Ones ->
            valueTimesCount 1

        Twos ->
            valueTimesCount 2

        Threes ->
            valueTimesCount 3

        Fours ->
            valueTimesCount 4

        Fives ->
            valueTimesCount 5

        Sixes ->
            valueTimesCount 6

        ThreeOfAKind ->
            meh

        FourOfAKind ->
            \counted ->
                if atLeast 4 counted then
                    sumDice counted

                else
                    0

        FullHouse ->
            meh

        SmallStraight ->
            meh

        LargeStraight ->
            meh

        FiveOfAKind ->
            \counter ->
                case List.any (\n -> n == 5) (Array.toList counter) of
                    True ->
                        50

                    False ->
                        0

        Chance ->
            sumDice


toInt : Combo -> Int
toInt combo =
    case combo of
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
