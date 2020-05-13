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
valueTimesCount value counted =
    value * Maybe.withDefault 0 (Array.get value counted)


sumDice : Array Int -> Int
sumDice count =
    Array.foldl (+) 0 (Array.indexedMap (*) count)


ofAKind : Array Int -> Int
ofAKind counted =
    Maybe.withDefault 0 (List.maximum (Array.toList counted))


ifAtLeast : Int -> Array Int -> Int
ifAtLeast min counted =
    if min <= ofAKind counted then
        sumDice counted

    else
        0


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
            ifAtLeast 3

        FourOfAKind ->
            ifAtLeast 4

        FullHouse ->
            \counted ->
                let
                    max =
                        ofAKind counted
                in
                if max == 5 || (max == 3 && List.any (\count -> count == 2) (Array.toList counted)) then
                    25

                else
                    0

        SmallStraight ->
            meh

        LargeStraight ->
            meh

        FiveOfAKind ->
            \counted ->
                case List.any (\n -> n == 5) (Array.toList counted) of
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
