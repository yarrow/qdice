module Combo exposing (Combo(..), scoreFn, toString)

import Array exposing (Array)
import CountedDice exposing (CountedDice(..))


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


valueTimesCount : Int -> CountedDice -> Int
valueTimesCount value (CountedDice counted) =
    value * Maybe.withDefault 0 (Array.get value counted)


sumDice : CountedDice -> Int
sumDice (CountedDice count) =
    Array.foldl (+) 0 (Array.indexedMap (*) count)


ofAKind : CountedDice -> Int
ofAKind counted =
    Maybe.withDefault 0 (List.maximum (CountedDice.toList counted))


sumDiceIfAtLeast : Int -> CountedDice -> Int
sumDiceIfAtLeast min counted =
    if min <= ofAKind counted then
        sumDice counted

    else
        0


leftTrimZeros : List Int -> List Int
leftTrimZeros list =
    case list of
        0 :: tail ->
            leftTrimZeros tail

        _ ->
            list


runCount : Int -> List Int -> Int
runCount n list =
    case list of
        [] ->
            n

        0 :: tail ->
            n

        _ :: tail ->
            runCount (n + 1) tail


longestStraight : CountedDice -> Int
longestStraight (CountedDice counted) =
    Array.toList counted |> leftTrimZeros |> runCount 0


scoreFn : Combo -> (CountedDice -> Int)
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
            sumDiceIfAtLeast 3

        FourOfAKind ->
            sumDiceIfAtLeast 4

        FullHouse ->
            \counted ->
                let
                    max =
                        ofAKind counted
                in
                if max == 5 || (max == 3 && List.any (\count -> count == 2) (CountedDice.toList counted)) then
                    25

                else
                    0

        SmallStraight ->
            \counted ->
                if longestStraight counted >= 4 then
                    30

                else
                    0

        LargeStraight ->
            \counted ->
                if longestStraight counted == 5 then
                    40

                else
                    0

        FiveOfAKind ->
            \counted ->
                case List.any (\n -> n == 5) (CountedDice.toList counted) of
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
