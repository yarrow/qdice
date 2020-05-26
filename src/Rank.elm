module Rank exposing
    ( PipsCounted(..)
    , Rank(..)
    , allRanks
    , caption
    , countPips
    , lowerRanks
    , numberOfRanks
    , tally
    , tallyPipsList
    , toInt
    , upperRanks
    )

import Array exposing (Array)
import Dice exposing (PipsList(..))


type PipsCounted
    = PipsCounted (Array Int)


countPips : PipsList -> PipsCounted
countPips (PipsList dice) =
    let
        increment jth counter =
            case Array.get jth counter of
                Just old ->
                    Array.set jth (old + 1) counter

                Nothing ->
                    counter
    in
    PipsCounted (List.foldr increment (Array.repeat 7 0) dice)


valueTimesCount : Int -> PipsCounted -> Int
valueTimesCount value (PipsCounted counted) =
    value * Maybe.withDefault 0 (Array.get value counted)


sumDice : PipsCounted -> Int
sumDice (PipsCounted count) =
    Array.foldl (+) 0 (Array.indexedMap (*) count)


ofAKind : PipsCounted -> Int
ofAKind (PipsCounted counted) =
    Maybe.withDefault 0 (List.maximum (Array.toList counted))


sumDiceIfAtLeast : Int -> PipsCounted -> Int
sumDiceIfAtLeast min counted =
    if min <= ofAKind counted then
        sumDice counted

    else
        0


longestStraight : PipsCounted -> Int
longestStraight (PipsCounted counted) =
    let
        longestRun : ( Int, Int ) -> List Int -> ( Int, Int )
        longestRun ( thisRun, longestPreviousRun ) list =
            case list of
                [] ->
                    ( 0, max thisRun longestPreviousRun )

                head :: tail ->
                    case head of
                        0 ->
                            longestRun ( 0, max thisRun longestPreviousRun ) tail

                        _ ->
                            longestRun ( thisRun + 1, longestPreviousRun ) tail
    in
    longestRun ( 0, 0 ) (Array.toList counted) |> Tuple.second


tallyPipsList : Rank -> PipsList -> Int
tallyPipsList rank pipsList =
    tally rank (countPips pipsList)


tally : Rank -> PipsCounted -> Int
tally rank =
    case rank of
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

                    hasPair (PipsCounted kounted) =
                        List.any (\count -> count == 2) (Array.toList kounted)
                in
                if max == 5 || (max == 3 && hasPair counted) then
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
            \(PipsCounted counted) ->
                if List.any (\n -> n == 5) (Array.toList counted) then
                    50

                else
                    0

        Chance ->
            sumDice


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


upperRanks : List Rank
upperRanks =
    [ Ones, Twos, Threes, Fours, Fives, Sixes ]


lowerRanks : List Rank
lowerRanks =
    [ ThreeOfAKind, FourOfAKind, FullHouse, SmallStraight, LargeStraight, FiveOfAKind, Chance ]


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
