module Rank exposing
    ( DiceToKeep(..)
    , PipsCounted(..)
    , Rank(..)
    , allRanks
    , caption
    , countPips
    , lowerRanks
    , numberOfRanks
    , numberOfUppers
    , suggestKeeping
    , tally
    , tallyPipsList
    , toInt
    , upperRanks
    )

import Array exposing (Array)
import Dice exposing (PipsList(..))


type PipsCounted
    = PipsCounted (Array Int)


type DiceToKeep
    = Straight (List Int)
    | OfAKind Int


suggestKeeping : PipsList -> List DiceToKeep
suggestKeeping pipsList =
    let
        counted =
            countPips pipsList

        pairOrBetter =
            case counted of
                PipsCounted kounted ->
                    kounted
                        |> Array.toList
                        |> List.indexedMap
                            (\pips count ->
                                if count >= 2 && count < 5 then
                                    pips

                                else
                                    0
                            )
                        |> List.filter (\n -> n > 0)
                        |> List.map OfAKind

        diceToKeep =
            case straightSuggestion counted of
                Just straight ->
                    straight :: pairOrBetter

                Nothing ->
                    pairOrBetter
    in
    List.reverse diceToKeep


straightSuggestion : PipsCounted -> Maybe DiceToKeep
straightSuggestion counted =
    let
        ( start, length ) =
            findRun counted
    in
    if length == 3 || length == 4 then
        Just (Straight (List.range start (start + length - 1)))

    else
        Nothing


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


findRun : PipsCounted -> ( Int, Int )
findRun (PipsCounted counted) =
    let
        find ( j, start, length ) list =
            case list of
                [] ->
                    ( j, start, length )

                present :: tail ->
                    if present > 0 then
                        find ( j + 1, start, length + 1 ) tail

                    else if length >= 3 then
                        -- There are only five dice, so there can be at most
                        -- one run of three or longer
                        ( j, start, length )

                    else
                        find ( j + 1, j + 1, 0 ) tail

        ( _, theStart, theLength ) =
            find ( 0, 0, 0 ) (Array.toList counted)
    in
    ( theStart, theLength )


longestStraight : PipsCounted -> Int
longestStraight counted =
    findRun counted |> Tuple.second


tallyPipsList : Rank -> PipsList -> Int
tallyPipsList rank pipsList =
    tally rank (countPips pipsList)


nWhen : Int -> Bool -> Int
nWhen n condition =
    if condition then
        n

    else
        0


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
                nWhen 25 (max == 5 || (max == 3 && hasPair counted))

        SmallStraight ->
            \counted -> nWhen 30 (longestStraight counted >= 4)

        LargeStraight ->
            \counted -> nWhen 40 (longestStraight counted == 5)

        FiveOfAKind ->
            \(PipsCounted counted) -> nWhen 50 (List.any (\n -> n == 5) (Array.toList counted))

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


numberOfUppers : Int
numberOfUppers =
    List.length upperRanks


numberOfRanks : Int
numberOfRanks =
    numberOfUppers + List.length lowerRanks


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
