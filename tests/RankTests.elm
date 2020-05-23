module RankTests exposing (rankTests)

import Array exposing (Array)
import Dice exposing (PipsList(..))
import DiceBoard
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, intRange, list, string)
import Random
import Rank exposing (PipsCounted(..), Rank(..), tally)
import Test exposing (..)


diceCount : List Int -> PipsCounted
diceCount list =
    Rank.countPips (PipsList list)


diceFromSeed : Int -> ( PipsList, Rank.PipsCounted )
diceFromSeed seed =
    let
        dice =
            Random.step (DiceBoard.rollForNewDice Nothing) (Random.initialSeed seed)
                |> Tuple.first

        counted =
            Rank.countPips dice
    in
    ( dice, counted )


sumDice : PipsList -> Int
sumDice (PipsList dice) =
    List.sum dice


ofAKind : PipsList -> Int
ofAKind dice =
    let
        counted =
            (\(PipsCounted x) -> x) (Rank.countPips dice)
    in
    Maybe.withDefault 0 (List.maximum (Array.toList counted))


rankTests : Test
rankTests =
    describe "Test for Rank types" <|
        [ test "Ones counts 1s" <|
            \_ ->
                tally Ones (diceCount [ 1, 3, 4, 1, 2 ])
                    |> Expect.equal 2
        , test "Twos counts 2s" <|
            \_ ->
                tally Twos (diceCount [ 2, 2, 4, 1, 2 ])
                    |> Expect.equal 6
        , fuzz (intRange Random.minInt Random.maxInt) "An upper score for N is N times the count of dice with N pips" <|
            \seed ->
                let
                    ( dice, counted ) =
                        diceFromSeed seed

                    countTimesVal (PipsList pipsList) val =
                        val * List.length (List.filter (\n -> n == val) pipsList)

                    expected =
                        List.map (countTimesVal dice) [ 1, 2, 3, 4, 5, 6 ]
                in
                List.map (\combo -> tally combo counted) [ Ones, Twos, Threes, Fours, Fives, Sixes ]
                    |> Expect.equal expected
        , fuzz (intRange Random.minInt Random.maxInt) "Chance score is always the sum of the dice" <|
            \seed ->
                let
                    ( dice, counted ) =
                        diceFromSeed seed
                in
                tally Chance counted
                    |> Expect.equal (sumDice dice)
        , test "5 of a kind is 50, if all dice are the same" <|
            \_ ->
                let
                    yatz =
                        List.map (\n -> diceCount (List.repeat 5 n)) [ 1, 2, 3, 4, 5, 6 ]
                in
                List.map (tally FiveOfAKind) yatz
                    |> Expect.equalLists (List.repeat 6 50)
        , test "4 of a kind score of [4,4,4,1,4] is 17" <|
            \_ ->
                tally FourOfAKind (diceCount [ 4, 4, 4, 1, 4 ])
                    |> Expect.equal 17
        , test "3 of a kind score of [4,4,4,1,4] is also 17" <|
            \_ ->
                tally ThreeOfAKind (diceCount [ 4, 4, 4, 1, 4 ])
                    |> Expect.equal 17
        , fuzz (intRange Random.minInt Random.maxInt) "4 of a kind is the sum of the dice, if there are at least 4 the same" <|
            \seed ->
                let
                    ( dice, counted ) =
                        diceFromSeed seed

                    expectedScore =
                        if ofAKind dice >= 4 then
                            sumDice dice

                        else
                            0
                in
                tally FourOfAKind counted
                    |> Expect.equal expectedScore
        , test "Full House scores 25 for 3 of a kind with a pair, and for 5 of a kind" <|
            \_ ->
                let
                    score plain =
                        tally FullHouse (diceCount plain)

                    scoresFound =
                        List.map score [ [ 6, 6, 6, 6, 6 ], [ 4, 5, 4, 5, 4 ], [ 1, 1, 2, 2, 3 ] ]

                    expectedScores =
                        [ 25, 25, 0 ]
                in
                scoresFound |> Expect.equalLists expectedScores
        , test "LargeStraight is permutations of [1,2,3,4,5] or [2,3,4,5,6]" <|
            \_ ->
                let
                    good =
                        [ [ 1, 2, 3, 4, 5 ], [ 2, 3, 4, 5, 6 ], [ 1, 3, 5, 2, 4 ], [ 6, 4, 2, 3, 5 ] ]

                    bad =
                        [ [ 2, 2, 3, 4, 5 ], [ 2, 4, 4, 5, 6 ], [ 1, 4, 5, 2, 4 ], [ 6, 4, 6, 3, 5 ] ]

                    expectedScores =
                        List.repeat 4 40 ++ List.repeat 4 0

                    score plain =
                        tally LargeStraight (diceCount plain)

                    scoresFound =
                        List.map score (good ++ bad)
                in
                scoresFound |> Expect.equalLists expectedScores
        , test "SmallStraight is a large straight or permutations of [1,2,3,4, x] or [2,3,4,5,x] or [3,4,5,6,x]" <|
            \_ ->
                let
                    good =
                        [ [ 1, 2, 3, 4, 5 ]
                        , [ 2, 3, 4, 5, 6 ]
                        , [ 1, 3, 5, 2, 4 ]
                        , [ 6, 4, 2, 3, 5 ]
                        , [ 1, 2, 3, 4, 4 ]
                        , [ 2, 3, 4, 5, 2 ]
                        , [ 3, 4, 5, 6, 3 ]
                        , [ 6, 4, 6, 3, 5 ]
                        ]

                    bad =
                        [ [ 1, 2, 4, 5, 6 ], [ 2, 4, 4, 5, 6 ], [ 1, 4, 5, 2, 4 ], [ 6, 4, 2, 3, 6 ] ]

                    expectedScores =
                        List.repeat 8 30 ++ List.repeat 4 0

                    score plain =
                        tally SmallStraight (diceCount plain)

                    scoresFound =
                        List.map score (good ++ bad)
                in
                scoresFound |> Expect.equalLists expectedScores
        ]
