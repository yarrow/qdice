module RankTests exposing (rankTests)

import Array
import DiceBoard
import Dict exposing (Dict)
import Expect
import Fuzz exposing (intRange)
import Pip exposing (Pip)
import Random
import Rank exposing (DiceToKeep(..), PipsCounted, Rank(..), Rating(..), rankInfo)
import Test exposing (..)


diceCount : List Int -> PipsCounted
diceCount list =
    Rank.countPips (Pip.mapFromInt list)


diceFromSeed : Int -> ( List Pip, Rank.PipsCounted )
diceFromSeed seed =
    let
        dice =
            Random.step (DiceBoard.rollForNewDice DiceBoard.empty) (Random.initialSeed seed)
                |> Tuple.first

        counted =
            Rank.countPips dice
    in
    ( dice, counted )


sumDice : List Pip -> Int
sumDice pips =
    List.sum (Pip.mapToInt pips)


ofAKind : List Pip -> Int
ofAKind pips =
    let
        counted =
            Rank.countPips pips
    in
    Maybe.withDefault 0 (List.maximum (Array.toList counted))


fnDict : Dict String (PipsCounted -> Int)
fnDict =
    Dict.fromList (List.map (\{ caption, fn } -> ( caption, fn )) rankInfo)


tally : String -> PipsCounted -> Int
tally name dice =
    case Dict.get name fnDict of
        Nothing ->
            -4321

        Just fn ->
            fn dice


tallyIntList : String -> List Int -> Int
tallyIntList name dice =
    tally name (Rank.countPips (Pip.mapFromInt dice))


rankTests : Test
rankTests =
    describe "Test for Rank types" <|
        [ test "Ones counts 1s" <|
            \_ ->
                tallyIntList "Ones" [ 1, 3, 4, 1, 2 ]
                    |> Expect.equal 2
        , test "Twos counts 2s" <|
            \_ ->
                tallyIntList "Twos" [ 2, 2, 4, 1, 2 ]
                    |> Expect.equal 6
        , fuzz (intRange Random.minInt Random.maxInt) "An upper score for N is N times the count of dice with N pips" <|
            \seed ->
                let
                    ( dice, counted ) =
                        diceFromSeed seed

                    countTimesVal pips val =
                        let
                            faces =
                                Pip.mapToInt pips
                        in
                        val * List.length (List.filter (\n -> n == val) faces)

                    expected =
                        List.map (countTimesVal dice) [ 1, 2, 3, 4, 5, 6 ]
                in
                List.map (\combo -> tally combo counted)
                    [ "Ones", "Twos", "Threes", "Fours", "Fives", "Sixes" ]
                    |> Expect.equal expected
        , fuzz (intRange Random.minInt Random.maxInt) "Chance score is always the sum of the dice" <|
            \seed ->
                let
                    ( dice, counted ) =
                        diceFromSeed seed
                in
                tally "Chance" counted
                    |> Expect.equal (sumDice dice)
        , test "5 of a kind is 50, if all dice are the same" <|
            \_ ->
                let
                    yatz =
                        List.map (\n -> diceCount (List.repeat 5 n)) [ 1, 2, 3, 4, 5, 6 ]
                in
                List.map (tally "5 of a kind") yatz
                    |> Expect.equalLists (List.repeat 6 50)
        , test "4 of a kind score of [4,4,4,1,4] is 17" <|
            \_ ->
                tally "4 of a kind" (diceCount [ 4, 4, 4, 1, 4 ])
                    |> Expect.equal 17
        , test "3 of a kind score of [4,4,4,1,4] is also 17" <|
            \_ ->
                tally "3 of a kind" (diceCount [ 4, 4, 4, 1, 4 ])
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
                tally "4 of a kind" counted
                    |> Expect.equal expectedScore
        , test "Full House scores 25 for 3 of a kind with a pair, and for 5 of a kind" <|
            \_ ->
                let
                    score plain =
                        tally "Full House" (diceCount plain)

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
                        tally "Lg Strght" (diceCount plain)

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
                        , [ 3, 1, 5, 4, 6 ]
                        ]

                    bad =
                        [ [ 1, 2, 4, 5, 6 ], [ 2, 4, 4, 5, 6 ], [ 1, 4, 5, 2, 4 ], [ 6, 4, 2, 3, 6 ] ]

                    expectedScores =
                        List.repeat 9 30 ++ List.repeat 4 0

                    score plain =
                        tally "Sm Strght" (diceCount plain)

                    scoresFound =
                        List.map score (good ++ bad)
                in
                scoresFound |> Expect.equalLists expectedScores
        , describe "suggestKeeping tests" <|
            let
                suggestion list =
                    Rank.suggestKeeping (Just (Pip.mapFromInt list))
            in
            [ test "suggestKeeping (Pip.mapFromInt [1,2,4,3,4]) == [OfAKind 4, Straight [1,2,3,4]]" <|
                \_ ->
                    suggestion [ 1, 2, 4, 3, 4 ]
                        |> Expect.equalLists [ OfAKind 4, Straight [ 1, 2, 3, 4 ] ]
            , test "suggestKeeping (Pip.mapFromInt [1,1,2,6,6]) == [OfAKind 6, OfAKind 1]" <|
                \_ ->
                    suggestion [ 1, 1, 2, 6, 6 ]
                        |> Expect.equalLists [ OfAKind 6, OfAKind 1 ]
            , test "suggestKeeping (Pip.mapFromInt [5,5,5,5,5]) == []" <|
                \_ ->
                    suggestion [ 5, 5, 5, 5, 5 ]
                        |> Expect.equalLists []
            , test "suggestKeeping (Pip.mapFromInt [1,2,3,4,5]) == []" <|
                \_ ->
                    suggestion [ 1, 2, 3, 4, 5 ]
                        |> Expect.equalLists []
            , test "suggestKeeping (Pip.mapFromInt [1,6,1,6,6]) == [OfAKind 6, OfAKind 1]" <|
                \_ ->
                    suggestion [ 1, 6, 1, 6, 6 ]
                        |> Expect.equalLists [ OfAKind 6, OfAKind 1 ]
            ]
        , describe "rating" <|
            let
                upperPar =
                    [ 3, 6, 9, 12, 15, 18 ]

                lowerPar =
                    List.repeat (List.length (Rank.lower rankInfo)) 1

                par =
                    upperPar ++ lowerPar

                subPar =
                    List.map (\n -> n - 1) par

                overPar =
                    List.map (\n -> n + 1) par

                repeat =
                    List.repeat Rank.numberOfRanks

                ( meager, sufficient, ample ) =
                    ( repeat Meager, repeat Sufficient, repeat Ample )
            in
            [ test "Sufficient" <|
                \_ ->
                    List.map2 (\r x -> r.rating x) rankInfo par
                        |> Expect.equalLists sufficient
            , test "Meager" <|
                \_ ->
                    List.map2 (\r x -> r.rating x) rankInfo subPar
                        |> Expect.equalLists meager
            , test "Upper over par is Ample, lower merely Sufficient" <|
                \_ ->
                    List.map2 (\r x -> r.rating x) rankInfo overPar
                        |> Expect.equal (Rank.upper ample ++ Rank.lower sufficient)
            ]
        , test "columnPar" <|
            \_ ->
                Rank.columnPar [ Just 1, Nothing, Just 4, Just 8, Nothing, Just 32 ]
                    |> Expect.equal (3 + 9 + 12 + 18)
        ]
