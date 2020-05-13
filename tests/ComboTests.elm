module ComboTests exposing (..)

import Array exposing (Array)
import Combo exposing (..)
import Dice exposing (fiveDice, oneDie)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, intRange, list, string)
import Random
import Test exposing (..)


diceCount : List Int -> Array Int
diceCount dice =
    Dice.count (List.map oneDie dice)


diceFromSeed : Int -> ( List Dice.OneDie, Array Int )
diceFromSeed seed =
    let
        dice =
            Random.step fiveDice (Random.initialSeed seed) |> Tuple.first

        counted =
            Dice.count dice
    in
    ( dice, counted )


countTimesVal : List Dice.OneDie -> Int -> Int
countTimesVal dice val =
    val * List.length (List.filter (\n -> n == val) (List.map Dice.pips dice))


sumDice : List Dice.OneDie -> Int
sumDice dice =
    List.sum (List.map Dice.pips dice)


ofAKind : List Dice.OneDie -> Int
ofAKind dice =
    Maybe.withDefault 0 (List.maximum (Array.toList (Dice.count dice)))


comboSuite : Test
comboSuite =
    only <|
        describe "Test for Combo types" <|
            [ test "The combos have the right tags" <|
                \_ ->
                    let
                        combos =
                            List.map Tuple.first matches

                        tags =
                            List.map Tuple.second matches

                        matches =
                            [ ( Ones, "Ones" )
                            , ( Twos, "Twos" )
                            , ( Threes, "Threes" )
                            , ( Fours, "Fours" )
                            , ( Fives, "Fives" )
                            , ( Sixes, "Sixes" )
                            , ( ThreeOfAKind, "3 of a kind" )
                            , ( FourOfAKind, "4 of a kind" )
                            , ( FullHouse, "Full House" )
                            , ( SmallStraight, "Sm Straight" )
                            , ( LargeStraight, "Lg Straight" )
                            , ( FiveOfAKind, "5 of a kind" )
                            , ( Chance, "Chance" )
                            ]
                    in
                    List.map Combo.toString combos
                        |> Expect.equalLists tags
            , test "Ones counts 1s" <|
                \_ ->
                    Combo.scoreFn Ones (diceCount [ 1, 3, 4, 1, 2 ])
                        |> Expect.equal 2
            , test "Twos counts 2s" <|
                \_ ->
                    Combo.scoreFn Twos (diceCount [ 2, 2, 4, 1, 2 ])
                        |> Expect.equal 6
            , fuzz (intRange Random.minInt Random.maxInt) "An upper score for N is N times the count of dice with N pips" <|
                \seed ->
                    let
                        ( dice, counted ) =
                            diceFromSeed seed

                        expected =
                            List.map (countTimesVal dice) [ 1, 2, 3, 4, 5, 6 ]
                    in
                    List.map (\combo -> scoreFn combo counted) [ Ones, Twos, Threes, Fours, Fives, Sixes ]
                        |> Expect.equal expected
            , fuzz (intRange Random.minInt Random.maxInt) "Chance score is always the sum of the dice" <|
                \seed ->
                    let
                        ( dice, counted ) =
                            diceFromSeed seed
                    in
                    scoreFn Chance counted
                        |> Expect.equal (sumDice dice)
            , test "5 of a kind is 50, if all dice are the same" <|
                \_ ->
                    let
                        yatz =
                            List.map (\n -> diceCount (List.repeat 5 n)) [ 1, 2, 3, 4, 5, 6 ]
                    in
                    List.map (scoreFn FiveOfAKind) yatz
                        |> Expect.equalLists (List.repeat 6 50)
            , test "4 of a kind score of [4,4,4,1,4] is 17" <|
                \_ ->
                    scoreFn FourOfAKind (diceCount [ 4, 4, 4, 1, 4 ])
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
                    scoreFn FourOfAKind counted
                        |> Expect.equal expectedScore
            ]
