module DiceTests exposing (..)

import Array exposing (Array)
import CountedDice exposing (CountedDice(..))
import Dice exposing (diceRoller, fiveDice, flipNth, makeDice)
import Die exposing (NextRoll(..), OneDie, flipNextRoll, makeDie, nextRoll, oneDie, pips)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, intRange, list, string)
import Random
import Test exposing (..)


countSuite : Test
countSuite =
    describe "(CountedDice.fromDice dice) returns an array where slot j holds the number of dies in dice with j pips" <|
        [ test "12345" <|
            \_ ->
                -- dice never have zero pips, and this set has no 6s
                CountedDice.fromDice (List.map oneDie [ 1, 2, 3, 4, 5 ])
                    |> CountedDice.toList
                    |> Expect.equalLists [ 0, 1, 1, 1, 1, 1, 0 ]
        , test "More of a kind" <|
            \_ ->
                CountedDice.fromDice (List.map oneDie [ 3, 6, 5, 6, 3 ])
                    |> CountedDice.toList
                    |> Expect.equalLists [ 0, 0, 0, 2, 0, 1, 2 ]
        ]


diceRollerSuite : Test
diceRollerSuite =
    describe "The value of `diceRoller n` is a random generator returning a list of `n` random dice" <|
        [ test "diceRoller 0 is a generator that always returns an empty list" <|
            \_ ->
                Random.step (diceRoller 0) (Random.initialSeed 42)
                    |> Tuple.first
                    |> List.map pips
                    |> Expect.equalLists []
        , test "diceRoller 1 returns a list with one random die" <|
            \_ ->
                Random.step (diceRoller 1) (Random.initialSeed 42)
                    |> Tuple.first
                    |> List.map pips
                    |> Expect.equalLists [ 6 ]
        , test "diceRoller 5 returns a list with five random dice" <|
            \_ ->
                Random.step (diceRoller 5) (Random.initialSeed 42)
                    |> Tuple.first
                    |> List.map pips
                    |> Expect.equalLists [ 1, 3, 1, 1, 6 ]
        , fuzz (intRange Random.minInt Random.maxInt) "fiveDice always returns five dice" <|
            \seed ->
                Random.step fiveDice (Random.initialSeed seed)
                    |> Tuple.first
                    |> List.length
                    |> Expect.equal 5
        ]


randomDie : Int -> List OneDie
randomDie seed =
    Random.step (diceRoller 1) (Random.initialSeed seed)
        |> Tuple.first


flipNthSuite : Test
flipNthSuite =
    describe "flipNth n dice performs flipNextRoll on the nth element of dice" <|
        [ test "flipNth 0" <|
            \_ ->
                flipNth 0 (makeDice [ ( 1, Keep ), ( 2, Reroll ) ])
                    |> Expect.equalLists (makeDice [ ( 1, Reroll ), ( 2, Reroll ) ])
        ]
