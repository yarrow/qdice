module DiceTests exposing (..)

import Dice exposing (diceRoller, oneDie, val)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Random
import Test exposing (..)


oneDieSuite : Test
oneDieSuite =
    describe "The OneDie type" <|
        [ test "Input values less than 1 become 1" <|
            \_ ->
                oneDie 0
                    |> val
                    |> Expect.equal 1
        , test "Input values greater than 6 become 6" <|
            \_ ->
                oneDie 7
                    |> val
                    |> Expect.equal 6
        , test "Die values 1 through 6 survive unchanged" <|
            \_ ->
                let
                    oneToSix =
                        List.range 1 6
                in
                List.map oneDie oneToSix
                    |> List.map val
                    |> Expect.equal oneToSix
        ]


diceRollerSuite : Test
diceRollerSuite =
    describe "The value of `diceRoller n` is a random generator returning a list of `n` random dice" <|
        [ test "diceRoller 0 is a generator that always returns an empty list" <|
            \_ ->
                Random.step (diceRoller 0) (Random.initialSeed 42)
                    |> Tuple.first
                    |> List.map val
                    |> Expect.equalLists []
        , test "diceRoller 1 returns a list with one random die" <|
            \_ ->
                Random.step (diceRoller 1) (Random.initialSeed 42)
                    |> Tuple.first
                    |> List.map val
                    |> Expect.equalLists [ 6 ]
        , test "diceRoller 5 returns a list with five random dice" <|
            \_ ->
                Random.step (diceRoller 5) (Random.initialSeed 42)
                    |> Tuple.first
                    |> List.map val
                    |> Expect.equalLists [ 1, 3, 1, 1, 6 ]
        ]
