module DieTests exposing (..)

import Dice exposing (DiceList, diceRoller)
import Die exposing (Die, NextRoll(..), flipNextRoll, makeDie, nextRoll, pips)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, intRange, list, string)
import Random
import Test exposing (..)


dieSuite : Test
dieSuite =
    describe "The Die type" <|
        [ test "Input values less than 1 become 1" <|
            \_ ->
                Die.fromInt 0
                    |> pips
                    |> Expect.equal 1
        , test "Input values greater than 6 become 6" <|
            \_ ->
                Die.fromInt 7
                    |> pips
                    |> Expect.equal 6
        , test "Die values 1 through 6 survive unchanged" <|
            \_ ->
                let
                    oneToSix =
                        List.range 1 6
                in
                List.map Die.fromInt oneToSix
                    |> List.map pips
                    |> Expect.equal oneToSix
        , test "makeDie clamps values like fromInt does" <|
            \_ ->
                makeDie ( 42, Die.Keep )
                    |> Expect.equal (Die.fromInt 42)
        , fuzz (intRange 0 7) "The url for a die with `n` pips contains '/die-`n`.`" <|
            \n ->
                let
                    d =
                        Die.fromInt n

                    fragment =
                        "/die-" ++ String.fromInt (pips d) ++ "."
                in
                Die.url d
                    |> String.contains fragment
                    |> Expect.true ("Expected to see " ++ fragment)
        ]


nextRollSuite : Test
nextRollSuite =
    describe "the nextRoll field" <|
        [ fuzz (intRange Random.minInt Random.maxInt) "A new die's nextRoll is Keep" <|
            \seed ->
                randomDie seed
                    |> List.map nextRoll
                    |> Expect.equalLists [ Keep ]
        , fuzz (intRange Random.minInt Random.maxInt) "flipNextRoll changes Keep to Reroll and vice-versa" <|
            \seed ->
                let
                    hasKeep =
                        randomDie seed

                    hasReroll =
                        List.map flipNextRoll hasKeep

                    alsoKeep =
                        List.map flipNextRoll hasReroll
                in
                List.map nextRoll (hasKeep ++ hasReroll ++ alsoKeep)
                    |> Expect.equalLists [ Keep, Reroll, Keep ]
        ]


randomDie : Int -> DiceList
randomDie seed =
    Random.step (diceRoller 1) (Random.initialSeed seed)
        |> Tuple.first
