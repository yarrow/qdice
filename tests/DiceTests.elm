module DiceTests exposing (..)

import Array exposing (Array)
import Dice exposing (NextRoll(..), OneDie, diceRoller, fiveDice, flipNextRoll, flipNth, makeDice, makeDie, nextRoll, oneDie, pips)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, intRange, list, string)
import Random
import Test exposing (..)


oneDieSuite : Test
oneDieSuite =
    describe "The OneDie type" <|
        [ test "Input values less than 1 become 1" <|
            \_ ->
                oneDie 0
                    |> pips
                    |> Expect.equal 1
        , test "Input values greater than 6 become 6" <|
            \_ ->
                oneDie 7
                    |> pips
                    |> Expect.equal 6
        , test "Die values 1 through 6 survive unchanged" <|
            \_ ->
                let
                    oneToSix =
                        List.range 1 6
                in
                List.map oneDie oneToSix
                    |> List.map pips
                    |> Expect.equal oneToSix
        , fuzz (intRange 0 7) "The url for a die with `n` pips contains '/die-`n`.`" <|
            \n ->
                let
                    d =
                        oneDie n

                    fragment =
                        "/die-" ++ String.fromInt (pips d) ++ "."
                in
                Dice.url d
                    |> String.contains fragment
                    |> Expect.true ("Expected to see " ++ fragment)
        ]


countSuite : Test
countSuite =
    describe "(Dice.count dice) returns an array where slot j holds the number of dies in dice with j pips" <|
        [ test "12345" <|
            \_ ->
                -- dice never have zero pips, and this set has no 6s
                Dice.count (List.map oneDie [ 1, 2, 3, 4, 5 ])
                    |> Array.toList
                    |> Expect.equalLists [ 0, 1, 1, 1, 1, 1, 0 ]
        , test "More of a kind" <|
            \_ ->
                Dice.count (List.map oneDie [ 3, 6, 5, 6, 3 ])
                    |> Array.toList
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


flipNthSuite : Test
flipNthSuite =
    describe "flipNth n dice performs flipNextRoll on the nth element of dice" <|
        [ test "flipNth 0" <|
            \_ ->
                flipNth 0 (makeDice [ ( 1, Keep ), ( 2, Reroll ) ])
                    |> Expect.equalLists (makeDice [ ( 1, Reroll ), ( 2, Reroll ) ])
        ]
