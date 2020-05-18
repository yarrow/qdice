module DiceTests exposing (..)

import Dice exposing (DiceBoard(..), diceRoller, fiveDice, flipNth, makeDiceList)
import Die exposing (NextRoll(..))
import Expect
import Fuzz exposing (intRange)
import Random
import Test exposing (..)


diceRollerSuite : Test
diceRollerSuite =
    describe "The value of `diceRoller n` is a random generator returning a list of `n` random dice" <|
        [ test "diceRoller 0 is a generator that always returns an empty list" <|
            \_ ->
                Random.step (diceRoller 0) (Random.initialSeed 42)
                    |> Tuple.first
                    |> Expect.equalLists []
        , test "diceRoller 1 returns a list with one random die" <|
            \_ ->
                Random.step (diceRoller 1) (Random.initialSeed 42)
                    |> Tuple.first
                    |> Expect.equalLists [ 6 ]
        , test "diceRoller 5 returns a list with five random dice" <|
            \_ ->
                Random.step (diceRoller 5) (Random.initialSeed 42)
                    |> Tuple.first
                    |> Expect.equalLists [ 1, 3, 1, 1, 6 ]
        , fuzz (intRange Random.minInt Random.maxInt) "fiveDice always returns five dice" <|
            \seed ->
                Random.step fiveDice (Random.initialSeed seed)
                    |> Tuple.first
                    |> List.length
                    |> Expect.equal 5
        ]


randomDie : Int -> Dice.PipsList
randomDie seed =
    Random.step (diceRoller 1) (Random.initialSeed seed)
        |> Tuple.first


flipNthSuite : Test
flipNthSuite =
    describe "flipNth n dice performs flipNextRoll on the nth element of dice" <|
        [ test "flipNth 0" <|
            \_ ->
                flipNth 0 (DiceBoard (makeDiceList [ ( 1, Keep ), ( 2, Reroll ) ]))
                    |> Expect.equal (DiceBoard (makeDiceList [ ( 1, Reroll ), ( 2, Reroll ) ]))
        ]
