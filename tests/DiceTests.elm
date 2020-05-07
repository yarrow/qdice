module DiceTests exposing (..)

import Dice exposing (diceRoller, fiveDice, oneDie, pips)
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
                        Dice.oneDie n

                    fragment =
                        "/die-" ++ String.fromInt (Dice.pips d) ++ "."
                in
                Dice.url d
                    |> String.contains fragment
                    |> Expect.true ("Expected to see " ++ fragment)
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


randomDie : Int -> List Dice.OneDie
randomDie seed =
    Random.step (Dice.diceRoller 1) (Random.initialSeed seed)
        |> Tuple.first


nextRollSuite : Test
nextRollSuite =
    describe "the nextRoll field" <|
        [ fuzz (intRange Random.minInt Random.maxInt) "A new die's nextRoll is Keep" <|
            \seed ->
                randomDie seed
                    |> List.map Dice.nextRoll
                    |> Expect.equalLists [ Dice.Keep ]
        , fuzz (intRange Random.minInt Random.maxInt) "flipNextRoll changes Keep to Reroll and vice-versa" <|
            \seed ->
                let
                    hasKeep =
                        randomDie seed

                    hasReroll =
                        List.map Dice.flipNextRoll hasKeep

                    alsoKeep =
                        List.map Dice.flipNextRoll hasReroll
                in
                List.map Dice.nextRoll (hasKeep ++ hasReroll ++ alsoKeep)
                    |> Expect.equalLists [ Dice.Keep, Dice.Reroll, Dice.Keep ]
        ]


reroll : Int -> Dice.OneDie
reroll n =
    Dice.flipNextRoll (oneDie n)


keep : Int -> Dice.OneDie
keep =
    oneDie


flipNthSuite : Test
flipNthSuite =
    describe "flipNth n dice performs flipNextRoll on the nth element of dice" <|
        [ test "flipNth 0" <|
            \_ ->
                Dice.flipNth 0 [ keep 1, reroll 2 ]
                    |> Expect.equalLists [ reroll 1, reroll 2 ]
        ]
