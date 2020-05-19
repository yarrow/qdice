module DiceTests exposing (diceTests)

import Dice exposing (DiceBoard(..), NextRoll(..), flipNextRoll)
import Expect
import Fuzz exposing (intRange)
import Random
import Test exposing (..)


diceTests : Test
diceTests =
    describe "Tests for Dice.elm" <|
        [ test "fromPips clamps the pips value to 1-6" <|
            \_ ->
                let
                    result =
                        Dice.fromPips [ 0, 1, 2, 3, 4, 5, 6, 7, 49, -82 ]

                    clamped =
                        [ 1, 1, 2, 3, 4, 5, 6, 6, 6, 1 ]
                in
                Dice.toPips result |> Expect.equal (Just clamped)
        , test "fromPips uses Keep for every die" <|
            \_ ->
                let
                    diceBoard =
                        Dice.fromPips [ 1, 3, 2, 4, 7, 2, 4 ]

                    -- FIXME
                    getNextRoll _ die =
                        Dice.nextRoll die

                    rolls =
                        Dice.display Reroll getNextRoll diceBoard
                in
                List.all (\r -> r == Keep) rolls
                    |> Expect.true "Found a nextRoll of Reroll"
        , fuzz (intRange 0 7) "The url for a die with `n` pips contains '/die-`n`.`" <|
            \n ->
                let
                    d =
                        Dice.fromInt n

                    fragment =
                        "/die-" ++ String.fromInt (Dice.pips d) ++ "."
                in
                Dice.url d
                    |> String.contains fragment
                    |> Expect.true ("Expected to see " ++ fragment)
        , describe "The value of `Dice.roller n` is a random generator returning a list of `n` random dice" <|
            [ test "Dice.roller 0 is a generator that always returns an empty list" <|
                \_ ->
                    Random.step (Dice.roller 0) (Random.initialSeed 42)
                        |> Tuple.first
                        |> Expect.equalLists []
            , test "Dice.roller 1 returns a list with one random die" <|
                \_ ->
                    Random.step (Dice.roller 1) (Random.initialSeed 42)
                        |> Tuple.first
                        |> Expect.equalLists [ 6 ]
            , test "Dice.roller 5 returns a list with five random dice" <|
                \_ ->
                    Random.step (Dice.roller 5) (Random.initialSeed 42)
                        |> Tuple.first
                        |> Expect.equalLists [ 1, 3, 1, 1, 6 ]
            ]
        , describe "`flipNextRoll n dice` flips the NextRoll on the nth element of dice" <|
            [ test "`flipNextRoll 0` flips Keep/Reroll status of first element" <|
                \_ ->
                    flipNextRoll 0 (Dice.makeDiceBoard [ ( 1, Keep ), ( 2, Reroll ) ])
                        |> Expect.equal (Dice.makeDiceBoard [ ( 1, Reroll ), ( 2, Reroll ) ])
            ]
        ]
