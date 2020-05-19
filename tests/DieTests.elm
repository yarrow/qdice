module DieTests exposing (dieTests)

import Dice
import Die exposing (NextRoll(..), flipNextRoll, nextRoll, pips)
import Expect
import Fuzz exposing (intRange)
import Random
import Test exposing (..)


dieTests : Test
dieTests =
    describe "Tests for Die.elm" <|
        [ fuzz (intRange 0 7) "The url for a die with `n` pips contains '/die-`n`.`" <|
            \n ->
                let
                    d =
                        Dice.fromInt n

                    fragment =
                        "/die-" ++ String.fromInt (pips d) ++ "."
                in
                Die.url d
                    |> String.contains fragment
                    |> Expect.true ("Expected to see " ++ fragment)
        , describe "the nextRoll field" <|
            [ fuzz (intRange Random.minInt Random.maxInt) "A new die's nextRoll is Keep" <|
                \seed ->
                    Dice.fromInt (randomDie seed)
                        |> nextRoll
                        |> Expect.equal Keep
            , fuzz (intRange Random.minInt Random.maxInt) "flipNextRoll changes Keep to Reroll and vice-versa" <|
                \seed ->
                    let
                        hasKeep =
                            Dice.fromInt (randomDie seed)

                        hasReroll =
                            flipNextRoll hasKeep

                        alsoKeep =
                            flipNextRoll hasReroll
                    in
                    List.map nextRoll [ hasKeep, hasReroll, alsoKeep ]
                        |> Expect.equalLists [ Keep, Reroll, Keep ]
            ]
        ]


randomDie : Int -> Int
randomDie seed =
    Random.step Die.roller (Random.initialSeed seed)
        |> Tuple.first
