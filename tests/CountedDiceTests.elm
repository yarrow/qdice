module CountedDiceTests exposing (..)

import CountedDice
import Die exposing (oneDie)
import Expect exposing (Expectation)
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
