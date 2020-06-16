module DiceTests exposing (diceTests)

import Dice exposing (NextRoll(..), flipNextRoll)
import DiceBoard
import Expect
import Fuzz exposing (intRange)
import Pip exposing (Pip)
import Random
import Rank exposing (DiceToKeep(..))
import Test exposing (..)


diceTests : Test
diceTests =
    describe "Tests for Dice.elm and DiceBoard.elm" <|
        [ fuzz (intRange 1 6) "The url for a die with `n` pips contains 'die-`n`.`" <|
            \n ->
                let
                    die =
                        { pips = Pip.fromInt n, nextRoll = Keep }

                    theUrl =
                        Dice.url die

                    fragment =
                        "die-" ++ String.fromInt n ++ "."
                in
                theUrl
                    |> String.contains fragment
                    |> Expect.true ("Expected to see " ++ fragment)
        , describe "`rollForNewDice diceBoard` returns a generator for the appropriate number of new dice for diceBoard" <|
            let
                numberRolled : Random.Generator (List Pip) -> Int
                numberRolled diceRoller =
                    Random.step diceRoller (Random.initialSeed 42)
                        |> Tuple.first
                        |> List.length
            in
            [ test "`rollForNewDice DiceBoard.empty` returns a 5-dice generator" <|
                \_ ->
                    numberRolled (DiceBoard.rollForNewDice DiceBoard.empty)
                        |> Expect.equal 5
            , test "When diceBoard is not empty, `rollForNewDice diceBoard` returns a generator for the number dice with nextRoll==Reroll" <|
                \_ ->
                    let
                        two =
                            [ ( 1, Keep ), ( 1, Keep ), ( 3, Reroll ), ( 4, Keep ), ( 6, Reroll ) ]

                        three =
                            [ ( 1, Reroll ), ( 1, Keep ), ( 3, Reroll ), ( 4, Keep ), ( 6, Reroll ) ]

                        none =
                            [ ( 1, Keep ), ( 1, Keep ), ( 3, Keep ), ( 4, Keep ), ( 6, Keep ) ]

                        diceBoards =
                            List.map DiceBoard.makeDiceBoard [ two, three, none ]
                    in
                    List.map (numberRolled << DiceBoard.rollForNewDice) diceBoards
                        |> Expect.equalLists [ 2, 3, 0 ]
            ]
        , describe "`flipNextRoll n dice` flips the NextRoll on the nth element of dice" <|
            [ test "`flipNextRoll 0` flips Keep/Reroll status of first element" <|
                \_ ->
                    flipNextRoll 0 (Dice.fromPairs [ ( 1, Keep ), ( 2, Reroll ) ])
                        |> Expect.equal (Dice.fromPairs [ ( 1, Reroll ), ( 2, Reroll ) ])
            ]
        , test "keepOnly (OfAKind 4) sets all dice with 4 pips to Keep and the rest to Reroll" <|
            \_ ->
                let
                    start =
                        [ ( 4, Reroll ), ( 1, Reroll ), ( 2, Keep ), ( 4, Keep ), ( 4, Reroll ) ]

                    expected =
                        [ ( 4, Keep ), ( 1, Reroll ), ( 2, Reroll ), ( 4, Keep ), ( 4, Keep ) ]
                in
                DiceBoard.keepOnly (OfAKind 4) (DiceBoard.makeDiceBoard start)
                    |> Expect.equal (DiceBoard.makeDiceBoard expected)
        , test "keepOnly (Straight [2,3,4]) sets exactly one die with 2 pips, one with 3, and one with 4 to Keep, and all other dice to Reroll" <|
            \_ ->
                let
                    start =
                        [ ( 4, Reroll ), ( 2, Reroll ), ( 2, Keep ), ( 4, Keep ), ( 3, Reroll ) ]

                    expected =
                        [ ( 4, Keep ), ( 2, Keep ), ( 2, Reroll ), ( 4, Reroll ), ( 3, Keep ) ]
                in
                DiceBoard.keepOnly (Straight [ 2, 3, 4 ]) (DiceBoard.makeDiceBoard start)
                    |> Expect.equal (DiceBoard.makeDiceBoard expected)
        , test "suggestions Diceboard lists all the suggested diceToKeep for those dice, and the urls for the dice images" <|
            \_ ->
                let
                    u n =
                        Dice.urlSmall (Dice.dieFromPair ( n, Keep ))

                    diceBoard =
                        DiceBoard.makeDiceBoard
                            [ ( 4, Reroll ), ( 2, Reroll ), ( 2, Keep ), ( 4, Keep ), ( 3, Reroll ) ]

                    expected =
                        [ ( OfAKind 4, [ u 4, u 4 ] )
                        , ( OfAKind 2, [ u 2, u 2 ] )
                        , ( Straight [ 2, 3, 4 ], [ u 2, u 3, u 4 ] )
                        ]
                in
                DiceBoard.suggestions diceBoard
                    |> Expect.equalLists expected
        ]
