module DiceTests exposing (diceTests)

import Dice exposing (NextRoll(..), flipNextRoll)
import DiceBoard
import Expect
import Fuzz exposing (intRange)
import Random
import Test exposing (..)


diceTests : Test
diceTests =
    describe "Tests for Dice.elm and DiceBoard.elm" <|
        let
            nextRolls dice =
                List.map (\die -> Dice.nextRoll die) dice
        in
        [ test "fromPips clamps the pips value to 1-6" <|
            \_ ->
                let
                    result =
                        Dice.fromPips [ 0, 1, 2, 3, 4, 5, 6, 7, 49, -82 ]

                    clamped =
                        [ 1, 1, 2, 3, 4, 5, 6, 6, 6, 1 ]
                in
                List.map Dice.pips result |> Expect.equal clamped
        , test "fromPips uses Keep for every die" <|
            \_ ->
                let
                    pips =
                        [ 1, 3, 2, 4, 7, 2, 4 ]

                    rolls =
                        nextRolls (Dice.fromPips pips)

                    ok =
                        (List.length rolls == List.length pips)
                            && List.all (\r -> r == Keep) rolls
                in
                ok |> Expect.true "Wrong number of dice, or found one with Reroll"
        , fuzz (intRange 1 6) "The url for a die with `n` pips contains '/die-`n`.`" <|
            \n ->
                let
                    dice =
                        Dice.fromPips [ n ]

                    theUrl =
                        case dice of
                            [] ->
                                ""

                            die :: _ ->
                                Dice.url die

                    fragment =
                        "/die-" ++ String.fromInt n ++ "."
                in
                theUrl
                    |> String.contains fragment
                    |> Expect.true ("Expected to see " ++ fragment)
        , describe "`rollForNewDice diceBoard` returns a generator for the appropriate number of new dice for diceBoard" <|
            let
                numberRolled diceRoller =
                    Random.step diceRoller (Random.initialSeed 42)
                        |> Tuple.first
                        |> List.length
            in
            [ test "`rollForNewDice emptyBoard` returns a 5-dice generator" <|
                \_ ->
                    numberRolled (DiceBoard.rollForNewDice Nothing)
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
        ]
