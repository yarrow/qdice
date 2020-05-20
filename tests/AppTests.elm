module AppTests exposing (appTests)

import App exposing (Model, Msg(..), initialModel, update, view)
import Dice exposing (NextRoll(..))
import DiceBoard
import Expect
import Html.Attributes as Attr
import Random
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector exposing (attribute, id, tag, text)


type alias PreDice =
    List ( Int, Dice.NextRoll )


randomPipsList : Dice.PipsList
randomPipsList =
    Random.step (DiceBoard.rollForNewDice Nothing) (Random.initialSeed 0) |> Tuple.first


modelWithDice : PreDice -> Model
modelWithDice dice =
    { initialModel | dice = DiceBoard.makeDiceBoard dice }


modelAfterFirstRoll : Model
modelAfterFirstRoll =
    update (GotDice randomPipsList) initialModel |> Tuple.first


appTests : Test
appTests =
    describe "Tests for App.elm" <|
        [ describe "Properties of initial model" <|
            [ test "We start with no dice" <|
                \_ ->
                    initialModel.dice
                        |> Expect.equal Nothing
            , test "We start with three rolls available" <|
                \_ ->
                    initialModel.remainingRolls
                        |> Expect.equal 3
            ]
        , describe "Properties of update" <|
            let
                keep =
                    ( 1, Keep )

                reroll =
                    ( 1, Reroll )

                keepAll =
                    [ keep, keep, keep, keep, keep ]
            in
            [ test "RollDice doesn't change the model" <|
                \_ ->
                    update RollDice initialModel
                        |> Tuple.first
                        |> Expect.equal initialModel
            , test "RollDice sends Cmd.none if remainingRolls is 0" <|
                \_ ->
                    update RollDice { modelAfterFirstRoll | remainingRolls = 0 }
                        |> Tuple.second
                        |> Expect.equal Cmd.none
            , test "RollDice sends Cmd.none if there are no dice to be rerolled" <|
                \_ ->
                    update RollDice (modelWithDice keepAll)
                        |> Tuple.second
                        |> Expect.equal Cmd.none

            {-
                  , test "In the initial model, (GotDice someDice) installs (Just someDice) as the model's .dice value" <|
                      \_ ->
                          modelWithRandomDice.dice
                              |> Expect.equal (Just Dice.makeDiceBoard randomPipsList)
               , test "Initially, all dice have nextRoll == Keep" <|
                   \_ ->
                       List.all (\d -> Dice.nextRoll d == Keep) randomPipsList
                           |> Expect.true "Initially, all dice have an NextRoll of Keep"
            -}
            , test "`DieFlipped 0` causes the 0th die to flip its NextRoll status" <|
                \_ ->
                    let
                        rerollFirst =
                            DiceBoard.makeDiceBoard [ reroll, keep, keep, keep, keep ]
                    in
                    update (DieFlipped 0) (modelWithDice keepAll)
                        |> Tuple.first
                        |> .dice
                        |> Expect.equal rerollFirst
            , test "Incoming dice replace dice to be rerolled" <|
                \_ ->
                    let
                        startingDice =
                            [ keep, reroll, keep, reroll, keep ]

                        incomingDice =
                            [ 2, 3 ]

                        resultingDice =
                            DiceBoard.makeDiceBoard [ keep, ( 2, Keep ), keep, ( 3, Keep ), keep ]
                    in
                    update (GotDice incomingDice) (modelWithDice startingDice)
                        |> Tuple.first
                        |> .dice
                        |> Expect.equal resultingDice
            , test "After the first roll, we have 2 rolls remaining" <|
                \_ ->
                    update (GotDice randomPipsList) initialModel
                        |> Tuple.first
                        |> .remainingRolls
                        |> Expect.equal 2
            ]
        , describe "Properties of view" <|
            [ test "The app has a 'Roll Dice' button" <|
                \_ ->
                    view initialModel
                        |> Query.fromHtml
                        |> Query.find [ tag "button", id "roll-dice" ]
                        |> Query.has [ text "Roll Dice" ]
            , test "We start with no dice" <|
                \_ ->
                    view initialModel
                        |> Query.fromHtml
                        |> Query.findAll [ tag "img" ]
                        |> Query.count (Expect.equal 0)
            , test "When there are dice, there are five dice" <|
                \_ ->
                    view modelAfterFirstRoll
                        |> Query.fromHtml
                        |> Query.findAll [ tag "img" ]
                        |> Query.count (Expect.equal 5)
            , test "There are five dice rows" <|
                \_ ->
                    view modelAfterFirstRoll
                        |> Query.fromHtml
                        |> Query.findAll [ tag "tr", attribute <| Attr.class "dice-row" ]
                        |> Query.count (Expect.equal 5)
            , test "There are five dice rows in the initial Model too" <|
                \_ ->
                    view initialModel
                        |> Query.fromHtml
                        |> Query.findAll [ tag "tr", attribute <| Attr.class "dice-row" ]
                        |> Query.count (Expect.equal 5)
            , test "The inital model shows 3 remainingRolls" <|
                \_ ->
                    view initialModel
                        |> Query.fromHtml
                        |> Query.find [ tag "caption" ]
                        |> Query.has [ text "3 rolls remaining" ]
            , test "After the first roll, we show two rolls remaining" <|
                \_ ->
                    view modelAfterFirstRoll
                        |> Query.fromHtml
                        |> Query.find [ tag "caption" ]
                        |> Query.has [ text "2 rolls remaining" ]
            ]
        ]
