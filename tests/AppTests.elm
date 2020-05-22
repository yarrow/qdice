module AppTests exposing (appTests)

import App exposing (Model, Msg(..), initialModel, update, view)
import Dice exposing (NextRoll(..))
import DiceBoard
import Expect
import Html exposing (Html)
import Html.Attributes as Attr
import Random
import ScorePad
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector exposing (attribute, class, id, tag, text)


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


find : Model -> List Test.Html.Selector.Selector -> Query.Single Msg
find model attributes =
    view model |> Query.fromHtml |> Query.find attributes


findAll : Model -> List Test.Html.Selector.Selector -> Query.Multiple Msg
findAll model attributes =
    view model |> Query.fromHtml |> Query.findAll attributes


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
            , test "We start with blank scores" <|
                \_ ->
                    initialModel.scores
                        |> Expect.equal ScorePad.initialScores
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
            , test "After the first roll, all dice have nextRoll == Keep" <|
                \_ ->
                    let
                        allKeep diceList =
                            List.all (\d -> Dice.nextRoll d == Keep) diceList

                        passed =
                            case modelAfterFirstRoll.dice of
                                Nothing ->
                                    False

                                Just dice ->
                                    allKeep (DiceBoard.toDiceList dice)
                    in
                    passed |> Expect.true "Initially, all dice have an NextRoll of Keep"
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
        , describe "Properties of viewing dice" <|
            [ test "The app has a 'Roll Dice' button" <|
                \_ ->
                    find initialModel [ tag "button", id "roll-dice" ]
                        |> Query.has [ text "Roll Dice" ]
            , test "There is a dice table" <|
                \_ ->
                    find initialModel [ class "dice" ]
                        |> Query.has [ id "dice" ]
            , test "We start with no dice" <|
                \_ ->
                    findAll initialModel [ tag "img" ]
                        |> Query.count (Expect.equal 0)
            , test "When there are dice, there are five dice" <|
                \_ ->
                    findAll modelAfterFirstRoll [ tag "img" ]
                        |> Query.count (Expect.equal 5)
            , test "There are five dice rows" <|
                \_ ->
                    findAll modelAfterFirstRoll [ tag "tr", attribute <| Attr.class "dice-row" ]
                        |> Query.count (Expect.equal 5)
            , test "There are five dice rows in the initial Model too" <|
                \_ ->
                    findAll initialModel [ tag "tr", attribute <| Attr.class "dice-row" ]
                        |> Query.count (Expect.equal 5)
            , test "The inital model shows 3 remainingRolls" <|
                \_ ->
                    find initialModel [ tag "caption" ]
                        |> Query.has [ text "3 rolls remaining" ]
            , test "After the first roll, we show two rolls remaining" <|
                \_ ->
                    find modelAfterFirstRoll [ tag "caption" ]
                        |> Query.has [ text "2 rolls remaining" ]
            ]
        , describe "Properties of viewing scores" <|
            [ test "There is a scorepad table" <|
                \_ ->
                    find initialModel [ class "scorepad" ]
                        |> Query.has [ id "scorepad" ]
            , test "There are 13 rows with scores" <|
                \_ ->
                    findAll initialModel [ class "score-row" ]
                        |> Query.count (Expect.equal 13)
            , test "The initial model has 39 unclickable score boxes" <|
                \_ ->
                    findAll initialModel [ class "score-box" ]
                        |> Query.count (Expect.equal 39)

            {-
               , test "... and those unclickable score boxes are blank" <|
                   \_ ->
                       findAll initialModel [ class "score-box" ]
                           |> Query.each (Query.contains [ Html.text "" ])
            -}
            {-
               , test "There are 5 rows with (sub-)totals" <|
                   \_ ->
                       findAll initialModel [ class "score-total-row" ]
                           |> Query.count (Expect.equal 5)
            -}
            ]
        ]
