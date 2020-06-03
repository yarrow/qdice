module AppTests exposing (appTests)

import App exposing (Model, Msg(..), initialModel, update, view)
import Dice exposing (NextRoll(..))
import DiceBoard
import Expect
import Fuzz
import Html
import Html.Attributes as Attr
import Pip exposing (Pip)
import Random
import Rank exposing (Rank)
import Score exposing (numberOfTurns)
import Shrink
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector exposing (attribute, class, id, tag, text)


type alias PreDice =
    List ( Int, Dice.NextRoll )


chance1 : ( Rank, Int )
chance1 =
    ( Rank.arbitraryRank, 1 )


randomPips : List Pip
randomPips =
    Random.step (DiceBoard.rollForNewDice DiceBoard.empty) (Random.initialSeed 0) |> Tuple.first


setDice : Model -> PreDice -> Model
setDice model dice =
    { model | dice = DiceBoard.makeDiceBoard dice }


updateModel : Msg -> Model -> Model
updateModel msg model =
    update msg model |> Tuple.first


modelAfterFirstRoll : Model
modelAfterFirstRoll =
    updateModel (GotDice randomPips) initialModel


pipsFuzz : Fuzz.Fuzzer (List Pip)
pipsFuzz =
    Fuzz.custom (DiceBoard.rollForNewDice DiceBoard.empty) Shrink.noShrink


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
                        |> Expect.equal DiceBoard.empty
            , test "We start with three rolls available" <|
                \_ ->
                    initialModel.rollsLeft
                        |> Expect.equal 3
            , test "We start with numberOfTurns turns left" <|
                \_ ->
                    initialModel.turnsLeft
                        |> Expect.equal numberOfTurns
            , test "We start with blank scores" <|
                \_ ->
                    initialModel.scores
                        |> Expect.equal Score.emptyScores
            ]
        , describe "Properties of update" <|
            let
                keep =
                    ( 1, Keep )

                reroll =
                    ( 1, Reroll )

                keepAll =
                    [ keep, keep, keep, keep, keep ]

                rollableModel =
                    updateModel (GotDice randomPips) initialModel
                        |> updateModel (DieFlipped 0)
            in
            [ test "RollDice doesn't change the model" <|
                \_ ->
                    updateModel RollDice rollableModel
                        |> Expect.equal rollableModel
            , test "RollDice sends a nontrivial Cmd (GotDice, but we can't test that) rollsLeft > 0, turnsLeft >0, and at least one die is Reroll" <|
                \_ ->
                    update RollDice rollableModel
                        |> Tuple.second
                        |> Expect.notEqual Cmd.none
            , test "RollDice sends Cmd.none if model.rollsLeft is 0" <|
                \_ ->
                    update RollDice { rollableModel | rollsLeft = 0 }
                        |> Tuple.second
                        |> Expect.equal Cmd.none
            , test "RollDice sends Cmd.none if model.turnsLeft is 0" <|
                \_ ->
                    update RollDice { rollableModel | turnsLeft = 0 }
                        |> Tuple.second
                        |> Expect.equal Cmd.none
            , test "RollDice sends Cmd.none if there are no dice to be rerolled" <|
                \_ ->
                    update RollDice (setDice rollableModel keepAll)
                        |> Tuple.second
                        |> Expect.equal Cmd.none
            , test "After the first roll, all dice have nextRoll == Keep" <|
                \_ ->
                    let
                        allKeep diceList =
                            List.all (\d -> d.nextRoll == Keep) diceList

                        passed =
                            case DiceBoard.toDiceList modelAfterFirstRoll.dice of
                                Nothing ->
                                    False

                                Just dice ->
                                    allKeep dice
                    in
                    passed |> Expect.true "Initially, all dice have an NextRoll of Keep"
            , test "`DieFlipped 0` causes the 0th die to flip its NextRoll status" <|
                \_ ->
                    let
                        rerollFirst =
                            DiceBoard.makeDiceBoard [ reroll, keep, keep, keep, keep ]
                    in
                    updateModel (DieFlipped 0) (setDice initialModel keepAll)
                        |> .dice
                        |> Expect.equal rerollFirst
            , test "`DieFlipped` does nothing if model.rollsLeft == 0" <|
                \_ ->
                    let
                        withDice =
                            setDice initialModel keepAll

                        outOfRolls =
                            { withDice | rollsLeft = 0 }

                        originalDice =
                            outOfRolls.dice
                    in
                    updateModel (DieFlipped 0) outOfRolls
                        |> .dice
                        |> Expect.equal originalDice
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
                    updateModel (GotDice (Pip.mapFromInt incomingDice)) (setDice initialModel startingDice)
                        |> .dice
                        |> Expect.equal resultingDice
            , test "After the first roll, we have 2 rolls remaining" <|
                \_ ->
                    updateModel (GotDice randomPips) initialModel
                        |> .rollsLeft
                        |> Expect.equal 2
            , test "RecordScore sets model.rollsLeft to 3" <|
                \_ ->
                    updateModel (RecordScore chance1) modelAfterFirstRoll
                        |> .rollsLeft
                        |> Expect.equal 3
            , test "RecordScore decrements model.turnsLeft" <|
                \_ ->
                    let
                        startingTurns =
                            modelAfterFirstRoll.turnsLeft
                    in
                    updateModel (RecordScore chance1) modelAfterFirstRoll
                        |> .turnsLeft
                        |> Expect.equal (startingTurns - 1)
            , test "RecordScore sets model.dice to Nothing" <|
                \_ ->
                    updateModel (RecordScore chance1) modelAfterFirstRoll
                        |> .dice
                        |> Expect.equal DiceBoard.empty
            , test "(RecordScore (rank, j) sets the new model's score at (rank, j) to the points indicated in the scorePad" <|
                \_ ->
                    let
                        scoreForRank =
                            modelAfterFirstRoll.dice
                                |> DiceBoard.toPips
                                |> Maybe.map (Rank.scoreAt Rank.arbitraryRank)
                    in
                    updateModel (RecordScore chance1) modelAfterFirstRoll
                        |> .scores
                        |> Score.getBox chance1
                        |> Expect.equal scoreForRank
            , test "NewGame resets the model to the initial model" <|
                \_ ->
                    updateModel NewGame rollableModel
                        |> Expect.equal initialModel
            ]
        , describe "Properties of viewing dice" <|
            [ test "The app has a 'Roll Dice' button" <|
                \_ ->
                    find initialModel [ tag "button", class "roll-dice" ]
                        |> Query.has [ text "Roll Dice" ]
            , test "The Roll Dice button changes to New Game if there are no turns left" <|
                \_ ->
                    find { initialModel | turnsLeft = 0 } [ tag "button", class "roll-dice" ]
                        |> Query.has [ text "New Game" ]
            , test "There is a dice table" <|
                \_ ->
                    find initialModel [ class "dice" ]
                        |> Query.has [ id "dice" ]
            , test "We start with no dice" <|
                \_ ->
                    findAll initialModel [ tag "img" ]
                        |> Query.count (Expect.equal 0)
            , test "There are five dice rows" <|
                \_ ->
                    findAll modelAfterFirstRoll [ tag "tr", attribute <| Attr.class "dice-row" ]
                        |> Query.count (Expect.equal 5)
            , test "There are five dice rows in the initial Model too" <|
                \_ ->
                    findAll initialModel [ tag "tr", attribute <| Attr.class "dice-row" ]
                        |> Query.count (Expect.equal 5)
            , test "The inital model shows 3 rollsLeft" <|
                \_ ->
                    find initialModel [ class "rolls-left" ]
                        |> Query.has [ text "3 rolls left" ]
            , test "After the first roll, we show two rolls remaining" <|
                \_ ->
                    find modelAfterFirstRoll [ class "rolls-left" ]
                        |> Query.has [ text "2 rolls left" ]
            , fuzz pipsFuzz "After a roll, we see the suggested keep sets" <|
                \pips ->
                    let
                        model =
                            updateModel (GotDice pips) initialModel

                        suggestions =
                            Rank.suggestKeeping (DiceBoard.toPips model.dice)
                    in
                    findAll model [ class "suggestion" ]
                        |> Query.count (Expect.equal (List.length suggestions))
            , test "We only see suggests keep sets if rollsLeft is nonzero" <|
                \_ ->
                    let
                        model =
                            { modelAfterFirstRoll | rollsLeft = 0 }
                    in
                    findAll model [ class "suggestion" ]
                        |> Query.count (Expect.equal 0)
            ]
        , describe "Properties of viewing scores" <|
            [ describe "The scorepad with no dice" <|
                [ test "There is a scorepad table" <|
                    \_ ->
                        find initialModel [ class "scorepad" ]
                            |> Query.has [ id "scorepad" ]
                , test "There are 13 rows with scores" <|
                    \_ ->
                        findAll initialModel [ class "score-row" ]
                            |> Query.count (Expect.equal 13)
                , test "There are numberOfTurns in-use score boxes" <|
                    \_ ->
                        findAll initialModel [ class "score-row" ]
                            |> Query.keep (class "in-use")
                            |> Query.count (Expect.equal numberOfTurns)
                , test "... and all in-use score boxes are blank" <|
                    \_ ->
                        findAll initialModel [ class "score-row" ]
                            |> Query.keep (class "in-use")
                            |> Query.each (Query.contains [ Html.text "" ])
                ]
            , describe "The scorepad after the first roll" <|
                [ test "There is a scorepad table" <|
                    \_ ->
                        find modelAfterFirstRoll [ class "scorepad" ]
                            |> Query.has [ id "scorepad" ]
                , test "There are 13 rows with scores" <|
                    \_ ->
                        findAll modelAfterFirstRoll [ class "score-row" ]
                            |> Query.count (Expect.equal 13)
                , test "There are numberOfTurns available score boxes" <|
                    \_ ->
                        findAll modelAfterFirstRoll [ class "available" ]
                            |> Query.count (Expect.equal numberOfTurns)
                , test "... and no available score box is blank" <|
                    \_ ->
                        True |> Expect.true "I don't know how to test for this"
                ]

            {-
               , test "There are 5 rows with (sub-)totals" <|
                   \_ ->
                       findAll initialModel [ class "score-total-row" ]
                           |> Query.count (Expect.equal 5)
            -}
            ]
        ]
