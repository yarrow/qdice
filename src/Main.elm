module Main exposing (Model, Msg(..), initialModel, main, update, view)

import Browser
import Dice
import DiceBoard exposing (DiceBoard)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Pip exposing (Pip)
import Random
import Rank exposing (Rating(..))
import Score exposing (Location, Scores, emptyScores)
import ScorePad exposing (Occupancy(..), RowKind(..), makeScorePad)



-- Model


type alias Model =
    { dice : DiceBoard
    , rollsLeft : Int
    , turnsLeft : Int
    , scores : Scores
    , undoInfo : Maybe ( DiceBoard, Location )
    }


initialModel : Model
initialModel =
    { dice = DiceBoard.empty
    , rollsLeft = 3
    , turnsLeft = Score.numberOfTurns
    , scores = emptyScores
    , undoInfo = Nothing
    }


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- Update


type Msg
    = RollDice
    | GotDice (List Pip)
    | DieFlipped Int
    | KeepSet Rank.DiceToKeep
    | RecordScore Location
    | UndoScore
    | NewGame


rollAllowed : Model -> Bool
rollAllowed model =
    if model.rollsLeft == 0 || model.turnsLeft == 0 then
        False

    else
        DiceBoard.hasRerolls model.dice


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RollDice ->
            let
                cmd =
                    if rollAllowed model then
                        Random.generate GotDice (DiceBoard.rollForNewDice model.dice)

                    else
                        Cmd.none
            in
            ( model, cmd )

        GotDice incomingDice ->
            let
                newModel =
                    { model
                        | dice = DiceBoard.mergeDice incomingDice model.dice
                        , rollsLeft = model.rollsLeft - 1
                        , undoInfo = Nothing
                    }
            in
            ( newModel, Cmd.none )

        DieFlipped j ->
            let
                newModel =
                    if model.rollsLeft == 0 then
                        model

                    else
                        { model | dice = DiceBoard.flipNextRoll j model.dice }
            in
            ( newModel, Cmd.none )

        KeepSet diceToKeep ->
            ( { model | dice = DiceBoard.keepOnly diceToKeep model.dice }, Cmd.none )

        RecordScore ( rank, column ) ->
            let
                scoreToInsert =
                    model.dice
                        |> DiceBoard.toPips
                        |> Maybe.map (Rank.scoreAt rank)

                scores =
                    Score.setBox ( rank, column ) scoreToInsert model.scores

                newModel =
                    { model
                        | dice = DiceBoard.empty
                        , rollsLeft = 3
                        , turnsLeft = model.turnsLeft - 1
                        , scores = scores
                        , undoInfo = Just ( model.dice, ( rank, column ) )
                    }
            in
            ( newModel, Cmd.none )

        UndoScore ->
            case model.undoInfo of
                Nothing ->
                    ( model, Cmd.none )

                Just ( oldDice, location ) ->
                    let
                        scores =
                            Score.setBox location Nothing model.scores

                        newModel =
                            { model
                                | dice = oldDice
                                , rollsLeft = 0
                                , turnsLeft = model.turnsLeft + 1
                                , scores = scores
                                , undoInfo = Nothing
                            }
                    in
                    ( newModel, Cmd.none )

        NewGame ->
            ( initialModel, Cmd.none )



-- View


view : Model -> Html Msg
view model =
    div []
        [ header [] [ h1 [] [ text "Quarantine Dice" ] ]
        , section []
            [ viewDice model
            , viewScores model
            ]
        ]



-- View Dice


viewDice : Model -> Html Msg
viewDice model =
    let
        buttonRow b =
            tr [ class "button-row" ] [ td [ class "dice-button", colspan 2 ] [ b ] ]

        rollButton =
            buttonRow <|
                if model.turnsLeft == 0 then
                    button [ class "roll-dice", onClick NewGame ] [ text "New Game" ]

                else if rollAllowed model then
                    button [ class "roll-dice", onClick RollDice ] [ text "Roll Dice" ]

                else
                    button [ class "dont-roll-dice" ] [ text "Roll Dice" ]

        undoButton =
            buttonRow <| button [ class "undo", onClick UndoScore ] [ text "Undo" ]

        buttonSection =
            case model.undoInfo of
                Nothing ->
                    [ rollButton ]

                Just _ ->
                    [ rollButton, undoButton ]

        suggestions =
            if model.rollsLeft > 0 then
                suggestionRows model.dice

            else
                []

        rollsLeft =
            String.fromInt model.rollsLeft ++ " rolls left"
    in
    table [ class "dice", id "dice" ] <|
        List.concat
            [ [ tr [] [ th [] [ text "Reroll" ], th [] [ text "Keep" ] ] ]
            , DiceBoard.display blankRow diceRow model.dice
            , [ tr [] [ td [ colspan 2, class "rolls-left" ] [ text rollsLeft ] ] ]
            , buttonSection
            , suggestions
            ]


suggestionRows : DiceBoard -> List (Html Msg)
suggestionRows diceBoard =
    let
        suggestions =
            DiceBoard.suggestions diceBoard

        suggestion ( diceToKeep, urls ) =
            let
                contents =
                    List.map (\u -> img [ src u ] []) urls

                attrs =
                    [ class "suggestion", colspan 2, onClick (KeepSet diceToKeep) ]
            in
            tr [] [ td attrs contents ]
    in
    List.map suggestion suggestions


tdDie : Dice.Die -> Html msg
tdDie d =
    td [ class "die" ] [ img [ src (Dice.url d) ] [] ]


diceRow : Int -> Dice.Die -> Html Msg
diceRow j d =
    tr [ class "dice-row", onClick (DieFlipped j) ] <|
        case d.nextRoll of
            Dice.Keep ->
                [ tdBlank, tdDie d ]

            Dice.Reroll ->
                [ tdDie d, tdBlank ]


tdBlank : Html msg
tdBlank =
    td [ class "die" ] []


blankRow : Html msg
blankRow =
    tr [ class "dice-row" ] [ tdBlank, tdBlank ]



-- View Scores


viewScores : Model -> Html Msg
viewScores model =
    let
        topBox label =
            td [ class "score-top" ] [ text label ]

        displayBox { occupancy, rating, score } =
            let
                rateClass =
                    case rating of
                        Meager ->
                            "meager"

                        Sufficient ->
                            "sufficient"

                        Ample ->
                            "ample"
            in
            case occupancy of
                InUse ->
                    td [ class "in-use", class rateClass ] [ text score ]

                Available location ->
                    td [ class "available", class rateClass ]
                        [ button [ onClick (RecordScore location) ] [ text score ] ]

        scoreRow : ScorePad.Row -> Html Msg
        scoreRow row =
            let
                capt =
                    td [ class "caption" ] [ text row.caption ]

                rowClass =
                    case row.kind of
                        Rolled ->
                            "score-row"

                        Calculated ->
                            "summation-row"

                scoreDisplay =
                    case row.boxes of
                        [ box ] ->
                            [ td [ class "in-use", colspan 3 ] [ text box.score ] ]

                        _ ->
                            List.map displayBox row.boxes
            in
            tr [ class rowClass ] <| capt :: scoreDisplay

        scoreRows =
            let
                pips =
                    DiceBoard.toPips model.dice
            in
            List.map scoreRow (makeScorePad pips model.scores)

        topRow =
            tr [ class "score-top-row" ]
                [ topBox "", topBox "x1", topBox "x2", topBox "x3" ]
    in
    table [ class "scorepad", id "scorepad" ] (topRow :: scoreRows)
