module App exposing (Model, Msg(..), initialModel, main, update, view)

import Browser
import Dice
import DiceBoard exposing (DiceBoard)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Random
import Rank
import ScorePad
    exposing
        ( Location
        , Occupancy(..)
        , RowKind(..)
        , ScorePadBox
        , ScorePadRow
        , Scores
        , activeScorePad
        , emptyScores
        , numberOfTurns
        , staticScorePad
        )


type Msg
    = RollDice
    | GotDice Dice.PipsList
    | DieFlipped Int
    | KeepSet Rank.DiceToKeep
    | RecordScore Location
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
                        | dice = Just (DiceBoard.mergeDice incomingDice model.dice)
                        , rollsLeft = model.rollsLeft - 1
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
                        |> Maybe.map DiceBoard.toPipsList
                        |> Maybe.map (Rank.tallyPipsList rank)

                scores =
                    ScorePad.setScoreBox ( rank, column ) scoreToInsert model.scores

                newModel =
                    { model
                        | dice = Nothing
                        , rollsLeft = 3
                        , turnsLeft = model.turnsLeft - 1
                        , scores = scores
                    }
            in
            ( newModel, Cmd.none )

        NewGame ->
            ( initialModel, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ header [] [ h1 [] [ text "Quarantine Dice" ] ]
        , section []
            [ viewDice model
            , viewScores model
            ]
        ]


viewDice : Model -> Html Msg
viewDice model =
    let
        theButton =
            if model.turnsLeft == 0 then
                button [ class "roll-dice", onClick NewGame ] [ text "New Game" ]

            else if rollAllowed model then
                button [ class "roll-dice", onClick RollDice ] [ text "Roll Dice" ]

            else
                button [ class "dont-roll-dice" ] [ text "Roll Dice" ]

        buttonRow =
            tr [ class "button-row" ] [ td [ class "dice-button", colspan 2 ] [ theButton ] ]

        suggestions =
            if model.rollsLeft > 0 then
                suggestionRows model.dice

            else
                []
    in
    table [ class "dice", id "dice" ] <|
        [ caption [] [ text (String.fromInt model.rollsLeft ++ " rolls remaining") ]
        , tr [] [ th [] [ text "Reroll" ], th [] [ text "Keep" ] ]
        ]
            ++ DiceBoard.display blankRow diceRow model.dice
            ++ [ buttonRow ]
            ++ suggestions


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
                    [ colspan 2, onClick (KeepSet diceToKeep) ]
            in
            tr [ class "suggestion" ] [ td attrs contents ]
    in
    List.map suggestion suggestions



--List.map foo (DiceBoard.suggestions diceBoard)


tdDie : Dice.Die -> Html msg
tdDie d =
    td [ class "die" ] [ img [ src (Dice.url d) ] [] ]


diceRow : Int -> Dice.Die -> Html Msg
diceRow j d =
    tr [ class "dice-row", onClick (DieFlipped j) ] <|
        case Dice.nextRoll d of
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


viewScores : Model -> Html Msg
viewScores model =
    let
        topBox label =
            td [ class "score-top" ] [ text label ]

        displayBox : ScorePadBox -> Html Msg
        displayBox ( occupancy, score ) =
            case occupancy of
                InUse ->
                    td [ class "in-use" ] [ text score ]

                Available location ->
                    td [ class "available" ]
                        [ a [ onClick (RecordScore location), href "#0" ] [ text score ] ]

        scoreRow : ScorePadRow -> Html Msg
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
                        [ ( InUse, score ) ] ->
                            [ td [ class "in-use", colspan 3 ] [ text score ] ]

                        _ ->
                            List.map displayBox row.boxes
            in
            tr [ class rowClass ] <| capt :: scoreDisplay

        scoreRows =
            let
                getScorePad =
                    case model.dice of
                        Nothing ->
                            staticScorePad

                        Just dice ->
                            activeScorePad (DiceBoard.toPipsList dice)
            in
            List.map scoreRow (getScorePad model.scores)

        topRow =
            tr [ class "score-top-row" ]
                [ topBox "", topBox "x1", topBox "x2", topBox "x3" ]
    in
    table [ class "scorepad", id "scorepad" ] (topRow :: scoreRows)


type alias Model =
    { dice : DiceBoard
    , rollsLeft : Int
    , turnsLeft : Int
    , scores : Scores
    }


initialModel : Model
initialModel =
    { dice = Nothing
    , rollsLeft = 3
    , turnsLeft = ScorePad.numberOfTurns
    , scores = emptyScores
    }


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
