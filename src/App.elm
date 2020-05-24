module App exposing (Model, Msg(..), initialModel, main, update, view)

import Browser
import Dice
import DiceBoard exposing (DiceBoard)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Random
import ScorePad exposing (Occupancy(..), ScorePadBox, ScorePadRow, Scores, activeScorePad, emptyScores, staticScorePad)


type Msg
    = RollDice
    | GotDice Dice.PipsList
    | DieFlipped Int
    | RecordScore


rollAllowed : Model -> Bool
rollAllowed model =
    if model.remainingRolls == 0 then
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
                        , remainingRolls = model.remainingRolls - 1
                    }
            in
            ( newModel, Cmd.none )

        DieFlipped j ->
            ( { model | dice = DiceBoard.flipNextRoll j model.dice }, Cmd.none )

        RecordScore ->
            ( { model | remainingRolls = 3 }, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ header [] [ h1 [] [ text "Quarantine Dice" ] ]
        , section []
            [ div []
                [ viewDice model
                , button [ id "roll-dice", onClick RollDice ] [ text "Roll Dice" ]
                ]
            , viewScores model
            ]
        ]


viewDice : Model -> Html Msg
viewDice model =
    table [ class "dice", id "dice" ] <|
        [ caption [] [ text (String.fromInt model.remainingRolls ++ " rolls remaining") ]
        , tr [] [ th [] [ text "Reroll" ], th [] [ text "Keep" ] ]
        ]
            ++ DiceBoard.display blankRow diceRow model.dice


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

        displayBox : ScorePadBox -> Html msg
        displayBox ( occupancy, score ) =
            let
                attrs =
                    case occupancy of
                        InUse ->
                            [ class "in-use" ]

                        Available pair ->
                            [ class "available" ]
            in
            td attrs [ text score ]

        scoreRow : ScorePadRow -> Html msg
        scoreRow row =
            let
                capt =
                    td [ class "caption" ] [ text row.caption ]
            in
            tr [ class "score-row" ] <| capt :: List.map displayBox row.boxes

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
    , remainingRolls : Int
    , scores : Scores
    }


initialModel : Model
initialModel =
    { dice = Nothing, remainingRolls = 3, scores = emptyScores }


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
