module App exposing (Model, Msg(..), initialModel, main, update, view)

import Browser
import Dice exposing (DiceBoard(..), DiceList)
import Die exposing (Die)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Random


type Msg
    = RollDice
    | GotDice DiceList
    | DieFlipped Int


rerollCount : Model -> Int
rerollCount model =
    case model.dice of
        Nothing ->
            5

        Just (DiceBoard dice) ->
            List.length (List.filter (\die -> Die.nextRoll die == Die.Reroll) dice)


mergeDice : DiceList -> Maybe DiceBoard -> Maybe DiceBoard
mergeDice incoming current =
    let
        diceList =
            case current of
                Nothing ->
                    incoming

                Just (DiceBoard oldDice) ->
                    refreshDice incoming oldDice
    in
    Just (DiceBoard diceList)


refreshDice : DiceList -> DiceList -> DiceList
refreshDice incoming current =
    case ( incoming, current ) of
        ( [], _ ) ->
            current

        ( _, [] ) ->
            []

        ( new :: tailIncoming, old :: tailCurrent ) ->
            case Die.nextRoll old of
                Die.Keep ->
                    old :: refreshDice incoming tailCurrent

                Die.Reroll ->
                    new :: refreshDice tailIncoming tailCurrent


rollAllowed : Model -> Bool
rollAllowed model =
    model.remainingRolls > 0 && (rerollCount model > 0)


gotDiceIfRollAllowed : Model -> Cmd Msg
gotDiceIfRollAllowed model =
    if rollAllowed model then
        Random.generate GotDice (Dice.diceRoller (rerollCount model))

    else
        Cmd.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RollDice ->
            ( model, gotDiceIfRollAllowed model )

        GotDice incomingDice ->
            let
                newModel =
                    { model
                        | dice = mergeDice incomingDice model.dice
                        , remainingRolls = model.remainingRolls - 1
                    }
            in
            ( newModel, Cmd.none )

        DieFlipped j ->
            let
                newDice =
                    case model.dice of
                        Nothing ->
                            Nothing

                        Just dice ->
                            Just (Dice.flipNth j dice)
            in
            ( { model | dice = newDice }, Cmd.none )


tdDie : Die -> Html msg
tdDie d =
    td [ class "dice" ] [ img [ src (Die.url d) ] [] ]


dieRow : Int -> Die -> Html Msg
dieRow j d =
    tr [ class "dice-row", onClick (DieFlipped j) ] <|
        case Die.nextRoll d of
            Die.Keep ->
                [ tdBlank, tdDie d ]

            Die.Reroll ->
                [ tdDie d, tdBlank ]


tdBlank : Html msg
tdBlank =
    td [ class "dice" ] []


blankRow : Html msg
blankRow =
    tr [ class "dice-row" ] [ tdBlank, tdBlank ]


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Quarantine Dice" ]
        , table [ class "dice", id "dice" ] <|
            [ caption [] [ text (String.fromInt model.remainingRolls ++ " rolls remaining") ]
            , tr [] [ th [] [ text "Reroll" ], th [] [ text "Keep" ] ]
            ]
                ++ (case model.dice of
                        Nothing ->
                            List.repeat 5 blankRow

                        Just (DiceBoard theDice) ->
                            List.indexedMap dieRow theDice
                   )
        , button [ id "roll-dice", onClick RollDice ] [ text "Roll Dice" ]
        ]


type alias Model =
    { dice : Maybe DiceBoard
    , remainingRolls : Int
    }


initialModel : Model
initialModel =
    { dice = Nothing, remainingRolls = 3 }


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
