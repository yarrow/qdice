module Dice exposing
    ( DiceBoard
    , Die
    , NextRoll(..)
    , PipsList
    , display
    , flipNextRoll
    , fromPairs
    , fromPips
    , hasRerolls
    , makeDiceBoard
    , mergeDice
    , nextRoll
    , pips
    , rollForNewDice
    , toDiceList
    , toPips
    , url
    )

import Array
import Random exposing (Generator)



--- The Die type (singular of Dice, not opposite of Live ^_^)


type Die
    = Die
        { pips : Int
        , nextRoll : NextRoll
        }


type
    NextRoll
    -- On the next roll, do we Keep this die on the board, or do we put it back in the dice
    -- cup to Reroll it?
    = Keep
    | Reroll


flipRoll : Die -> Die
flipRoll (Die die) =
    Die
        { die
            | nextRoll =
                case die.nextRoll of
                    Keep ->
                        Reroll

                    Reroll ->
                        Keep
        }


pips : Die -> Int
pips (Die die) =
    die.pips


nextRoll : Die -> NextRoll
nextRoll (Die die) =
    die.nextRoll


url : Die -> String
url d =
    "assets/die-" ++ String.fromInt (pips d) ++ ".png"


minDie : Int
minDie =
    1


maxDie : Int
maxDie =
    6


dieFromPair : ( Int, NextRoll ) -> Die
dieFromPair ( n, nextStatus ) =
    Die
        { pips = clamp minDie maxDie n
        , nextRoll = nextStatus
        }


dieFromInt : Int -> Die
dieFromInt n =
    dieFromPair ( n, Keep )



--- The DiceBoard type ----------------------------------


type alias DiceList =
    List Die


type DiceBoard
    = DiceBoard DiceList


numberOfDice : Int
numberOfDice =
    5


display : a -> (Int -> Die -> a) -> Maybe DiceBoard -> List a
display emptyRow makeRow board =
    case board of
        Nothing ->
            List.repeat numberOfDice emptyRow

        Just (DiceBoard theDice) ->
            List.indexedMap makeRow theDice


type alias PipsList =
    List Int


fromPips : PipsList -> DiceBoard
fromPips pipsList =
    DiceBoard (List.map dieFromInt pipsList)


toPips : DiceBoard -> PipsList
toPips (DiceBoard dice) =
    List.map pips dice


toDiceList : DiceBoard -> DiceList
toDiceList (DiceBoard dice) =
    dice


mergeDice : PipsList -> Maybe DiceBoard -> DiceBoard
mergeDice incoming current =
    case current of
        Nothing ->
            fromPips incoming

        Just (DiceBoard oldDice) ->
            DiceBoard (refreshDice incoming oldDice)


refreshDice : PipsList -> DiceList -> DiceList
refreshDice incoming current =
    case ( incoming, current ) of
        ( [], _ ) ->
            current

        ( _, [] ) ->
            []

        ( new :: tailIncoming, old :: tailCurrent ) ->
            case nextRoll old of
                Keep ->
                    old :: refreshDice incoming tailCurrent

                Reroll ->
                    dieFromInt new :: refreshDice tailIncoming tailCurrent


rerollCount : Maybe DiceBoard -> Int
rerollCount board =
    case board of
        Nothing ->
            numberOfDice

        Just (DiceBoard dice) ->
            List.length (List.filter (\die -> nextRoll die == Reroll) dice)


rollForNewDice : Maybe DiceBoard -> Generator PipsList
rollForNewDice diceBoard =
    Random.list (rerollCount diceBoard) (Random.int minDie maxDie)


hasRerolls : Maybe DiceBoard -> Bool
hasRerolls board =
    rerollCount board > 0


fromPairs : List ( Int, NextRoll ) -> DiceBoard
fromPairs pairs =
    DiceBoard (List.map dieFromPair pairs)


makeDiceBoard : List ( Int, NextRoll ) -> Maybe DiceBoard
makeDiceBoard raw =
    Just <| DiceBoard <| List.map dieFromPair raw


flipNextRoll : Int -> Maybe DiceBoard -> Maybe DiceBoard
flipNextRoll n board =
    Maybe.map (flipNth n) board


flipNth : Int -> DiceBoard -> DiceBoard
flipNth j (DiceBoard dice) =
    let
        diceArray =
            Array.fromList dice

        oldDie =
            diceArray |> Array.get j
    in
    DiceBoard <|
        case oldDie of
            Nothing ->
                dice

            Just aDie ->
                diceArray
                    |> Array.set j (flipRoll aDie)
                    |> Array.toList
