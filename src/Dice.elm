module Dice exposing
    ( DiceBoard
    , Die(..)
    , NextRoll(..)
    , PipsList
    , display
    , emptyBoard
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
    = DiceBoard (Maybe DiceList)


emptyBoard : DiceBoard
emptyBoard =
    DiceBoard Nothing


numberOfDice : Int
numberOfDice =
    5


display : a -> (Int -> Die -> a) -> DiceBoard -> List a
display emptyRow makeRow (DiceBoard board) =
    case board of
        Nothing ->
            List.repeat numberOfDice emptyRow

        Just theDice ->
            List.indexedMap makeRow theDice


fromDiceList : DiceList -> DiceBoard
fromDiceList dice =
    DiceBoard (Just dice)


type alias PipsList =
    List Int


fromPips : PipsList -> DiceBoard
fromPips pipsList =
    fromDiceList (List.map dieFromInt pipsList)


toPips : DiceBoard -> PipsList
toPips diceBoard =
    List.map pips (toDiceList diceBoard)


toDiceList : DiceBoard -> DiceList
toDiceList (DiceBoard board) =
    case board of
        Nothing ->
            []

        Just dice ->
            dice


mergeDice : PipsList -> DiceBoard -> DiceBoard
mergeDice incoming (DiceBoard current) =
    case current of
        Nothing ->
            fromPips incoming

        Just oldDice ->
            fromDiceList (refreshDice incoming oldDice)


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


rerollCount : DiceBoard -> Int
rerollCount (DiceBoard board) =
    case board of
        Nothing ->
            numberOfDice

        Just dice ->
            List.length (List.filter (\die -> nextRoll die == Reroll) dice)


rollForNewDice : DiceBoard -> Generator PipsList
rollForNewDice diceBoard =
    Random.list (rerollCount diceBoard) (Random.int minDie maxDie)


hasRerolls : DiceBoard -> Bool
hasRerolls dice =
    rerollCount dice > 0


fromPairs : List ( Int, NextRoll ) -> DiceBoard
fromPairs pairs =
    fromDiceList (List.map dieFromPair pairs)


makeDiceBoard : List ( Int, NextRoll ) -> DiceBoard
makeDiceBoard raw =
    DiceBoard (Just (List.map dieFromPair raw))


flipNextRoll : Int -> DiceBoard -> DiceBoard
flipNextRoll n (DiceBoard board) =
    case board of
        Nothing ->
            DiceBoard Nothing

        Just dice ->
            DiceBoard <|
                Just <|
                    let
                        diceArray =
                            Array.fromList dice

                        oldDie =
                            diceArray |> Array.get n
                    in
                    case oldDie of
                        Nothing ->
                            dice

                        Just (Die aDie) ->
                            let
                                flipped =
                                    case aDie.nextRoll of
                                        Keep ->
                                            Reroll

                                        Reroll ->
                                            Keep
                            in
                            Array.set n (Die { aDie | nextRoll = flipped }) diceArray
                                |> Array.toList
