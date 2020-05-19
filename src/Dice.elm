module Dice exposing (DiceBoard, PipsList, diceRoller, display, emptyBoard, fiveDice, flipNth, fromInt, fromPips, hasRerolls, makeDiceBoard, mergeDice, rerollCount, toPips)

import Array
import Die exposing (Die(..), NextRoll, flipNextRoll)
import Random exposing (Generator)


type alias DiceList =
    List Die


type alias PipsList =
    List Int


type DiceBoard
    = DiceBoard (Maybe DiceList)


emptyBoard : DiceBoard
emptyBoard =
    DiceBoard Nothing


numberOfDice : Int
numberOfDice =
    5


minDie : Int
minDie =
    1


maxDie : Int
maxDie =
    6


display : a -> (Int -> Die -> a) -> DiceBoard -> List a
display emptyRow makeRow (DiceBoard board) =
    case board of
        Nothing ->
            List.repeat numberOfDice emptyRow

        Just theDice ->
            List.indexedMap makeRow theDice


diceBoard : DiceList -> DiceBoard
diceBoard dice =
    DiceBoard (Just dice)


fromPips : PipsList -> DiceBoard
fromPips pips =
    diceBoard (List.map fromInt pips)


toPips : DiceBoard -> Maybe PipsList
toPips (DiceBoard board) =
    Maybe.map (List.map Die.pips) board


mergeDice : PipsList -> DiceBoard -> DiceBoard
mergeDice incoming (DiceBoard current) =
    case current of
        Nothing ->
            fromPips incoming

        Just oldDice ->
            diceBoard (refreshDice incoming oldDice)


refreshDice : PipsList -> DiceList -> DiceList
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
                    fromInt new :: refreshDice tailIncoming tailCurrent


rerollCount : DiceBoard -> Int
rerollCount (DiceBoard board) =
    case board of
        Nothing ->
            numberOfDice

        Just dice ->
            List.length (List.filter (\die -> Die.nextRoll die == Die.Reroll) dice)


hasRerolls : DiceBoard -> Bool
hasRerolls dice =
    rerollCount dice > 0


makeDie : ( Int, NextRoll ) -> Die
makeDie ( n, nextStatus ) =
    Die
        { pips = clamp minDie maxDie n
        , nextRoll = nextStatus
        }


fromInt : Int -> Die
fromInt n =
    makeDie ( n, Die.Keep )


makeDiceBoard : List ( Int, NextRoll ) -> DiceBoard
makeDiceBoard raw =
    DiceBoard (Just (List.map makeDie raw))


flipNth : Int -> DiceBoard -> DiceBoard
flipNth n (DiceBoard board) =
    case board of
        Nothing ->
            DiceBoard Nothing

        Just dice ->
            DiceBoard (Just (do_flip n dice))


do_flip : Int -> DiceList -> DiceList
do_flip n dice =
    let
        diceArray =
            Array.fromList dice

        oldDie =
            diceArray |> Array.get n
    in
    case oldDie of
        Nothing ->
            dice

        Just aDie ->
            Array.set n (flipNextRoll aDie) diceArray
                |> Array.toList


type alias DiceGenerator =
    Generator PipsList


diceRoller : Int -> DiceGenerator
diceRoller n =
    Random.list n Die.roller


fiveDice : DiceGenerator
fiveDice =
    diceRoller numberOfDice
