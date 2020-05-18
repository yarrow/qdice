module Dice exposing (DiceBoard(..), PipsList, diceRoller, fiveDice, flipNth, hasRerolls, makeDiceBoard, mergeDice, rerollCount)

import Array
import Die exposing (Die, NextRoll, flipNextRoll, makeDie)
import Random exposing (Generator)


type alias DiceList =
    List Die


type alias PipsList =
    List Int


type DiceBoard
    = DiceBoard (Maybe DiceList)


mergeDice : PipsList -> DiceBoard -> DiceBoard
mergeDice incoming (DiceBoard current) =
    let
        diceList =
            case current of
                Nothing ->
                    List.map Die.fromInt incoming

                Just oldDice ->
                    refreshDice incoming oldDice
    in
    DiceBoard (Just diceList)


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
                    Die.fromInt new :: refreshDice tailIncoming tailCurrent


rerollCount : DiceBoard -> Int
rerollCount (DiceBoard diceBoard) =
    case diceBoard of
        Nothing ->
            5

        Just dice ->
            List.length (List.filter (\die -> Die.nextRoll die == Die.Reroll) dice)


hasRerolls : DiceBoard -> Bool
hasRerolls dice =
    rerollCount dice > 0


makeDiceBoard : List ( Int, NextRoll ) -> DiceBoard
makeDiceBoard raw =
    DiceBoard (Just (List.map makeDie raw))


flipNth : Int -> DiceBoard -> DiceBoard
flipNth n (DiceBoard diceBoard) =
    case diceBoard of
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
    diceRoller 5
