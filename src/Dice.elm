module Dice exposing (DiceBoard(..), DiceList, diceRoller, fiveDice, flipNth, hasRerolls, makeDice, mergeDice, rerollCount)

import Array exposing (Array)
import Die exposing (Die, NextRoll, flipNextRoll, makeDie)
import Random exposing (Generator)


type alias DiceList =
    List Die


type DiceBoard
    = DiceBoard (List Die)


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


rerollCount : DiceBoard -> Int
rerollCount (DiceBoard dice) =
    List.length (List.filter (\die -> Die.nextRoll die == Die.Reroll) dice)


hasRerolls : DiceBoard -> Bool
hasRerolls dice =
    rerollCount dice > 0


makeDice : List ( Int, NextRoll ) -> DiceList
makeDice raw =
    List.map makeDie raw


flipNth : Int -> DiceBoard -> DiceBoard
flipNth n (DiceBoard dice) =
    let
        diceArray =
            Array.fromList dice

        oldDie =
            diceArray |> Array.get n
    in
    case oldDie of
        Nothing ->
            DiceBoard dice

        Just aDie ->
            Array.set n (flipNextRoll aDie) diceArray
                |> Array.toList
                |> DiceBoard


type alias DiceGenerator =
    Generator DiceList


diceRoller : Int -> DiceGenerator
diceRoller n =
    Random.list n Die.roller


fiveDice : DiceGenerator
fiveDice =
    diceRoller 5
