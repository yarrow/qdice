module Dice exposing (DiceBoard(..), DiceList, PipsList, diceRoller, fiveDice, flipNth, hasRerolls, makeDiceBoard, makeDiceList, mergeDice, rerollCount)

import Array
import Die exposing (Die, NextRoll, flipNextRoll, makeDie)
import Random exposing (Generator)


type alias DiceList =
    List Die


type alias PipsList =
    List Int


type DiceBoard
    = DiceBoard (List Die)


mergeDice : PipsList -> Maybe DiceBoard -> Maybe DiceBoard
mergeDice incoming current =
    let
        diceList =
            case current of
                Nothing ->
                    List.map Die.fromInt incoming

                Just (DiceBoard oldDice) ->
                    refreshDice incoming oldDice
    in
    Just (DiceBoard diceList)


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
rerollCount (DiceBoard dice) =
    List.length (List.filter (\die -> Die.nextRoll die == Die.Reroll) dice)


hasRerolls : DiceBoard -> Bool
hasRerolls dice =
    rerollCount dice > 0


makeDiceList : List ( Int, NextRoll ) -> DiceList
makeDiceList raw =
    List.map makeDie raw


makeDiceBoard : List ( Int, NextRoll ) -> DiceBoard
makeDiceBoard raw =
    DiceBoard (makeDiceList raw)


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
    Generator PipsList


diceRoller : Int -> DiceGenerator
diceRoller n =
    Random.list n Die.roller


fiveDice : DiceGenerator
fiveDice =
    diceRoller 5
