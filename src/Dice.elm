module Dice exposing (DiceBoard(..), DiceList, diceRoller, fiveDice, flipNth, makeDice)

import Array exposing (Array)
import Die exposing (Die, NextRoll, flipNextRoll, makeDie)
import Random exposing (Generator)


type alias DiceList =
    List Die


type DiceBoard
    = DiceBoard (List Die)


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
