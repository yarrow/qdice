module Dice exposing (DiceList, diceRoller, fiveDice, flipNth, makeDice)

import Array exposing (Array)
import Die exposing (Die, NextRoll, flipNextRoll, makeDie)
import Random exposing (Generator)


makeDice : List ( Int, NextRoll ) -> DiceList
makeDice raw =
    List.map makeDie raw


flipNth : Int -> DiceList -> DiceList
flipNth n dice =
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


type alias DiceList =
    List Die


type alias DiceGenerator =
    Generator DiceList


diceRoller : Int -> DiceGenerator
diceRoller n =
    Random.list n Die.roller


fiveDice : DiceGenerator
fiveDice =
    diceRoller 5
