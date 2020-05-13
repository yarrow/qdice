module Dice exposing (diceRoller, fiveDice, flipNth, makeDice)

import Array exposing (Array)
import Die exposing (NextRoll(..), OneDie, flipNextRoll, makeDie, nextRoll, oneDie, pips, url)
import Random exposing (Generator)


makeDice : List ( Int, NextRoll ) -> List OneDie
makeDice raw =
    List.map makeDie raw


flipNth : Int -> List OneDie -> List OneDie
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


type alias DiceGenerator =
    Generator (List OneDie)


diceRoller : Int -> DiceGenerator
diceRoller n =
    Random.list n (Random.map oneDie (Random.int Die.minDie Die.maxDie))


fiveDice : DiceGenerator
fiveDice =
    diceRoller 5
