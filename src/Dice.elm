module Dice exposing (NextRoll(..), OneDie, diceRoller, fiveDice, flipNextRoll, flipNth, makeDice, makeDie, nextRoll, oneDie, pips, url)

import Array
import Random exposing (Generator)


type NextRoll
    = Keep
    | Reroll


type alias Fields =
    { pips : Int
    , nextRoll : NextRoll
    }


type OneDie
    = OneDie Fields


oneDie : Int -> OneDie
oneDie n =
    OneDie
        { pips = clamp minDie maxDie n
        , nextRoll = Keep
        }


pips : OneDie -> Int
pips (OneDie die) =
    die.pips


nextRoll : OneDie -> NextRoll
nextRoll (OneDie die) =
    die.nextRoll


makeDie : ( Int, NextRoll ) -> OneDie
makeDie ( n, nextStatus ) =
    OneDie { pips = n, nextRoll = nextStatus }


makeDice : List ( Int, NextRoll ) -> List OneDie
makeDice raw =
    List.map makeDie raw


flipNextRoll : OneDie -> OneDie
flipNextRoll (OneDie d) =
    OneDie
        { pips = d.pips
        , nextRoll =
            case d.nextRoll of
                Keep ->
                    Reroll

                Reroll ->
                    Keep
        }


minDie : Int
minDie =
    1


maxDie : Int
maxDie =
    6


url : OneDie -> String
url d =
    "assets/die-" ++ String.fromInt (pips d) ++ ".png"


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
    Random.list n (Random.map oneDie (Random.int minDie maxDie))


fiveDice : DiceGenerator
fiveDice =
    diceRoller 5
