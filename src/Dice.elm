module Dice exposing (OnRoll(..), OneDie, diceRoller, fiveDice, flipNth, flipOnRoll, onRoll, oneDie, pips, url)

import Array
import Random exposing (Generator)


type OnRoll
    = Keep
    | Reroll


type alias Fields =
    { pips : Int
    , onRoll : OnRoll
    }


type OneDie
    = OneDie Fields


oneDie : Int -> OneDie
oneDie n =
    OneDie
        { pips = clamp minDie maxDie n
        , onRoll = Keep
        }


pips : OneDie -> Int
pips (OneDie die) =
    die.pips


onRoll : OneDie -> OnRoll
onRoll (OneDie die) =
    die.onRoll


flipOnRoll : OneDie -> OneDie
flipOnRoll (OneDie d) =
    OneDie
        { pips = d.pips
        , onRoll =
            case d.onRoll of
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
            Array.set n (flipOnRoll aDie) diceArray
                |> Array.toList


type alias DiceGenerator =
    Generator (List OneDie)


diceRoller : Int -> DiceGenerator
diceRoller n =
    Random.list n (Random.map oneDie (Random.int minDie maxDie))


fiveDice : DiceGenerator
fiveDice =
    diceRoller 5
