module Dice exposing (OnRoll(..), OneDie, diceRoller, fiveDice, onRoll, oneDie, pips, url)

import Random exposing (Generator)


type OnRoll
    = Keep
    | Reroll


type alias Fields =
    { pips : Int
    , onRoll : OnRoll
    }


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


type OneDie
    = OneDie Fields


minDie : Int
minDie =
    1


maxDie : Int
maxDie =
    6


url : OneDie -> String
url d =
    "assets/die-" ++ String.fromInt (pips d) ++ ".png"


type alias DiceGenerator =
    Generator (List OneDie)


diceRoller : Int -> DiceGenerator
diceRoller n =
    Random.list n (Random.map oneDie (Random.int minDie maxDie))


fiveDice : DiceGenerator
fiveDice =
    diceRoller 5
