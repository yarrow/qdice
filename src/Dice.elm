module Dice exposing (OneDie, diceRoller, fiveDice, oneDie, pips, url)

import Random exposing (Generator)


type alias Fields =
    { pips : Int }


type OneDie
    = OneDie Fields


minDie : Int
minDie =
    1


maxDie : Int
maxDie =
    6


oneDie : Int -> OneDie
oneDie n =
    OneDie { pips = clamp minDie maxDie n }


pips : OneDie -> Int
pips (OneDie die) =
    die.pips


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
