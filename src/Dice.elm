module Dice exposing (DiceGenerator, OneDie, diceRoller, oneDie, pips, url)

import Random exposing (Generator)


type OneDie
    = OneDie Int


minDie : Int
minDie =
    1


maxDie : Int
maxDie =
    6


oneDie : Int -> OneDie
oneDie n =
    OneDie (clamp minDie maxDie n)


pips : OneDie -> Int
pips (OneDie n) =
    n


url : OneDie -> String
url d =
    "assets/die-" ++ String.fromInt (pips d) ++ ".png"


type alias DiceGenerator =
    Generator (List OneDie)


diceRoller : Int -> DiceGenerator
diceRoller n =
    Random.list n (Random.map oneDie (Random.int minDie maxDie))
