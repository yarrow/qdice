module Dice exposing (diceRoller, oneDie, pips)

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


diceRoller : Int -> Generator (List OneDie)
diceRoller n =
    Random.list n (Random.map oneDie (Random.int minDie maxDie))
