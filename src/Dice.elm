module Dice exposing (oneDie, val)


type OneDie
    = OneDie Int


oneDie : Int -> OneDie
oneDie n =
    OneDie (clamp 1 6 n)


val : OneDie -> Int
val (OneDie n) =
    n
