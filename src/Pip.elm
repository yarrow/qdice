module Pip exposing
    ( Pip
    , fromInt
    , mapFromInt
    , mapToInt
    , randomPip
    , toInt
    )

import Random


type Pip
    = Pip Int


minPip : Int
minPip =
    1


maxPip : Int
maxPip =
    6


fromInt : Int -> Pip
fromInt n =
    Pip (clamp minPip maxPip n)


toInt : Pip -> Int
toInt (Pip n) =
    n


mapToInt : List Pip -> List Int
mapToInt list =
    List.map toInt list


mapFromInt : List Int -> List Pip
mapFromInt list =
    List.map fromInt list


randomPip : Random.Generator Pip
randomPip =
    Random.map Pip (Random.int minPip maxPip)
