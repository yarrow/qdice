module Pip exposing
    ( Pip
    , pipFromInt
    , pipListFromIntList
    , pipListToIntList
    , pipToInt
    , randomPip
    )

import Array
import Random


type Pip
    = Pip Int


minPip : Int
minPip =
    1


maxPip : Int
maxPip =
    6


pipFromInt : Int -> Pip
pipFromInt n =
    Pip (clamp minPip maxPip n)


pipToInt : Pip -> Int
pipToInt (Pip n) =
    n


pipListToIntList : List Pip -> List Int
pipListToIntList list =
    List.map pipToInt list


pipListFromIntList : List Int -> List Pip
pipListFromIntList list =
    List.map pipFromInt list


randomPip : Random.Generator Pip
randomPip =
    Random.map Pip (Random.int minPip maxPip)
