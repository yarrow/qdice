module CountedDice exposing (CountedDice(..), fromDice, toList)

import Array exposing (Array)
import Dice exposing (DiceList)
import Die


type CountedDice
    = CountedDice (Array Int)


fromDice : DiceList -> CountedDice
fromDice dice =
    let
        increment jth counter =
            case Array.get jth counter of
                Just old ->
                    Array.set jth (old + 1) counter

                Nothing ->
                    counter
    in
    CountedDice (List.foldr increment (Array.repeat 7 0) (List.map Die.pips dice))


toList : CountedDice -> List Int
toList (CountedDice counted) =
    Array.toList counted
