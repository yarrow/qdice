module Dice exposing
    ( DiceList
    , Die
    , NextRoll(..)
    , dieFromPair
    , flipNextRoll
    , fromPairs
    , fromPips
    , mergeDice
    , rerollCount
    , url
    , urlSmall
    )

import Array
import Pip exposing (Pip)


type alias Die =
    { pips : Pip
    , nextRoll : NextRoll
    }


type
    NextRoll
    -- On the next roll, do we Keep this die on the board, or do we put it back in the dice
    -- cup to Reroll it?
    = Keep
    | Reroll


flipRoll : Die -> Die
flipRoll die =
    { die
        | nextRoll =
            case die.nextRoll of
                Keep ->
                    Reroll

                Reroll ->
                    Keep
    }


dieFromPair : ( Int, NextRoll ) -> Die
dieFromPair ( n, nextStatus ) =
    { pips = Pip.fromInt n
    , nextRoll = nextStatus
    }


aUrl : String -> Die -> String
aUrl location d =
    location ++ "/die-" ++ String.fromInt (Pip.toInt d.pips) ++ ".png"


url : Die -> String
url =
    aUrl "assets"


urlSmall : Die -> String
urlSmall =
    aUrl "assets/smol"



--- The DiceList type ----------------------------------


type alias DiceList =
    List Die


fromPips : List Pip -> DiceList
fromPips pips =
    List.map dieFromPip pips


dieFromPip : Pip -> Die
dieFromPip pip =
    { pips = pip, nextRoll = Keep }


mergeDice : List Pip -> DiceList -> DiceList
mergeDice incoming current =
    case ( incoming, current ) of
        ( [], _ ) ->
            current

        ( _, [] ) ->
            []

        ( new :: tailIncoming, old :: tailCurrent ) ->
            case old.nextRoll of
                Keep ->
                    old :: mergeDice incoming tailCurrent

                Reroll ->
                    dieFromPip new :: mergeDice tailIncoming tailCurrent


rerollCount : DiceList -> Int
rerollCount dice =
    List.length (List.filter (\die -> die.nextRoll == Reroll) dice)


flipNextRoll : Int -> DiceList -> DiceList
flipNextRoll j dice =
    let
        diceArray =
            Array.fromList dice

        oldDie =
            diceArray |> Array.get j
    in
    case oldDie of
        Nothing ->
            dice

        Just aDie ->
            diceArray
                |> Array.set j (flipRoll aDie)
                |> Array.toList



-- For ease in testing


fromPairs : List ( Int, NextRoll ) -> DiceList
fromPairs pairs =
    List.map dieFromPair pairs
