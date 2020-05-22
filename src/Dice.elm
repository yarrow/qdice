module Dice exposing
    ( DiceList
    , Die
    , NextRoll(..)
    , PipsList(..)
    , flipNextRoll
    , fromPairs
    , fromPips
    , mergeDice
    , nextRoll
    , pips
    , randomPip
    , rerollCount
    , unPip
    , url
    )

import Array
import Random



--- The Die type (singular of Dice, not opposite of Live ^_^)


type Die
    = Die
        { pips : Int
        , nextRoll : NextRoll
        }


type
    NextRoll
    -- On the next roll, do we Keep this die on the board, or do we put it back in the dice
    -- cup to Reroll it?
    = Keep
    | Reroll


flipRoll : Die -> Die
flipRoll (Die die) =
    Die
        { die
            | nextRoll =
                case die.nextRoll of
                    Keep ->
                        Reroll

                    Reroll ->
                        Keep
        }


pips : Die -> Int
pips (Die die) =
    die.pips


nextRoll : Die -> NextRoll
nextRoll (Die die) =
    die.nextRoll


url : Die -> String
url d =
    "assets/die-" ++ String.fromInt (pips d) ++ ".png"


minPip : Int
minPip =
    1


maxPip : Int
maxPip =
    6


randomPip : Random.Generator Int
randomPip =
    Random.int minPip maxPip


dieFromPair : ( Int, NextRoll ) -> Die
dieFromPair ( n, nextStatus ) =
    Die
        { pips = clamp minPip maxPip n
        , nextRoll = nextStatus
        }


dieFromInt : Int -> Die
dieFromInt n =
    dieFromPair ( n, Keep )



--- The DiceList type ----------------------------------


type alias DiceList =
    List Die


type PipsList
    = PipsList (List Int)


unPip : PipsList -> List Int
unPip (PipsList list) =
    list


fromPips : PipsList -> DiceList
fromPips (PipsList pipsList) =
    List.map dieFromInt pipsList


mergeDice : PipsList -> DiceList -> DiceList
mergeDice (PipsList incoming) current =
    case ( incoming, current ) of
        ( [], _ ) ->
            current

        ( _, [] ) ->
            []

        ( new :: tailIncoming, old :: tailCurrent ) ->
            case nextRoll old of
                Keep ->
                    old :: mergeDice (PipsList incoming) tailCurrent

                Reroll ->
                    dieFromInt new :: mergeDice (PipsList tailIncoming) tailCurrent


rerollCount : DiceList -> Int
rerollCount dice =
    List.length (List.filter (\die -> nextRoll die == Reroll) dice)


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
