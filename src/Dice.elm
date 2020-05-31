module Dice exposing
    ( DiceList
    , Die
    , NextRoll(..)
    , Pip
    , PipList
    , PipsList(..)
    , dieFromPair
    , flipNextRoll
    , fromPairs
    , fromPips
    , fromPipsList
    , mergeDice
    , pipFromInt
    , pipListFromIntList
    , pipListToIntList
    , pipToInt
    , randomPip
    , rerollCount
    , singular
    , unPip
    , url
    , urlSmall
    )

import Array
import Random



{---- Heading for 

module Pip exposing
    ( List,
    , Pip
    , ...
    )

-----}


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


type alias PipList =
    List Pip



--- The Die type (singular of Dice, not opposite of Live ^_^)


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


aUrl : String -> Die -> String
aUrl location d =
    location ++ "/die-" ++ String.fromInt (pipToInt d.pips) ++ ".png"


url : Die -> String
url =
    aUrl "assets"


urlSmall : Die -> String
urlSmall =
    aUrl "assets/smol"


randomPip : Random.Generator Pip
randomPip =
    Random.map Pip (Random.int minPip maxPip)


dieFromPair : ( Int, NextRoll ) -> Die
dieFromPair ( n, nextStatus ) =
    { pips = pipFromInt n
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


singular : PipsList -> List Pip
singular (PipsList pipsList) =
    List.map pipFromInt pipsList


unPip : PipsList -> List Int
unPip (PipsList list) =
    list


fromPipsList : PipsList -> DiceList
fromPipsList (PipsList pipsList) =
    List.map dieFromInt pipsList


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
