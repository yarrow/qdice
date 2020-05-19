module Dice exposing
    ( DiceBoard
    , Die(..)
    , NextRoll(..)
    , PipsList
    , display
    , emptyBoard
    , flipNextRoll
    , fromPips
    , hasRerolls
    , makeDiceBoard
    , mergeDice
    , nextRoll
    , pips
    , rerollCount
    , roller
    , toDiceList
    , toPips
    , url
    )

import Array
import Random exposing (Generator)


type alias DiceList =
    List Die


type alias PipsList =
    List Int


type DiceBoard
    = DiceBoard (Maybe DiceList)


emptyBoard : DiceBoard
emptyBoard =
    DiceBoard Nothing


numberOfDice : Int
numberOfDice =
    5


minDie : Int
minDie =
    1


maxDie : Int
maxDie =
    6


display : a -> (Int -> Die -> a) -> DiceBoard -> List a
display emptyRow makeRow (DiceBoard board) =
    case board of
        Nothing ->
            List.repeat numberOfDice emptyRow

        Just theDice ->
            List.indexedMap makeRow theDice


fromDiceList : DiceList -> DiceBoard
fromDiceList dice =
    DiceBoard (Just dice)


fromPips : PipsList -> DiceBoard
fromPips pipsList =
    fromDiceList (List.map dieFromInt pipsList)


toPips : DiceBoard -> PipsList
toPips diceBoard =
    List.map pips (toDiceList diceBoard)


toDiceList : DiceBoard -> DiceList
toDiceList (DiceBoard board) =
    case board of
        Nothing ->
            []

        Just dice ->
            dice


mergeDice : PipsList -> DiceBoard -> DiceBoard
mergeDice incoming (DiceBoard current) =
    case current of
        Nothing ->
            fromPips incoming

        Just oldDice ->
            fromDiceList (refreshDice incoming oldDice)


refreshDice : PipsList -> DiceList -> DiceList
refreshDice incoming current =
    case ( incoming, current ) of
        ( [], _ ) ->
            current

        ( _, [] ) ->
            []

        ( new :: tailIncoming, old :: tailCurrent ) ->
            case nextRoll old of
                Keep ->
                    old :: refreshDice incoming tailCurrent

                Reroll ->
                    dieFromInt new :: refreshDice tailIncoming tailCurrent


rerollCount : DiceBoard -> Int
rerollCount (DiceBoard board) =
    case board of
        Nothing ->
            numberOfDice

        Just dice ->
            List.length (List.filter (\die -> nextRoll die == Reroll) dice)


hasRerolls : DiceBoard -> Bool
hasRerolls dice =
    rerollCount dice > 0


makeDie : ( Int, NextRoll ) -> Die
makeDie ( n, nextStatus ) =
    Die
        { pips = clamp minDie maxDie n
        , nextRoll = nextStatus
        }


dieFromInt : Int -> Die
dieFromInt n =
    makeDie ( n, Keep )


makeDiceBoard : List ( Int, NextRoll ) -> DiceBoard
makeDiceBoard raw =
    DiceBoard (Just (List.map makeDie raw))


flipNextRoll : Int -> DiceBoard -> DiceBoard
flipNextRoll n (DiceBoard board) =
    case board of
        Nothing ->
            DiceBoard Nothing

        Just dice ->
            DiceBoard (Just (do_flip n dice))


do_flip : Int -> DiceList -> DiceList
do_flip n dice =
    let
        diceArray =
            Array.fromList dice

        oldDie =
            diceArray |> Array.get n
    in
    case oldDie of
        Nothing ->
            dice

        Just (Die aDie) ->
            let
                flipped =
                    case aDie.nextRoll of
                        Keep ->
                            Reroll

                        Reroll ->
                            Keep
            in
            Array.set n (Die { aDie | nextRoll = flipped }) diceArray
                |> Array.toList


type alias DiceGenerator =
    Generator PipsList


roller : Int -> DiceGenerator
roller n =
    Random.list n (Random.int minDie maxDie)



--------------------------------------------------------------- Die stuff


type NextRoll
    = Keep
    | Reroll


type Die
    = Die
        { pips : Int
        , nextRoll : NextRoll
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
