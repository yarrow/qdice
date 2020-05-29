module DiceBoard exposing
    ( DiceBoard
    , display
    , flipNextRoll
    , hasRerolls
    , keepOnly
    , makeDiceBoard
    , mergeDice
    , rollForNewDice
    , suggestions
    , toDiceList
    , toPipsList
    )

import Dice exposing (DiceList, Die, NextRoll(..), PipsList(..))
import Random exposing (Generator)
import Rank exposing (DiceToKeep(..))
import Set



-- This module (pretends to) enforce the invariant that a DiceBoard has either
-- zero dice (is Nothing) or five dice (is Just FiveDice). See the mergeDice
-- comment below for a place that we can't check this easily.


type alias DiceBoard =
    Maybe FiveDice


type FiveDice
    = FiveDice DiceList


numberOfDice : Int
numberOfDice =
    5


display : a -> (Int -> Die -> a) -> DiceBoard -> List a
display emptyRow makeRow board =
    case board of
        Nothing ->
            List.repeat numberOfDice emptyRow

        Just (FiveDice theDice) ->
            List.indexedMap makeRow theDice


toDiceList : FiveDice -> DiceList
toDiceList (FiveDice dice) =
    dice


toPipsList : FiveDice -> PipsList
toPipsList (FiveDice dice) =
    PipsList (List.map Dice.pips dice)


rerollCount : DiceBoard -> Int
rerollCount board =
    case board of
        Nothing ->
            numberOfDice

        Just (FiveDice dice) ->
            Dice.rerollCount dice



-- Note that mergeDice depends on the incoming PipsList to have been
-- generated from rollForNewDice.  If this is NOT the case, then the
-- invariant that a FiveDice has either 0 or 5 dice will be broken.
-- I can't think of a way around this that doesn't make things worse.


mergeDice : PipsList -> DiceBoard -> FiveDice
mergeDice incoming current =
    case current of
        Nothing ->
            FiveDice (Dice.fromPipsList incoming)

        Just (FiveDice oldDice) ->
            FiveDice (Dice.mergeDice incoming oldDice)


rollForNewDice : DiceBoard -> Generator PipsList
rollForNewDice diceBoard =
    Random.map PipsList (Random.list (rerollCount diceBoard) Dice.randomPip)


hasRerolls : DiceBoard -> Bool
hasRerolls board =
    rerollCount board > 0


flipNextRoll : Int -> DiceBoard -> DiceBoard
flipNextRoll n board =
    let
        flip =
            \j (FiveDice dice) -> FiveDice (Dice.flipNextRoll j dice)
    in
    Maybe.map (flip n) board


suggestions : DiceBoard -> List ( DiceToKeep, List String )
suggestions diceBoard =
    case diceBoard of
        Nothing ->
            []

        Just fiveDice ->
            let
                keepSets =
                    Rank.suggestKeeping (toPipsList fiveDice)

                keptUrls diceToKeep =
                    justKeepOnly diceToKeep fiveDice
                        |> toDiceList
                        |> List.filter (\die -> Dice.nextRoll die == Keep)
                        |> List.map Dice.url
                        |> List.sort
            in
            List.map (\keep -> ( keep, keptUrls keep )) keepSets


justKeepOnly : DiceToKeep -> FiveDice -> FiveDice
justKeepOnly suggested (FiveDice diceList) =
    let
        keepByPips pipsToKeep =
            List.map
                (\die ->
                    if Dice.pips die == pipsToKeep then
                        Dice.dieFromPair ( pipsToKeep, Keep )

                    else
                        Dice.dieFromPair ( Dice.pips die, Reroll )
                )
                diceList

        keepStraight straight =
            let
                straighten ( set, old, new ) =
                    case old of
                        [] ->
                            ( set, old, new )

                        die :: tail ->
                            let
                                face =
                                    Dice.pips die

                                ( newSet, newDie ) =
                                    if Set.member face set then
                                        ( Set.remove face set, Dice.dieFromPair ( face, Keep ) )

                                    else
                                        ( set, Dice.dieFromPair ( face, Reroll ) )
                            in
                            straighten ( newSet, tail, newDie :: new )

                wanted =
                    Set.fromList straight

                ( _, _, dice ) =
                    straighten ( wanted, diceList, [] )
            in
            List.reverse dice
    in
    FiveDice
        (case suggested of
            OfAKind pipsToKeep ->
                keepByPips pipsToKeep

            Straight straight ->
                keepStraight straight
        )


keepOnly : DiceToKeep -> DiceBoard -> DiceBoard
keepOnly suggestion diceBoard =
    Maybe.map (justKeepOnly suggestion) diceBoard



-- For ease in testing


makeDiceBoard : List ( Int, NextRoll ) -> DiceBoard
makeDiceBoard raw =
    if List.length raw == numberOfDice then
        Just <| FiveDice <| Dice.fromPairs raw

    else
        Nothing
