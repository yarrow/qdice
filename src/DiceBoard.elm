module DiceBoard exposing
    ( DiceBoard
    , display
    , empty
    , flipNextRoll
    , hasRerolls
    , keepOnly
    , makeDiceBoard
    , mergeDice
    , rollForNewDice
    , suggestions
    , toDiceList
    , toPips
    )

import Dice exposing (DiceList, Die, NextRoll(..))
import Pip exposing (Pip)
import Random exposing (Generator)
import Rank exposing (DiceToKeep(..))
import Set


toPips : DiceBoard -> Maybe (List Pip)
toPips (DiceBoard diceList) =
    Maybe.map (List.map .pips) diceList



-- This module (pretends to) enforce the invariant that a DiceBoard has either
-- zero dice — is (DiceBoard Nothing) or five dice. See the mergeDice comment
-- below for a place that we can't check this easily.


type DiceBoard
    = DiceBoard (Maybe DiceList)


empty : DiceBoard
empty =
    DiceBoard Nothing


numberOfDice : Int
numberOfDice =
    5


display : a -> (Int -> Die -> a) -> DiceBoard -> List a
display emptyRow makeRow (DiceBoard board) =
    case board of
        Nothing ->
            List.repeat numberOfDice emptyRow

        Just theDice ->
            List.indexedMap makeRow theDice


rerollCount : DiceBoard -> Int
rerollCount (DiceBoard board) =
    case board of
        Nothing ->
            numberOfDice

        Just dice ->
            Dice.rerollCount dice



-- Note that `mergeDice Nothing` depends on the incoming List Pip to
-- have been generated from rollForNewDice.  If this is NOT the case,
-- then the invariant that a FiveDice has either 0 or 5 dice will be
-- broken. I can't think of a way around this that doesn't make things
-- worse.


mergeDice : List Pip -> DiceBoard -> DiceBoard
mergeDice incoming (DiceBoard current) =
    let
        dice =
            case current of
                Nothing ->
                    Dice.fromPips incoming

                Just oldDice ->
                    Dice.mergeDice incoming oldDice
    in
    DiceBoard (Just dice)


rollForNewDice : DiceBoard -> Generator (List Pip)
rollForNewDice diceBoard =
    Random.list (rerollCount diceBoard) Pip.randomPip


hasRerolls : DiceBoard -> Bool
hasRerolls board =
    rerollCount board > 0


flipNextRoll : Int -> DiceBoard -> DiceBoard
flipNextRoll j (DiceBoard dice) =
    DiceBoard (Maybe.map (Dice.flipNextRoll j) dice)


suggestions : DiceBoard -> List ( DiceToKeep, List String )
suggestions diceBoard =
    case diceBoard of
        DiceBoard Nothing ->
            []

        DiceBoard (Just dice) ->
            let
                keepSets =
                    Rank.suggestKeeping (toPips diceBoard)

                keptUrls diceToKeep =
                    justKeepOnly diceToKeep dice
                        |> List.filter (\die -> die.nextRoll == Keep)
                        |> List.map Dice.urlSmall
                        |> List.sort
            in
            List.map (\keep -> ( keep, keptUrls keep )) keepSets


type alias Args =
    ( Set.Set Int, DiceList, DiceList )


justKeepOnly : DiceToKeep -> DiceList -> DiceList
justKeepOnly suggested diceList =
    let
        keepByPips pipsToKeep =
            List.map
                (\die ->
                    { die
                        | nextRoll =
                            if Pip.toInt die.pips == pipsToKeep then
                                Keep

                            else
                                Reroll
                    }
                )
                diceList

        keepStraight straight =
            let
                straighten : Args -> Args
                straighten ( set, old, new ) =
                    case old of
                        [] ->
                            ( set, old, new )

                        die :: tail ->
                            let
                                face =
                                    Pip.toInt die.pips

                                ( newSet, newDie ) =
                                    if Set.member face set then
                                        ( Set.remove face set, { die | nextRoll = Keep } )

                                    else
                                        ( set, { die | nextRoll = Reroll } )
                            in
                            straighten ( newSet, tail, newDie :: new )

                wanted =
                    Set.fromList straight

                ( _, _, dice ) =
                    straighten ( wanted, diceList, [] )
            in
            List.reverse dice
    in
    case suggested of
        OfAKind pipsToKeep ->
            keepByPips pipsToKeep

        Straight straight ->
            keepStraight straight


keepOnly : DiceToKeep -> DiceBoard -> DiceBoard
keepOnly suggestion (DiceBoard diceBoard) =
    DiceBoard (Maybe.map (justKeepOnly suggestion) diceBoard)



-- For ease in testing


makeDiceBoard : List ( Int, NextRoll ) -> DiceBoard
makeDiceBoard raw =
    DiceBoard <|
        if List.length raw == numberOfDice then
            Just <| Dice.fromPairs raw

        else
            Nothing


toDiceList : DiceBoard -> Maybe DiceList
toDiceList (DiceBoard diceList) =
    diceList
