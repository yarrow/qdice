module ScorePad exposing
    ( Box
    , Occupancy(..)
    , Row
    , RowKind(..)
    , ScorePad
    , grandTotal
    , makeScorePad
    , totalScore
    , upperBonus
    , upperTotal
    , weightedScore
    )

import Pip exposing (Pip)
import Rank
import Score exposing (Location, Scores)



----- ScorePad
-- Total row captions


upperTotal : String
upperTotal =
    "Upper"


upperBonus : String
upperBonus =
    "Bonus"


totalScore : String
totalScore =
    "Combined"


weightedScore : String
weightedScore =
    "x1 x2 x3"


grandTotal : String
grandTotal =
    "Total"


boxToString : Score.Box -> String
boxToString box =
    case box of
        Nothing ->
            ""

        Just n ->
            String.fromInt n


type alias ScorePad =
    List Row


type alias Row =
    { caption : String
    , kind : RowKind
    , boxes : List Box
    }


type alias Box =
    ( Occupancy, String )


type Occupancy
    = Available Location
    | InUse


type RowKind
    = Rolled
    | Calculated



-- Uses staticRows if there are no pips to score, and activeRows if there are
-- section totals are the same either way


makeScorePad : Maybe (List Pip) -> Scores -> ScorePad
makeScorePad pips scoreRows =
    let
        scores =
            Score.toRows scoreRows

        scorePadRows =
            case pips of
                Nothing ->
                    staticRows scores

                Just pipList ->
                    activeRows pipList scores

        getSectionTotal : List Score.Row -> List Int
        getSectionTotal sectionRows =
            let
                section : List (List Int)
                section =
                    List.map (List.map (Maybe.withDefault 0)) sectionRows

                addRows a b =
                    List.map2 (+) a b
            in
            List.foldr addRows [ 0, 0, 0 ] section

        topTotal =
            getSectionTotal (Rank.upper scores)

        bottomTotal =
            getSectionTotal (Rank.lower scores)

        bonus =
            List.map
                (\t ->
                    if t >= 63 then
                        35

                    else
                        0
                )
                topTotal

        rowTotal =
            List.map3 (\a b c -> a + b + c) topTotal bonus bottomTotal

        withWeights =
            List.map2 (*) [ 1, 2, 3 ] rowTotal

        wholeMegilla =
            List.foldr (+) 0 withWeights

        sumRow caption row =
            { caption = caption
            , kind = Calculated
            , boxes = List.map (\n -> ( InUse, String.fromInt n )) row
            }
    in
    List.concat
        [ Rank.upper scorePadRows
        , [ sumRow upperTotal topTotal
          , sumRow upperBonus bonus
          ]
        , Rank.lower scorePadRows
        , [ sumRow totalScore rowTotal
          , sumRow weightedScore withWeights
          , sumRow grandTotal [ wholeMegilla ]
          ]
        ]


staticRows : List Score.Row -> List Row
staticRows scores =
    let
        inUse box =
            ( InUse, boxToString box )

        staticRow caption boxes =
            { kind = Rolled
            , caption = caption
            , boxes = List.map inUse boxes
            }
    in
    List.map2 staticRow Rank.captions scores


activeRows : List Pip -> List Score.Row -> List Row
activeRows pips scores =
    let
        counted =
            Rank.countPips pips

        activeRow rank caption fn boxes =
            let
                pointsForThisRoll : Int
                pointsForThisRoll =
                    fn counted

                blankIfZero n =
                    if n == 0 then
                        "\u{00A0}"

                    else
                        String.fromInt n

                makeBox column box =
                    case box of
                        Nothing ->
                            ( Available ( rank, column ), blankIfZero pointsForThisRoll )

                        Just points ->
                            ( InUse, String.fromInt points )
            in
            { caption = caption
            , kind = Rolled
            , boxes = List.indexedMap makeBox boxes
            }
    in
    List.map4 activeRow Rank.ranks Rank.captions Rank.fns scores
