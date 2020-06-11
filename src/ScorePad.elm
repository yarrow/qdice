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
import Rank exposing (Rating(..), rankInfo)
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
    { occupancy : Occupancy, rating : Rating, score : String }


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

        currentPar =
            let
                hed =
                    Maybe.withDefault Nothing << List.head

                upper =
                    Rank.upper scores

                c1 =
                    List.map hed upper

                c2 =
                    List.map hed (List.map (List.drop 1) upper)

                c3 =
                    List.map hed (List.map (List.drop 2) upper)
            in
            List.map Rank.columnPar [ c1, c2, c3 ]

        bonusBox par total =
            if total >= Rank.maxPar then
                { occupancy = InUse
                , rating = Ample
                , score = "35"
                }

            else if total > par then
                { occupancy = InUse
                , rating = Ample
                , score = "⬆︎" ++ String.fromInt (total - par)
                }

            else if total == par then
                { occupancy = InUse
                , rating = Sufficient
                , score = ""
                }

            else
                { occupancy = InUse
                , rating = Meager
                , score = "⬇︎" ++ String.fromInt (par - total)
                }

        bonusBoxes =
            List.map2 bonusBox currentPar topTotal

        bonusScores =
            List.map
                (\total ->
                    if total >= Rank.maxPar then
                        35

                    else
                        0
                )
                topTotal

        rowTotal =
            List.map3 (\a b c -> a + b + c) topTotal bonusScores bottomTotal

        withWeights =
            List.map2 (*) [ 1, 2, 3 ] rowTotal

        wholeMegilla =
            List.foldr (+) 0 withWeights

        inUse n =
            { occupancy = InUse
            , rating = Sufficient
            , score = String.fromInt n
            }

        sumRow caption row =
            { caption = caption
            , kind = Calculated
            , boxes = List.map (\n -> inUse n) row
            }
    in
    List.concat
        [ Rank.upper scorePadRows
        , [ sumRow upperTotal topTotal
          , { caption = upperBonus, kind = Calculated, boxes = bonusBoxes }
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
        inUse rating box =
            let
                rated =
                    Maybe.withDefault Sufficient (Maybe.map rating box)
            in
            { occupancy = InUse
            , rating = rated
            , score = boxToString box
            }

        staticRow { caption, rating } boxes =
            { kind = Rolled
            , caption = caption
            , boxes = List.map (inUse rating) boxes
            }
    in
    List.map2 staticRow rankInfo scores


activeRows : List Pip -> List Score.Row -> List Row
activeRows pips scores =
    let
        counted =
            Rank.countPips pips

        activeRow { rank, caption, fn, rating } boxes =
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
                            { occupancy = Available ( rank, column )
                            , rating = rating pointsForThisRoll
                            , score = blankIfZero pointsForThisRoll
                            }

                        Just points ->
                            { occupancy = InUse
                            , rating = rating points
                            , score = String.fromInt points
                            }
            in
            { caption = caption
            , kind = Rolled
            , boxes = List.indexedMap makeBox boxes
            }
    in
    List.map2 activeRow rankInfo scores
