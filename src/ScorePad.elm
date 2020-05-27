module ScorePad exposing
    ( Location
    , Occupancy(..)
    , RowKind(..)
    , ScorePad
    , ScorePadBox
    , ScorePadRow
    , Scores
    , activeScorePad
    , emptyScores
    , getScoreBox
    , grandTotal
    , makeScoresForTesting
    , setScoreBox
    , staticScorePad
    , totalScore
    , upperBonus
    , upperTotal
    , weightedScore
    )

import Array exposing (Array)
import Dice exposing (PipsList(..))
import Rank
    exposing
        ( Rank(..)
        , caption
        , lowerRanks
        , numberOfRanks
        , numberOfUppers
        , tally
        , toInt
        , upperRanks
        )



----- ScorePad
-- Total row captions


upperTotal : String
upperTotal =
    "Total"


upperBonus : String
upperBonus =
    "Bonus"


totalScore : String
totalScore =
    "Total Score"


weightedScore : String
weightedScore =
    "Weighted Score"


grandTotal : String
grandTotal =
    "Grand Total"


type alias ScorePad =
    List ScorePadRow


type alias ScorePadRow =
    { caption : String
    , kind : RowKind
    , boxes : List ScorePadBox
    }


type alias ScorePadBox =
    ( Occupancy, String )


type alias Location =
    ( Rank, Int )


type Occupancy
    = Available Location
    | InUse


type RowKind
    = Rolled
    | Calculated


makeScorePad : (List Rank -> Scores -> List ScorePadRow) -> Scores -> ScorePad
makeScorePad scoreRows (Scores scores) =
    let
        getSectionTotal : List ScoreRow -> List Int
        getSectionTotal sectionRows =
            let
                section : List (List Int)
                section =
                    sectionRows
                        |> List.map Array.toList
                        |> List.map (List.map (Maybe.withDefault 0))

                addRows a b =
                    List.map2 (+) a b
            in
            List.foldr addRows [ 0, 0, 0 ] section

        scoresList =
            Array.toList scores

        topTotal =
            getSectionTotal (List.take numberOfUppers scoresList)

        bottomTotal =
            getSectionTotal (List.drop numberOfUppers scoresList)

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

        sumRow caption row =
            { caption = caption
            , kind = Calculated
            , boxes = List.map (\n -> ( InUse, String.fromInt n )) row
            }
    in
    List.concat
        [ scoreRows upperRanks (Scores scores)
        , [ sumRow upperTotal topTotal, sumRow upperBonus bonus ]
        , scoreRows lowerRanks (Scores scores)
        , [ sumRow totalScore rowTotal, sumRow weightedScore withWeights ]
        ]


staticScorePad : Scores -> ScorePad
staticScorePad scores =
    makeScorePad staticScoreRows scores


activeScorePad : PipsList -> Scores -> ScorePad
activeScorePad pipsList scores =
    makeScorePad (activeScoreRows pipsList) scores


staticScoreRows : List Rank -> Scores -> List ScorePadRow
staticScoreRows ranks scores =
    let
        inUse box =
            ( InUse, boxToString box )

        staticRow rank =
            { kind = Rolled
            , caption = caption rank
            , boxes = List.map inUse (getScoreBoxList rank scores)
            }
    in
    List.map staticRow ranks


activeScoreRows : PipsList -> List Rank -> Scores -> List ScorePadRow
activeScoreRows pipsList ranks scores =
    let
        counted =
            Rank.countPips pipsList

        activeRow rank =
            let
                current =
                    getScoreBoxList rank scores

                pointsForThisRoll =
                    tally rank counted

                makeBox column box =
                    case box of
                        Nothing ->
                            ( Available ( rank, column ), String.fromInt pointsForThisRoll )

                        Just points ->
                            ( InUse, String.fromInt points )
            in
            { caption = caption rank
            , kind = Rolled
            , boxes = List.indexedMap makeBox current
            }
    in
    List.map activeRow ranks



---- Scores


type alias ScoreBox =
    Maybe Int


type alias ScoreRow =
    Array ScoreBox


type Scores
    = Scores (Array ScoreRow)


makeScoresForTesting : Array ScoreRow -> Scores
makeScoresForTesting scoreRows =
    if Array.length scoreRows == numberOfRanks then
        Scores scoreRows

    else
        -- make sure the error shows up early
        Scores (Array.fromList [])


threeNothings : ScoreRow
threeNothings =
    Array.fromList (List.repeat 3 Nothing)


emptyScores : Scores
emptyScores =
    Scores (Array.fromList (List.repeat numberOfRanks threeNothings))


boxToString : ScoreBox -> String
boxToString box =
    case box of
        Nothing ->
            ""

        Just n ->
            String.fromInt n


getScoreRow : Rank -> Scores -> ScoreRow
getScoreRow rank (Scores scores) =
    Maybe.withDefault threeNothings (Array.get (Rank.toInt rank) scores)


getScoreBox : Location -> Scores -> ScoreBox
getScoreBox ( rank, column ) scores =
    Maybe.withDefault Nothing (getScoreRow rank scores |> Array.get column)


getScoreBoxList : Rank -> Scores -> List ScoreBox
getScoreBoxList rank (Scores scores) =
    Array.toList (Maybe.withDefault threeNothings (Array.get (Rank.toInt rank) scores))


setScoreBox : Location -> ScoreBox -> Scores -> Scores
setScoreBox ( rank, column ) scoreBox (Scores scores) =
    let
        row =
            getScoreRow rank (Scores scores)
                |> Array.set column scoreBox
    in
    Scores (Array.set (Rank.toInt rank) row scores)
