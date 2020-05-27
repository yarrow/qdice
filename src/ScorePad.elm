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
    , numberOfTurns
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
                    List.map (List.map (Maybe.withDefault 0)) sectionRows

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

        wholeMegilla =
            List.foldr (+) 0 withWeights

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
        , [ sumRow totalScore rowTotal
          , sumRow weightedScore withWeights
          , sumRow grandTotal [ wholeMegilla ]
          ]
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
            , boxes = List.map inUse (getScoreRow rank scores)
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
                    getScoreRow rank scores

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
    List ScoreBox


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
    List.repeat 3 Nothing


emptyScores : Scores
emptyScores =
    Scores (Array.fromList (List.repeat numberOfRanks threeNothings))


numberOfTurns : Int
numberOfTurns =
    case emptyScores of
        Scores scores ->
            Array.length scores * List.length threeNothings


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
    getScoreRow rank scores
        |> Array.fromList
        |> Array.get column
        |> Maybe.withDefault Nothing


setScoreBox : Location -> ScoreBox -> Scores -> Scores
setScoreBox ( rank, column ) scoreBox (Scores scores) =
    let
        row =
            getScoreRow rank (Scores scores)
                |> Array.fromList
                |> Array.set column scoreBox
                |> Array.toList
    in
    Scores (Array.set (Rank.toInt rank) row scores)
