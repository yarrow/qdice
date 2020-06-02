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

import Array
import Pip exposing (Pip)
import Rank
    exposing
        ( Rank(..)
        , numberOfRanks
        , toInt
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


staticScorePad : Scores -> ScorePad
staticScorePad (Scores scores) =
    makeScorePad staticScorePadRows scores


activeScorePad : List Pip -> Scores -> ScorePad
activeScorePad pipList (Scores scores) =
    makeScorePad (activeScorePadRows pipList) scores


makeScorePad : (List ScoreRow -> List ScorePadRow) -> List ScoreRow -> ScorePad
makeScorePad padRows scores =
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

        scorePadRows =
            padRows scores
    in
    List.concat
        [ Rank.upper scorePadRows
        , [ sumRow upperTotal topTotal, sumRow upperBonus bonus ]
        , Rank.lower scorePadRows
        , [ sumRow totalScore rowTotal
          , sumRow weightedScore withWeights
          , sumRow grandTotal [ wholeMegilla ]
          ]
        ]


staticScorePadRows : List ScoreRow -> List ScorePadRow
staticScorePadRows scores =
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


activeScorePadRows : List Pip -> List ScoreRow -> List ScorePadRow
activeScorePadRows pips scores =
    let
        counted =
            Rank.countPips pips

        activeRow rank caption fn boxes =
            let
                pointsForThisRoll : Int
                pointsForThisRoll =
                    fn counted

                makeBox column box =
                    case box of
                        Nothing ->
                            ( Available ( rank, column ), String.fromInt pointsForThisRoll )

                        Just points ->
                            ( InUse, String.fromInt points )
            in
            { caption = caption
            , kind = Rolled
            , boxes = List.indexedMap makeBox boxes
            }
    in
    List.map4 activeRow Rank.ranks Rank.captions Rank.fns scores



---- Scores


type alias ScoreBox =
    Maybe Int


type alias ScoreRow =
    List ScoreBox


type Scores
    = Scores (List ScoreRow)


makeScoresForTesting : List ScoreRow -> Scores
makeScoresForTesting scoreRows =
    if List.length scoreRows == numberOfRanks then
        Scores scoreRows

    else
        -- make sure the error shows up early
        Scores []


threeNothings : ScoreRow
threeNothings =
    List.repeat 3 Nothing


emptyScores : Scores
emptyScores =
    Scores (List.repeat numberOfRanks threeNothings)


numberOfTurns : Int
numberOfTurns =
    case emptyScores of
        Scores scores ->
            List.length scores * List.length threeNothings


boxToString : ScoreBox -> String
boxToString box =
    case box of
        Nothing ->
            ""

        Just n ->
            String.fromInt n


setScoreBox : Location -> ScoreBox -> Scores -> Scores
setScoreBox ( rank, column ) scoreBox (Scores scores) =
    let
        scoreArray =
            Array.fromList scores

        r =
            Rank.toInt rank

        row =
            Maybe.withDefault threeNothings (Array.get r scoreArray)

        newRow =
            row |> Array.fromList |> Array.set column scoreBox |> Array.toList
    in
    Array.set r newRow scoreArray
        |> Array.toList
        |> Scores


getScoreBox : Location -> Scores -> ScoreBox
getScoreBox ( rank, column ) (Scores scores) =
    let
        row =
            List.drop (Rank.toInt rank) scores
                |> List.head
                |> Maybe.withDefault threeNothings
    in
    Maybe.withDefault Nothing (Array.get column (Array.fromList row))
