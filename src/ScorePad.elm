module ScorePad exposing
    ( Location
    , Occupancy(..)
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
    , weightScore
    )

import Array exposing (Array)
import Dice exposing (PipsList(..))
import Rank
    exposing
        ( Rank(..)
        , allRanks
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


weightScore : String
weightScore =
    "Weighted Score"


grandTotal : String
grandTotal =
    "Grand Total"


type alias ScorePad =
    List ScorePadRow


type alias ScorePadRow =
    { caption : String
    , boxes : List ScorePadBox
    }


type alias ScorePadBox =
    ( Occupancy, String )


type alias Location =
    ( Rank, Int )


type Occupancy
    = Available Location
    | InUse


staticScoreRows : List Rank -> Scores -> ScorePad
staticScoreRows ranks scores =
    let
        inUse box =
            ( InUse, boxToString box )

        staticRow rank =
            { caption = caption rank, boxes = List.map inUse (getScoreBoxList rank scores) }
    in
    List.map staticRow ranks


staticScorePad : Scores -> ScorePad
staticScorePad scores =
    staticScoreRows upperRanks scores ++ staticScoreRows lowerRanks scores


activeScoreRows : List Rank -> PipsList -> Scores -> ScorePad
activeScoreRows ranks pipsList scores =
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
            , boxes = List.indexedMap makeBox current
            }
    in
    List.map activeRow ranks


activeScorePad : PipsList -> Scores -> ScorePad
activeScorePad pipsList scores =
    activeScoreRows upperRanks pipsList scores ++ activeScoreRows lowerRanks pipsList scores



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
