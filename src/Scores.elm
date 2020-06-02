module Scores exposing
    ( Location
    , ScoreBox
    , ScoreRow
    , Scores
    , emptyScores
    , getScoreBox
    , makeScoresForTesting
    , numberOfTurns
    , setScoreBox
    , toRows
    )

import Array
import Rank exposing (Rank, numberOfRanks)


type alias ScoreBox =
    Maybe Int


type alias ScoreRow =
    List ScoreBox


type Scores
    = Scores (List ScoreRow)


toRows : Scores -> List ScoreRow
toRows (Scores scores) =
    scores


type alias Location =
    ( Rank, Int )


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
