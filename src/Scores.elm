module Scores exposing
    ( Location
    , Box
    , Row
    , Scores
    , emptyScores
    , getBox
    , makeScoresForTesting
    , numberOfTurns
    , setBox
    , toRows
    )

import Array
import Rank exposing (Rank, numberOfRanks)


type alias Box =
    Maybe Int


type alias Row =
    List Box


type Scores
    = Scores (List Row)


toRows : Scores -> List Row
toRows (Scores scores) =
    scores


type alias Location =
    ( Rank, Int )


makeScoresForTesting : List Row -> Scores
makeScoresForTesting scoreRows =
    if List.length scoreRows == numberOfRanks then
        Scores scoreRows

    else
        -- make sure the error shows up early
        Scores []


threeNothings : Row
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


setBox : Location -> Box -> Scores -> Scores
setBox ( rank, column ) scoreBox (Scores scores) =
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


getBox : Location -> Scores -> Box
getBox ( rank, column ) (Scores scores) =
    let
        row =
            List.drop (Rank.toInt rank) scores
                |> List.head
                |> Maybe.withDefault threeNothings
    in
    Maybe.withDefault Nothing (Array.get column (Array.fromList row))
