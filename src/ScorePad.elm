module ScorePad exposing
    ( Location
    , Occupancy(..)
    , ScorePad
    , ScorePadBox
    , ScorePadRow
    , Scores
    , activeScorePad
    , emptyScores
    , staticScorePad
    )

import Array exposing (Array)
import Debug
import Dice exposing (PipsList(..))
import Rank exposing (Rank(..), allRanks, caption, numberOfRanks, tally, toInt)



----- ScorePad


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


staticScorePad : Scores -> List ScorePadRow
staticScorePad scores =
    let
        inUse box =
            ( InUse, boxToString box )

        staticRow rank =
            { caption = caption rank, boxes = List.map inUse (getRow rank scores) }
    in
    List.map staticRow allRanks


activeScorePad : PipsList -> Scores -> List ScorePadRow
activeScorePad pipsList scores =
    let
        counted =
            Rank.countPips pipsList

        activeRow rank =
            let
                current =
                    getRow rank scores

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
    List.map activeRow allRanks



---- Scores


type alias ScoreBox =
    Maybe Int


type alias ScoreRow =
    List ScoreBox


type Scores
    = Scores (Array ScoreRow)


threeNothings : ScoreRow
threeNothings =
    List.repeat 3 Nothing


emptyScores : Scores
emptyScores =
    Scores (Array.fromList (List.repeat numberOfRanks threeNothings))


scoreBoxToInt : ScoreBox -> Int
scoreBoxToInt box =
    Maybe.withDefault 0 box


boxToString : ScoreBox -> String
boxToString box =
    case box of
        Nothing ->
            ""

        Just n ->
            String.fromInt n


getRow : Rank -> Scores -> ScoreRow
getRow rank (Scores scores) =
    Maybe.withDefault threeNothings (Array.get (Rank.toInt rank) scores)


setBox : Occupancy -> Int -> Scores -> Scores
setBox location points scores =
    case location of
        InUse ->
            scores

        Available ( rank, column ) ->
            Debug.todo "setBox"
