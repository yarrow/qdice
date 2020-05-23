module ScorePad exposing
    ( CanUse(..)
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
    ( CanUse, String )


type CanUse
    = Vacant ( Rank, Int )
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


activeScorePad : Scores -> PipsList -> List ScorePadRow
activeScorePad scores pipsList =
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
                            ( Vacant ( rank, column ), String.fromInt pointsForThisRoll )

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


getBox : Rank -> Int -> Scores -> ScoreBox
getBox rank column scores =
    Debug.todo "getBox"


setBox : CanUse -> Int -> Scores -> Scores
setBox location points scores =
    case location of
        InUse ->
            scores

        Vacant ( rank, column ) ->
            Debug.todo "setBox"
