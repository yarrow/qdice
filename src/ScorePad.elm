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
import Dice exposing (PipsList)



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


activeScorePad : Scores -> List ScorePadRow
activeScorePad =
    Debug.todo "activeScorePad"


staticScorePad : Scores -> List ScorePadRow
staticScorePad scores =
    let
        emptyBox =
            ( InUse, "" )
    in
    List.repeat 13
        { caption = "Sm Straight", boxes = List.repeat 3 emptyBox }



----- private


activeRow : Rank -> Scores -> scorePadRow
activeRow rank scores =
    Debug.todo "activeRow"


staticRow : Rank -> Scores -> scorePadRow
staticRow rank scores =
    Debug.todo "staticRow"



-- activeBox is a box that's part of an active ScorePad,
-- not necessarily itself available for input


activeBox : Rank -> Int -> Scores -> PipsList -> ScorePadBox
activeBox rank column scores pipsList =
    let
        current =
            getBox rank column scores

        ( status, points ) =
            case current of
                Nothing ->
                    ( Vacant ( rank, column ), tally rank pipsList )

                Just pts ->
                    ( InUse, pts )
    in
    ( status, String.fromInt points )



---- Scores


type alias ScoreBox =
    Maybe Int


type alias ScoreRow =
    Array ScoreBox


type Scores
    = Scores (Array ScoreRow)


emptyScores : Scores
emptyScores =
    Scores (Array.fromList [])


toInt : ScoreBox -> Int
toInt box =
    Maybe.withDefault 0 box


toString : ScoreBox -> String
toString box =
    case box of
        Nothing ->
            ""

        Just n ->
            String.fromInt n


getRow : Rank -> Scores -> ScoreRow
getRow rank scores =
    Debug.todo "Scores.get"


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



---- Rank


type Rank
    = Ones
    | Twos
    | Threes
    | Fours
    | Fives
    | Sixes
    | ThreeOfAKind
    | FourOfAKind
    | FullHouse
    | SmallStraight
    | LargeStraight
    | FiveOfAKind
    | Chance


tally : Rank -> PipsList -> Int
tally =
    Debug.todo "tally"



{-
   goodness : Rank -> Int -> Order
   goodness =
       Debug.todo "goodness"
-}


caption : Rank -> String
caption =
    Debug.todo "caption"
