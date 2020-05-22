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


activeScorePad : Scores -> PipsList -> List ScorePadRow
activeScorePad scores pips =
    staticScorePad scores


staticScorePad : Scores -> List ScorePadRow
staticScorePad scores =
    let
        inUse box =
            ( InUse, boxToString box )

        staticRow rank =
            { caption = caption rank, boxes = List.map inUse (getRow rank scores) }
    in
    List.map staticRow allRanks



----- private


activeRow : Rank -> Scores -> scorePadRow
activeRow rank scores =
    Debug.todo "activeRow"



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
    Maybe.withDefault threeNothings (Array.get (rankToInt rank) scores)


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


uppers : List Rank
uppers =
    [ Ones, Twos, Threes, Fours, Fives, Sixes ]


lowers : List Rank
lowers =
    [ ThreeOfAKind, FourOfAKind, FullHouse, SmallStraight, LargeStraight, FiveOfAKind, Chance ]


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


allRanks : List Rank
allRanks =
    [ Ones
    , Twos
    , Threes
    , Fours
    , Fives
    , Sixes
    , ThreeOfAKind
    , FourOfAKind
    , FullHouse
    , SmallStraight
    , LargeStraight
    , FiveOfAKind
    , Chance
    ]


tally : Rank -> PipsList -> Int
tally =
    Debug.todo "tally"



{-
   goodness : Rank -> Int -> Order
   goodness =
       ...
-}


caption : Rank -> String
caption rank =
    case rank of
        Ones ->
            "Ones"

        Twos ->
            "Twos"

        Threes ->
            "Threes"

        Fours ->
            "Fours"

        Fives ->
            "Fives"

        Sixes ->
            "Sixes"

        ThreeOfAKind ->
            "3 of a kind"

        FourOfAKind ->
            "4 of a kind"

        FullHouse ->
            "Full House"

        SmallStraight ->
            "Sm Strght"

        LargeStraight ->
            "Lg Strght"

        FiveOfAKind ->
            "5 of a kind"

        Chance ->
            "Chance"


rankToInt : Rank -> Int
rankToInt rank =
    case rank of
        Ones ->
            0

        Twos ->
            1

        Threes ->
            2

        Fours ->
            3

        Fives ->
            4

        Sixes ->
            5

        ThreeOfAKind ->
            6

        FourOfAKind ->
            7

        FullHouse ->
            8

        SmallStraight ->
            9

        LargeStraight ->
            10

        FiveOfAKind ->
            11

        Chance ->
            12


numberOfRanks : Int
numberOfRanks =
    13
