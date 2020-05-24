module ScorePadTests exposing (scorePadTests)

import Dice exposing (PipsList(..))
import Expect
import ScorePad
    exposing
        ( CanUse(..)
        , ScorePad
        , ScorePadBox
        , ScorePadRow
        , Scores
        , activeScorePad
        , emptyScores
        , staticScorePad
        )
import Test exposing (..)


theCaptions =
    [ "Ones"
    , "Twos"
    , "Threes"
    , "Fours"
    , "Fives"
    , "Sixes"
    , "3 of a kind"
    , "4 of a kind"
    , "Full House"
    , "Sm Strght"
    , "Lg Strght"
    , "5 of a kind"
    , "Chance"
    ]


aPipsList : PipsList
aPipsList =
    PipsList [ 1, 3, 2, 5, 6 ]


allBoxes : ScorePad -> List ScorePadBox
allBoxes scores =
    List.concatMap .boxes scores


emptyStatic : ScorePad
emptyStatic =
    staticScorePad emptyScores


emptyActive : ScorePad
emptyActive =
    activeScorePad aPipsList emptyScores


isVacant : CanUse -> Bool
isVacant canUse =
    case canUse of
        InUse ->
            False

        Vacant _ ->
            True


scorePadTests : Test
scorePadTests =
    describe "ScorePad tests" <|
        [ test "Every box in staticScorePad emptyScores is (InUse, '')" <|
            \_ ->
                List.all (\box -> box == ( InUse, "" )) (allBoxes emptyStatic)
                    |> Expect.true "Every box should be (InUse, '')"
        , test "Each staticScorePad box has the correct caption" <|
            \_ ->
                List.map .caption emptyStatic
                    |> Expect.equalLists theCaptions
        , test "Each activeScorePad box has the correct caption" <|
            \_ ->
                List.map .caption emptyActive
                    |> Expect.equalLists theCaptions
        , test "Every box in ScorePad emptyScores is Vacant" <|
            \_ ->
                List.all (\box -> isVacant (Tuple.first box)) (allBoxes emptyActive)
                    |> Expect.true "Every box should be Vacant"
        ]
