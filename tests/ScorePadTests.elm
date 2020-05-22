module ScorePadTests exposing (scorePadTests)

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


aPipsList =
    [ 1, 3, 2, 5, 6 ]


scorePadTests : Test
scorePadTests =
    describe "ScorePad tests" <|
        [ test "Every box in staticScorePad emptyScores is (InUse, '')" <|
            \_ ->
                let
                    boxes =
                        List.concatMap .boxes (staticScorePad emptyScores)
                in
                List.all (\box -> box == ( InUse, "" )) boxes
                    |> Expect.true "Every box should be (InUse, '')"
        , test "Each staticScorePad box has the correct caption" <|
            \_ ->
                List.map .caption (staticScorePad emptyScores)
                    |> Expect.equalLists theCaptions
        , test "Each activeScorePad box has the correct caption" <|
            \_ ->
                List.map .caption (activeScorePad emptyScores aPipsList)
                    |> Expect.equalLists theCaptions
        ]
