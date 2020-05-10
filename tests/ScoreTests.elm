module ScoreTests exposing (..)

import Expect exposing (Expectation)
import Score exposing (..)
import ScoreTags exposing (..)
import Set
import Test exposing (..)


onClickSuite : Test
onClickSuite =
    describe "Location of (Just) onClicks" <|
        [ test "An inert ScoreDisplay has no onClicks" <|
            \_ ->
                let
                    inactive =
                        inert initialScorePad

                    boxes =
                        List.concatMap .boxes inactive

                    clicks =
                        List.map .onClick boxes
                in
                List.all (\it -> it == Nothing) clicks
                    |> Expect.true "Found an onClick in (inert initialScorePad)"
        ]


allTags : Test
allTags =
    test "scoreDisplay has all the tags" <|
        \_ ->
            let
                allTheTags =
                    Set.fromList [ ones, twos, threes, fours, fives, sixes, upperTotal, bonus, threeOfAKind, fourOfAKind, fullHouse, smallStraight, largeStraight, fiveOfAKind, chance, total, weighted, grandTotal ]
            in
            Set.fromList scoreOrder |> Expect.equalSets allTheTags
