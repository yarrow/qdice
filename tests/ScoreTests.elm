module ScoreTests exposing (..)

import Expect exposing (Expectation)
import Score exposing (..)
import ScoreTags exposing (..)
import Set
import Test exposing (..)


allTags : Test
allTags =
    test "scoreOrder has all the tags" <|
        \_ ->
            let
                allTheTags =
                    Set.fromList [ ones, twos, threes, fours, fives, sixes, upperTotal, bonus, threeOfAKind, fourOfAKind, fullHouse, smallStraight, largeStraight, fiveOfAKind, chance, total, weighted, grandTotal ]
            in
            Set.fromList scoreOrder |> Expect.equalSets allTheTags
