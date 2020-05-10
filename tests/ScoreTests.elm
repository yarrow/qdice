module ScoreTests exposing (..)

import Expect exposing (Expectation)
import Score
import ScoreTags exposing (..)
import Set exposing (Set)
import Test exposing (..)


inertDisplay : Score.Display
inertDisplay =
    Score.inert Score.initialPad


onClickSuite : Test
onClickSuite =
    describe "Location of (Just) onClicks" <|
        [ test "An inert Display has no onClicks" <|
            \_ ->
                let
                    boxes =
                        List.concatMap .boxes inertDisplay

                    clicks =
                        List.map .onClick boxes
                in
                List.all (\it -> it == Nothing) clicks
                    |> Expect.true "Found an onClick in inertDisplay"
        ]


expectedTags : List String
expectedTags =
    [ ones, twos, threes, fours, fives, sixes, upperTotal, bonus, threeOfAKind, fourOfAKind, fullHouse, smallStraight, largeStraight, fiveOfAKind, chance, total, weighted, grandTotal ]


tagsOf : Score.Display -> List String
tagsOf display =
    List.map .tag display


allTags : Test
allTags =
    test "inert scoreDisplay has all the tags" <|
        \_ ->
            tagsOf inertDisplay
                |> Expect.equal expectedTags
