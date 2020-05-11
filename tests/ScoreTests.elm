module ScoreTests exposing (..)

import Dict exposing (Dict)
import Expect exposing (Expectation)
import Score
import ScoreTags exposing (..)
import Set exposing (Set)
import Test exposing (..)


dictify : Score.Display -> Dict String Score.DisplayRow
dictify display =
    Dict.fromList (List.map (\row -> ( row.tag, row )) display)


inertDisplay : Score.Display
inertDisplay =
    Score.inert Score.initialPad


activeDisplay : Score.Display
activeDisplay =
    Score.active Score.initialPad


clicks : Score.Display -> List (Maybe Score.BoxLocation)
clicks display =
    let
        boxes =
            List.concatMap .boxes display
    in
    List.map .onClick boxes


onClickSuite : Test
onClickSuite =
    describe "Location of (Just) onClicks" <|
        [ test "An inert Display has no onClicks" <|
            \_ ->
                List.all (\it -> it == Nothing) (clicks inertDisplay)
                    |> Expect.true "Found an onClick in inertDisplay"
        , test "No calculated boxes have onClicks" <|
            \_ ->
                List.all (\it -> it == Nothing) (clicks (rowsWith calculatedTags activeDisplay))
                    |> Expect.true "Found an onClick in a calculated box"
        , test "Initially, all the score boxes have onClicks" <|
            \_ ->
                List.all (\it -> it /= Nothing) (clicks (rowsWith activeTags activeDisplay))
                    |> Expect.true "Found a score box without an onClick"
        ]


rowsWith : Set String -> Score.Display -> Score.Display
rowsWith tagSet display =
    List.filter (\row -> Set.member row.tag tagSet) display


allTags : List String
allTags =
    [ ones, twos, threes, fours, fives, sixes, upperTotal, bonus, threeOfAKind, fourOfAKind, fullHouse, smallStraight, largeStraight, fiveOfAKind, chance, total, weighted, grandTotal ]


activeTags : Set String
activeTags =
    Set.fromList [ ones, twos, threes, fours, fives, sixes, threeOfAKind, fourOfAKind, fullHouse, smallStraight, largeStraight, fiveOfAKind, chance ]


calculatedTags : Set String
calculatedTags =
    Set.diff (Set.fromList allTags) (Set.fromList allTags)


tagsOf : Score.Display -> List String
tagsOf display =
    List.map .tag display


tagSuite : Test
tagSuite =
    describe "Displays have all the tags, in the right order" <|
        [ test "inert scoreDisplay has all the tags" <|
            \_ ->
                tagsOf inertDisplay
                    |> Expect.equal allTags
        , test "active scoreDisplay also has all the tags" <|
            \_ ->
                tagsOf activeDisplay
                    |> Expect.equal allTags
        ]
