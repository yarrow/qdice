module ScorePadTests exposing (fuzzTests, regularTests)

import Dict exposing (Dict)
import Expect exposing (Expectation)
import Fuzz
import Pip
import Random
import Random.Extra
import Rank exposing (Rating(..), rankInfo)
import Score exposing (Scores, emptyScores)
import ScorePad
    exposing
        ( Occupancy(..)
        , RowKind(..)
        , ScorePad
        , grandTotal
        , makeScorePad
        , totalScore
        , upperBonus
        , upperTotal
        , weightedScore
        )
import Shrink
import Test exposing (Test, describe, fuzz, test)


type alias Info =
    Dict String (List Int)


parse : ScorePad -> Info
parse scorePad =
    -- We're using this to check the sum rows, so available boxes, not yet set, count 0
    let
        boxParse { occupancy, score } =
            case occupancy of
                InUse ->
                    Maybe.withDefault 0 (String.toInt score)

                _ ->
                    0

        rowParse row =
            ( row.caption, List.map boxParse row.boxes )
    in
    Dict.fromList (List.map rowParse scorePad)


allScoreCaptions : List String
allScoreCaptions =
    List.map .caption rankInfo


upperCaptions : List String
upperCaptions =
    Rank.upper allScoreCaptions


activeScorePad : Scores -> ScorePad
activeScorePad scores =
    let
        somePips =
            Pip.mapFromInt [ 1, 3, 2, 5, 6 ]
    in
    makeScorePad (Just somePips) scores


staticScorePad : Scores -> ScorePad
staticScorePad scores =
    makeScorePad Nothing scores


sectionSum : List String -> Scores -> List Int
sectionSum captions scores =
    let
        info =
            parse (staticScorePad scores)

        missingRow =
            [ -100, -100, -100 ]

        getRow caption =
            Maybe.withDefault missingRow (Dict.get caption info)

        section =
            List.map getRow captions

        add a b =
            List.map2 (+) a b

        zero =
            [ 0, 0, 0 ]
    in
    List.foldr add zero section


getSumRow : String -> Scores -> List Int
getSumRow caption scores =
    let
        info =
            parse (staticScorePad scores)

        missingRow =
            [ 4000, 4000, 4000 ]
    in
    Maybe.withDefault missingRow (Dict.get caption info)


genScores : Random.Generator Scores
genScores =
    let
        genScoreBox =
            Random.Extra.maybe Random.Extra.bool (Random.int 0 50)

        genScoreRow =
            Random.list 3 genScoreBox
    in
    Random.map Score.makeScoresForTesting (Random.list Rank.numberOfRanks genScoreRow)


scoreFuzz : Fuzz.Fuzzer Scores
scoreFuzz =
    Fuzz.custom genScores Shrink.noShrink


fuzzTests : Test
fuzzTests =
    fuzz scoreFuzz "All the fuzz tests at once, so we don't have to refuzz for every test" <|
        \scores ->
            testAll scores subtests


testAll : Scores -> List (Scores -> Expectation) -> Expectation
testAll scores tests =
    scores |> Expect.all tests


subtest : String -> (String -> Scores -> Expectation) -> (Scores -> Expectation)
subtest label fn =
    fn label


subtests : List (Scores -> Expectation)
subtests =
    [ subtest "static and active ScorePads have the same totals" <|
        \label scores ->
            let
                static =
                    staticScorePad scores

                active =
                    activeScorePad scores
            in
            parse static
                == parse active
                |> Expect.true label
    , subtest "The upperTotal row is the sum of the upperRanks rows" <|
        \label scores ->
            sectionSum upperCaptions scores
                == getSumRow upperTotal scores
                |> Expect.true label
    , subtest "An active ScorePad has blank where the new score would be zero" <|
        \label scores ->
            let
                available : List String
                available =
                    scores
                        |> activeScorePad
                        |> List.filter (\r -> r.kind == Rolled)
                        |> List.map .boxes
                        |> List.concat
                        |> List.filterMap
                            (\{ occupancy, score } ->
                                case occupancy of
                                    Available _ ->
                                        Just score

                                    InUse ->
                                        Nothing
                            )

                zeros =
                    List.filter (\b -> b == "0") available
            in
            List.length zeros
                == 0
                |> Expect.true label
    , subtest "An active ScorePad has a Meager Rating for potential zero scores" <|
        \label scores ->
            let
                scoreOf box =
                    Maybe.withDefault -1 (String.toInt box.score)

                nonmeager =
                    scores
                        |> activeScorePad
                        |> List.filter (\r -> r.kind == Rolled)
                        |> List.map .boxes
                        |> List.concat
                        |> List.filter (\box -> isAvailable box.occupancy)
                        |> List.filter
                            (\box -> scoreOf box == 0 && box.rating /= Meager)
            in
            List.length nonmeager
                == 0
                |> Expect.true label
    , subtest "An active ScorePad has a Sufficient Rating for InUse boxes" <|
        \label scores ->
            let
                notSufficient =
                    scores
                        |> activeScorePad
                        |> List.filter (\r -> r.kind == Rolled)
                        |> List.map .boxes
                        |> List.concat
                        |> List.filter (\box -> box.occupancy == InUse)
                        |> List.filter (\box -> box.rating /= Sufficient)
            in
            List.length notSufficient
                == 0
                |> Expect.true label
    , subtest "A static ScorePad has a Sufficient rating for all scores" <|
        \label scores ->
            let
                notSufficient =
                    scores
                        |> staticScorePad
                        |> List.filter (\r -> r.kind == Rolled)
                        |> List.map .boxes
                        |> List.concat
                        |> List.filter (\box -> box.rating /= Sufficient)
            in
            List.length notSufficient
                == 0
                |> Expect.true label
    , subtest "Each upperBonus box is 35 if the corresponding upperTotal box is >= 63, 0 otherwise" <|
        \label scores ->
            let
                topTotals =
                    getSumRow upperTotal scores

                bonuses =
                    getSumRow upperBonus scores

                goodBonus total bonus =
                    (total >= 63 && bonus == 35) || (total < 63 && bonus == 0)
            in
            List.all (\bool -> bool) (List.map2 goodBonus topTotals bonuses)
                |> Expect.true label
    , subtest "The totalScore row is the sum of the Rolled rows, plus the bonus row" <|
        \label scores ->
            let
                allBoxes =
                    sectionSum allScoreCaptions scores

                bonus =
                    getSumRow upperBonus scores

                expected =
                    List.map2 (+) allBoxes bonus
            in
            expected
                == getSumRow totalScore scores
                |> Expect.true label
    , subtest "The weightedScore row is the totalScore row, times [1,2,3]" <|
        \label scores ->
            let
                totals =
                    getSumRow totalScore scores

                expected =
                    List.map2 (*) [ 1, 2, 3 ] totals
            in
            expected
                == getSumRow weightedScore scores
                |> Expect.true label
    , subtest "The grandTotal box is the sum of the totalScore row" <|
        \label scores ->
            let
                weighted =
                    getSumRow weightedScore scores

                expected =
                    List.foldr (+) 0 weighted

                grandRow =
                    getSumRow grandTotal scores

                grand =
                    Maybe.withDefault -99 (List.head grandRow)
            in
            expected
                == grand
                |> Expect.true label
    ]


theCaptions : List String
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


rolledRows : ScorePad -> List ScorePad.Row
rolledRows scores =
    List.filter (\r -> r.kind == Rolled) scores


rolledBoxes : ScorePad -> List ScorePad.Box
rolledBoxes scores =
    List.concatMap .boxes (rolledRows scores)


emptyStatic : ScorePad
emptyStatic =
    staticScorePad emptyScores


emptyActive : ScorePad
emptyActive =
    activeScorePad emptyScores


isAvailable : Occupancy -> Bool
isAvailable occupancy =
    case occupancy of
        InUse ->
            False

        Available _ ->
            True


regularTests : Test
regularTests =
    describe "ScorePad tests" <|
        [ test "Every Rolled box in staticScorePad emptyScores is (InUse, '')" <|
            \_ ->
                List.all (\box -> box == ScorePad.Box InUse Sufficient "") (rolledBoxes emptyStatic)
                    |> Expect.true "Every box should be (InUse, '')"
        , test "Each Rolled staticScorePad row has the correct caption" <|
            \_ ->
                List.map .caption (rolledRows emptyStatic)
                    |> Expect.equalLists theCaptions
        , test "Each activeScorePad box has the correct caption" <|
            \_ ->
                List.map .caption (rolledRows emptyActive)
                    |> Expect.equalLists theCaptions
        , test "Every Rolled box in ScorePad emptyScores is Available" <|
            \_ ->
                List.all (\box -> isAvailable box.occupancy) (rolledBoxes emptyActive)
                    |> Expect.true "Every box should be Available"
        ]
