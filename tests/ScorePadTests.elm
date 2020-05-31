module ScorePadTests exposing (fuzzTests, regularTests)

import Array exposing (Array)
import Dice exposing (Pip)
import Dict exposing (Dict)
import Expect
import Fuzz
import Random
import Random.Array
import Random.Extra
import Rank exposing (Rank, allRanks, numberOfRanks, upperRanks)
import ScorePad
    exposing
        ( Occupancy(..)
        , RowKind(..)
        , ScorePad
        , ScorePadBox
        , ScorePadRow
        , Scores
        , activeScorePad
        , emptyScores
        , grandTotal
        , staticScorePad
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
        boxParse ( occupancy, pointString ) =
            case occupancy of
                InUse ->
                    Maybe.withDefault 0 (String.toInt pointString)

                _ ->
                    0

        rowParse row =
            ( row.caption, List.map boxParse row.boxes )
    in
    Dict.fromList (List.map rowParse scorePad)


sectionSum : List Rank -> Scores -> List Int
sectionSum ranks scores =
    let
        info =
            parse (staticScorePad scores)

        missingRow =
            [ -100, -100, -100 ]

        getRow rank =
            Maybe.withDefault missingRow (Dict.get (Rank.caption rank) info)

        section =
            List.map getRow ranks

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
    Random.map ScorePad.makeScoresForTesting (Random.Array.array numberOfRanks genScoreRow)


scoreFuzz : Fuzz.Fuzzer Scores
scoreFuzz =
    Fuzz.custom genScores Shrink.noShrink


fuzzTests : Test
fuzzTests =
    fuzz scoreFuzz "All the fuzz tests at once, so we don't have to refuzz for every test" <|
        \someScores ->
            testAll someScores subtests
                |> Expect.equal ""


testAll : Scores -> List (Scores -> String) -> String
testAll scores tests =
    String.concat (List.map (\test -> test scores) tests)


subtest : String -> (Scores -> Bool) -> (Scores -> String)
subtest label fn =
    \scores ->
        if fn scores then
            ""

        else
            label


subtests : List (Scores -> String)
subtests =
    [ subtest "static and active ScorePads have the same totals" <|
        \scores ->
            let
                static =
                    staticScorePad scores

                active =
                    activeScorePad somePips scores
            in
            parse static == parse active
    , subtest "The upperTotal row is the sum of the upperRanks rows" <|
        \scores ->
            sectionSum upperRanks scores == getSumRow upperTotal scores
    , subtest "Each upperBonus box is 35 if the corresponding upperTotal box is >= 63, 0 otherwise" <|
        \scores ->
            let
                topTotals =
                    getSumRow upperTotal scores

                bonuses =
                    getSumRow upperBonus scores

                goodBonus total bonus =
                    (total >= 63 && bonus == 35) || (total < 63 && bonus == 0)
            in
            List.all (\bool -> bool) (List.map2 goodBonus topTotals bonuses)
    , subtest "The totalScore row is the sum of the Rolled rows, plus the bonus row" <|
        \scores ->
            let
                allBoxes =
                    sectionSum allRanks scores

                bonus =
                    getSumRow upperBonus scores

                expected =
                    List.map2 (+) allBoxes bonus
            in
            expected == getSumRow totalScore scores
    , subtest "The weightedScore row is the totalScore row, times [1,2,3]" <|
        \scores ->
            let
                totals =
                    getSumRow totalScore scores

                expected =
                    List.map2 (*) [ 1, 2, 3 ] totals
            in
            expected == getSumRow weightedScore scores
    , subtest "The grandTotal box is the sum of the totalScore row" <|
        \scores ->
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
            expected == grand
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


somePips : List Pip
somePips =
    Dice.pipListFromIntList [ 1, 3, 2, 5, 6 ]


rolledRows : ScorePad -> List ScorePadRow
rolledRows scores =
    List.filter (\r -> r.kind == Rolled) scores


rolledBoxes : ScorePad -> List ScorePadBox
rolledBoxes scores =
    List.concatMap .boxes (rolledRows scores)


emptyStatic : ScorePad
emptyStatic =
    staticScorePad emptyScores


emptyActive : ScorePad
emptyActive =
    activeScorePad somePips emptyScores


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
                List.all (\box -> box == ( InUse, "" )) (rolledBoxes emptyStatic)
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
                List.all (\box -> isAvailable (Tuple.first box)) (rolledBoxes emptyActive)
                    |> Expect.true "Every box should be Available"
        ]
