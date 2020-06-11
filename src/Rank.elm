module Rank exposing
    ( DiceToKeep(..)
    , PipsCounted
    , Rank
    , Rating(..)
    , arbitraryRank
    , columnPar
    , countPips
    , lower
    , maxPar
    , numberOfRanks
    , rankInfo
    , scoreAll
    , scoreAt
    , suggestKeeping
    , toInt
    , upper
    )

import Array exposing (Array)
import Pip exposing (Pip)


type alias PipsCounted =
    Array Int


type DiceToKeep
    = Straight (List Int)
    | OfAKind Int


suggestKeeping : Maybe (List Pip) -> List DiceToKeep
suggestKeeping pips =
    case pips of
        Nothing ->
            []

        Just pipList ->
            let
                counted =
                    countPips pipList

                pairOrBetter =
                    counted
                        |> Array.toList
                        |> List.indexedMap
                            (\pipz count ->
                                if count >= 2 && count < 5 then
                                    pipz

                                else
                                    0
                            )
                        |> List.filter (\n -> n > 0)
                        |> List.map OfAKind

                diceToKeep =
                    case straightSuggestion counted of
                        Just straight ->
                            straight :: pairOrBetter

                        Nothing ->
                            pairOrBetter
            in
            List.reverse diceToKeep


straightSuggestion : PipsCounted -> Maybe DiceToKeep
straightSuggestion counted =
    let
        ( start, length ) =
            findRun counted
    in
    if length == 3 || length == 4 then
        Just (Straight (List.range start (start + length - 1)))

    else
        Nothing


countPips : List Pip -> PipsCounted
countPips pipList =
    -- FIXME — We need to import maxPips instead of using the magic number 7
    let
        faces =
            List.map Pip.toInt pipList

        increment jth counter =
            case Array.get jth counter of
                Just old ->
                    Array.set jth (old + 1) counter

                Nothing ->
                    counter
    in
    List.foldr increment (Array.repeat 7 0) faces


valueTimesCount : Int -> PipsCounted -> Int
valueTimesCount value counted =
    value * Maybe.withDefault 0 (Array.get value counted)


sumDice : PipsCounted -> Int
sumDice count =
    Array.foldl (+) 0 (Array.indexedMap (*) count)


ofAKind : PipsCounted -> Int
ofAKind counted =
    Maybe.withDefault 0 (List.maximum (Array.toList counted))


sumDiceIfAtLeast : Int -> PipsCounted -> Int
sumDiceIfAtLeast min counted =
    if min <= ofAKind counted then
        sumDice counted

    else
        0


findRun : PipsCounted -> ( Int, Int )
findRun counted =
    let
        find ( j, start, length ) list =
            case list of
                [] ->
                    ( j, start, length )

                present :: tail ->
                    if present > 0 then
                        find ( j + 1, start, length + 1 ) tail

                    else if length >= 3 then
                        -- There are only five dice, so there can be at most
                        -- one run of three or longer
                        ( j, start, length )

                    else
                        find ( j + 1, j + 1, 0 ) tail

        ( _, theStart, theLength ) =
            find ( 0, 0, 0 ) (Array.toList counted)
    in
    ( theStart, theLength )


longestStraight : PipsCounted -> Int
longestStraight counted =
    findRun counted |> Tuple.second


scoreAt : Rank -> List Pip -> Int
scoreAt (Rank rank) pipList =
    let
        fn : PipsCounted -> Int
        fn =
            Maybe.withDefault (\_ -> 0) (Array.get rank fnArray)
    in
    fn (countPips pipList)


nWhen : Int -> Bool -> Int
nWhen n condition =
    if condition then
        n

    else
        0


fullHouse : PipsCounted -> Int
fullHouse counted =
    let
        max =
            ofAKind counted

        hasPair kounted =
            List.any (\count -> count == 2) (Array.toList kounted)
    in
    nWhen 25 (max == 5 || (max == 3 && hasPair counted))


type Rank
    = Rank Int


type Rating
    = Meager -- under par for upper section, 0 for lower
    | Sufficient -- at par for upper section, non-zero for lower
    | Ample -- over par for upper section


type alias RankInfo =
    { rank : Rank, caption : String, fn : PipsCounted -> Int, rating : Int -> Rating }


maxPar : Int
maxPar =
    63


columnPar : List (Maybe Int) -> Int
columnPar col =
    let
        thisPar par n =
            case n of
                Nothing ->
                    0

                Just _ ->
                    par
    in
    List.sum (List.map2 thisPar [ 3, 6, 9, 12, 15, 18 ] col)


rankInfo : List RankInfo
rankInfo =
    let
        compareToPar : Int -> Int -> Rating
        compareToPar par n =
            case compare n par of
                LT ->
                    Meager

                EQ ->
                    Sufficient

                GT ->
                    Ample

        compareToZero : Int -> Rating
        compareToZero n =
            if n == 0 then
                Meager

            else
                Sufficient

        base =
            [ { caption = "Ones"
              , fn = valueTimesCount 1
              , rating = compareToPar 3
              }
            , { caption = "Twos"
              , fn = valueTimesCount 2
              , rating = compareToPar 6
              }
            , { caption = "Threes"
              , fn = valueTimesCount 3
              , rating = compareToPar 9
              }
            , { caption = "Fours"
              , fn = valueTimesCount 4
              , rating = compareToPar 12
              }
            , { caption = "Fives"
              , fn = valueTimesCount 5
              , rating = compareToPar 15
              }
            , { caption = "Sixes"
              , fn = valueTimesCount 6
              , rating = compareToPar 18
              }
            , { caption = "3 of a kind"
              , fn = sumDiceIfAtLeast 3
              , rating = compareToZero
              }
            , { caption = "4 of a kind"
              , fn = sumDiceIfAtLeast 4
              , rating = compareToZero
              }
            , { caption = "Full House"
              , fn = fullHouse
              , rating = compareToZero
              }
            , { caption = "Sm Strght"
              , fn = \counted -> nWhen 30 (longestStraight counted >= 4)
              , rating = compareToZero
              }
            , { caption = "Lg Strght"
              , fn = \counted -> nWhen 40 (longestStraight counted >= 5)
              , rating = compareToZero
              }
            , { caption = "5 of a kind"
              , fn = \counted -> nWhen 50 (List.any (\n -> n == 5) (Array.toList counted))
              , rating = compareToZero
              }
            , { caption = "Chance"
              , fn = sumDice
              , rating = compareToZero
              }
            ]

        addRank j { caption, fn, rating } =
            { rank = Rank j, caption = caption, fn = fn, rating = rating }
    in
    List.indexedMap addRank base


captions : List String
captions =
    List.map .caption rankInfo


fns : List (PipsCounted -> Int)
fns =
    List.map .fn rankInfo


ratings : List (Int -> Rating)
ratings =
    List.map .rating rankInfo


ranks : List Rank
ranks =
    List.map .rank rankInfo


fnArray : Array (PipsCounted -> Int)
fnArray =
    Array.fromList (List.map .fn rankInfo)


scoreAll : PipsCounted -> List Int
scoreAll counted =
    List.map (\f -> f counted) fns


numberOfUppers : Int
numberOfUppers =
    6


upper : List a -> List a
upper list =
    List.take numberOfUppers list


lower : List a -> List a
lower list =
    List.drop numberOfUppers list


numberOfRanks : Int
numberOfRanks =
    List.length rankInfo


toInt : Rank -> Int
toInt (Rank rank) =
    rank



--- for testing


arbitraryRank : Rank
arbitraryRank =
    Rank (numberOfRanks - 1)
