module Rank exposing
    ( DiceToKeep(..)
    , PipsCounted
    , Rank
    , arbitraryRank
    , captions
    , countPips
    , fns
    , lower
    , numberOfRanks
    , ranks
    , scoreAll
    , suggestKeeping
    , tallyPips
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


tallyPips : Rank -> List Pip -> Int
tallyPips (Rank rank) pipList =
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


table : List ( String, PipsCounted -> Int )
table =
    [ ( "Ones", valueTimesCount 1 )
    , ( "Twos", valueTimesCount 2 )
    , ( "Threes", valueTimesCount 3 )
    , ( "Fours", valueTimesCount 4 )
    , ( "Fives", valueTimesCount 5 )
    , ( "Sixes", valueTimesCount 6 )
    , ( "3 of a kind", sumDiceIfAtLeast 3 )
    , ( "4 of a kind", sumDiceIfAtLeast 4 )
    , ( "Full House", fullHouse )
    , ( "Sm Strght", \counted -> nWhen 30 (longestStraight counted >= 4) )
    , ( "Lg Strght", \counted -> nWhen 40 (longestStraight counted >= 5) )
    , ( "5 of a kind", \counted -> nWhen 50 (List.any (\n -> n == 5) (Array.toList counted)) )
    , ( "Chance", sumDice )
    ]


numberOfUppers : Int
numberOfUppers =
    6


captions : List String
captions =
    List.map Tuple.first table


fns : List (PipsCounted -> Int)
fns =
    List.map Tuple.second table


fnArray : Array (PipsCounted -> Int)
fnArray =
    Array.fromList fns


scoreAll : PipsCounted -> List Int
scoreAll counted =
    List.map (\f -> f counted) fns


upper : List a -> List a
upper list =
    List.take numberOfUppers list


lower : List a -> List a
lower list =
    List.drop numberOfUppers list


type Rank
    = Rank Int


ranks : List Rank
ranks =
    List.indexedMap (\j _ -> Rank j) table


numberOfRanks : Int
numberOfRanks =
    List.length table



{-
   goodness : Rank -> Int -> Order
   goodness =
       ...
-}


toInt : Rank -> Int
toInt (Rank rank) =
    rank



--- for testing


arbitraryRank : Rank
arbitraryRank =
    Rank (numberOfRanks - 1)
