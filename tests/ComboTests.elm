module ComboTests exposing (..)

import Combo exposing (..)
import Expect exposing (Expectation)
import Test exposing (..)


comboSuite : Test
comboSuite =
    only <|
        describe "Test for Combo types" <|
            [ test "The combos have the right tags" <|
                \_ ->
                    let
                        combos =
                            List.map Tuple.first matches

                        tags =
                            List.map Tuple.second matches

                        matches =
                            [ ( Ones, "Ones" )
                            , ( Twos, "Twos" )
                            , ( Threes, "Threes" )
                            , ( Fours, "Fours" )
                            , ( Fives, "Fives" )
                            , ( Sixes, "Sixes" )
                            , ( ThreeOfAKind, "3 of a kind" )
                            , ( FourOfAKind, "4 of a kind" )
                            , ( FullHouse, "Full House" )
                            , ( SmallStraight, "Sm Straight" )
                            , ( LargeStraight, "Lg Straight" )
                            , ( FiveOfAKind, "5 of a kind" )
                            , ( Chance, "Chance" )
                            ]
                    in
                    List.map Combo.toString combos
                        |> Expect.equalLists tags
            ]



{-
   getUppers rowArray == List.map getRow combo [Ones, Twos, Threes, Fours, Fives, Sixes]

   getLowers rowArray == List.map getRow combo [ThreeOfAKind, fourOfAKind, FullHouse, SmallStraight, LargeStraight, FiveOfAKind, Chance]

   needed? toList rowArray == (getUppers rowArray) ++ (getLowers rowArray)
-}
