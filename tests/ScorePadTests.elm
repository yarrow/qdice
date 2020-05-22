module ScorePadTests exposing (scorePadTests)

import Expect
import ScorePad exposing (Score(..), toText)
import Test exposing (..)


scorePadTests : Test
scorePadTests =
    describe "Tests for ScorePad.elm" <|
        [ test "toText Available is the empty string" <|
            toText Available
                |> Expect.equal ""
        ]
