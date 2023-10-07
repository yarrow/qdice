module Xpect exposing (true)

import Expect exposing (Expectation)


true : String -> Bool -> Expectation
true errorMessage bool =
    bool |> Expect.equal True |> Expect.onFail errorMessage
