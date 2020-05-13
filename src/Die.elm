module Die exposing (NextRoll(..), OneDie, flipNextRoll, makeDie, maxDie, minDie, nextRoll, oneDie, pips, url)


type NextRoll
    = Keep
    | Reroll


type alias Fields =
    { pips : Int
    , nextRoll : NextRoll
    }


type OneDie
    = OneDie Fields


oneDie : Int -> OneDie
oneDie n =
    OneDie
        { pips = clamp minDie maxDie n
        , nextRoll = Keep
        }


pips : OneDie -> Int
pips (OneDie die) =
    die.pips


nextRoll : OneDie -> NextRoll
nextRoll (OneDie die) =
    die.nextRoll


makeDie : ( Int, NextRoll ) -> OneDie
makeDie ( n, nextStatus ) =
    OneDie { pips = n, nextRoll = nextStatus }


flipNextRoll : OneDie -> OneDie
flipNextRoll (OneDie d) =
    OneDie
        { pips = d.pips
        , nextRoll =
            case d.nextRoll of
                Keep ->
                    Reroll

                Reroll ->
                    Keep
        }


minDie : Int
minDie =
    1


maxDie : Int
maxDie =
    6


url : OneDie -> String
url d =
    "assets/die-" ++ String.fromInt (pips d) ++ ".png"
