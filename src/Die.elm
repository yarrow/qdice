module Die exposing (Die, NextRoll(..), flipNextRoll, makeDie, maxDie, minDie, nextRoll, oneDie, pips, url)


type NextRoll
    = Keep
    | Reroll


type alias Fields =
    { pips : Int
    , nextRoll : NextRoll
    }


type Die
    = Die Fields


makeDie : ( Int, NextRoll ) -> Die
makeDie ( n, nextStatus ) =
    Die
        { pips = clamp minDie maxDie n
        , nextRoll = nextStatus
        }


oneDie : Int -> Die
oneDie n =
    makeDie ( n, Keep )


pips : Die -> Int
pips (Die die) =
    die.pips


nextRoll : Die -> NextRoll
nextRoll (Die die) =
    die.nextRoll


flipNextRoll : Die -> Die
flipNextRoll (Die d) =
    Die
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


url : Die -> String
url d =
    "assets/die-" ++ String.fromInt (pips d) ++ ".png"