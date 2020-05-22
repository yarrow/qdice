module ScorePad exposing
    ( ScorePadRow
    , Scores
    , initialScores
    , staticScorePad
    )


staticScorePad : Scores -> List ScorePadRow
staticScorePad _ =
    List.repeat 13 { caption = "Ones", boxes = List.repeat 3 "" }


type alias ScorePadRow =
    { caption : String
    , boxes : List ScorePadBox
    }


type alias ScorePadBox =
    String


type alias Scores =
    Int


initialScores : Scores
initialScores =
    0
