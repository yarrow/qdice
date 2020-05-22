module ScorePad exposing
    ( ScorePadRow
    , Scores
    , blank
    , display
    )


display : Scores -> List ScorePadRow
display _ =
    List.repeat 13 { caption = "Ones", boxes = List.repeat 3 { score = 0 } }


type alias ScorePadRow =
    { caption : String
    , boxes : List ScorePadBox
    }


type alias ScorePadBox =
    { score : Int }


type alias Scores =
    Int


blank : Scores
blank =
    0
