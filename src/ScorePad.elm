module ScorePad exposing
    ( ScorePadRow
    , Scores
    , blank
    , display
    )


display : Scores -> List ScorePadRow
display _ =
    List.repeat 13 0


type alias ScorePadRow =
    Int


type alias Scores =
    Int


blank : Scores
blank =
    0
