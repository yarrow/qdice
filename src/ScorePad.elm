module ScorePad exposing
    ( Score(..)
    , ScorePadRow
    , Scores
    , display
    , initialScores
    , toText
    )


type Score
    = Available
    | Is Int


toText : Score -> String
toText score =
    ""


display : Scores -> List ScorePadRow
display _ =
    let
        available =
            { text = toText Available }
    in
    List.repeat 13 { caption = "Ones", boxes = List.repeat 3 available }


type alias ScorePadRow =
    { caption : String
    , boxes : List ScorePadBox
    }


type alias ScorePadBox =
    { text : String
    }


type alias Scores =
    Int


initialScores : Scores
initialScores =
    0
