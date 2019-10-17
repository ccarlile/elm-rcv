module Ballots exposing (Ballot, restBallots, firstBallot)

import Candidates exposing (CandidateName)

type alias Ballot =
    { choice1: CandidateName
    , choice2: CandidateName
    , choice3: CandidateName
    , pos : Maybe (Float, Float)
    }


ballots : List Ballot
ballots =
    [
     { choice1 = Candidates.pepperoni
     , choice2 = Candidates.supreme
     , choice3 = Candidates.hawaiian
     , pos = Nothing
     }
    ,

     { choice1 = Candidates.hawaiian
     , choice2 = Candidates.pepperoni
     , choice3 = Candidates.supreme
     , pos = Nothing
     }
        
    , { choice1 = Candidates.hawaiian
      , choice2 = Candidates.pepperoni
      , choice3 = Candidates.supreme
      , pos = Nothing
      }
    ]
    
