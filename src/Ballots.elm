module Ballots exposing (Ballot, ballots)

import Candidates exposing (CandidateName)

type alias Ballot =
    { choice1: CandidateName
    , choice2: CandidateName
    , choice3: CandidateName
    , pos : Maybe (Float, Float)
    }

ballots : Int -> List Ballot
ballots candidates =
    case candidates of
        4 ->
            [ { choice1 = Candidates.pepperoni, choice2 = Candidates.supreme, choice3 = Candidates.mushroom, pos = Nothing}
            , { choice1 = Candidates.mushroom, choice2 = Candidates.hawaiian, choice3 = Candidates.supreme, pos = Nothing}
            , { choice1 = Candidates.pepperoni, choice2 = Candidates.supreme, choice3 = Candidates.hawaiian, pos = Nothing}
            , { choice1 = Candidates.hawaiian, choice2 = Candidates.pepperoni, choice3 = Candidates.supreme, pos = Nothing}
            , { choice1 = Candidates.mushroom, choice2 = Candidates.pepperoni, choice3 = Candidates.supreme, pos = Nothing}
            , { choice1 = Candidates.mushroom, choice2 = Candidates.supreme, choice3 = Candidates.hawaiian, pos = Nothing}
            , { choice1 = Candidates.pepperoni, choice2 = Candidates.supreme, choice3 = Candidates.hawaiian, pos = Nothing}
            , { choice1 = Candidates.supreme, choice2 = Candidates.pepperoni, choice3 = Candidates.hawaiian, pos = Nothing}
            , { choice1 = Candidates.pepperoni, choice2 = Candidates.mushroom, choice3 = Candidates.supreme, pos = Nothing}
            , { choice1 = Candidates.hawaiian, choice2 = Candidates.pepperoni, choice3 = Candidates.mushroom, pos = Nothing}
            , { choice1 = Candidates.mushroom, choice2 = Candidates.supreme, choice3 = Candidates.hawaiian, pos = Nothing}
            , { choice1 = Candidates.supreme, choice2 = Candidates.hawaiian, choice3 = Candidates.pepperoni, pos = Nothing}
            , { choice1 = Candidates.supreme, choice2 = Candidates.mushroom, choice3 = Candidates.hawaiian, pos = Nothing}
            , { choice1 = Candidates.mushroom, choice2 = Candidates.pepperoni, choice3 = Candidates.supreme, pos = Nothing}
            , { choice1 = Candidates.mushroom, choice2 = Candidates.pepperoni, choice3 = Candidates.hawaiian, pos = Nothing}
            , { choice1 = Candidates.pepperoni, choice2 = Candidates.mushroom, choice3 = Candidates.supreme, pos = Nothing}

            ]
        3 ->
             [ { choice1 = Candidates.hawaiian, choice2 = Candidates.pepperoni, choice3 = Candidates.supreme, pos = Nothing}
             , { choice1 = Candidates.hawaiian, choice2 = Candidates.pepperoni, choice3 = Candidates.mushroom, pos = Nothing}
             ]

        2 ->
             [ { choice1 = Candidates.supreme, choice2 = Candidates.pepperoni, choice3 = Candidates.hawaiian, pos = Nothing}
             , { choice1 = Candidates.supreme, choice2 = Candidates.pepperoni, choice3 = Candidates.hawaiian, pos = Nothing}
            , { choice1 = Candidates.supreme, choice2 = Candidates.mushroom, choice3 = Candidates.hawaiian, pos = Nothing}
             ]
        _ -> []
        
