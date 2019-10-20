module Candidates exposing (..)

type alias CandidateName = String

type alias Candidate = { name: CandidateName, votes: Int }

defaultCandidates : List CandidateName
defaultCandidates = [ pepperoni, mushroom, hawaiian, supreme ]

pepperoni = "Pepperoni"
mushroom = "Mushroom"
hawaiian = "Hawaiian"
supreme = "Supreme"
