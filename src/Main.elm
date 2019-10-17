module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (style, attribute)
import Html.Events exposing (onClick)
import Set
import Browser exposing (element)
import Ballots exposing (Ballot)
import Candidates exposing (..)

type Model
    = Intro
    | Game GameState
    | Elimination Round
    | Winner Candidate

type alias Round = List Candidate

type LastGuess
    = Correct
    | Incorrect
    

type alias GameState =
    -- Ballots reset every round
    { unsortedBallots: List Ballot 
    -- After each vote: (current ballot, unsorted) = pop unsorted
    , currentBallot: Ballot
    , currentRound: Round
    , lastGuess: Maybe LastGuess
    , dragDrop : DragDrop.Model Ballot CandidateName
    }

type alias Count = Maybe Int

ballotTarget: Round -> Ballot -> CandidateName
ballotTarget candidates ballot =
    -- top choice that doesnt appear in round
    if (List.member ballot.choice1 candidates) then
        ballot.choice1
    else if (List.member ballot.choice2 candidates) then
        ballot.choice2
    else ballot.choice3

-- Message types
type Msg
    = NoOp
    | PlayGame
    | DragDropMsg (DragDrop.Msg BallotState CandidateState)

init : () -> (Model, Cmd Msg)
init () =
    (Intro, Cmd.none)

update : Msg -> Model -> Model
update msg model =
    case model of
        Game game ->
            updateGame msg game
        _ ->
            case msg of
                PlayGame ->
                    initializeGame Ballots.ballots
                _ -> model

initializeGame : List Ballot -> Model
initializeGame ballots =
    case ballots of
        first :: rest -> Game ( GameState first rest defaultCandidates AtRest Nothing DragDrop.init )
        only :: Nil -> Winner { name = only.choice1, votes = 1 }
        Nil -> Winner { name = "Vladmir Putin", votes = "one million"}

updateGame : Msg -> GameState -> Model
updateGame msg game =
   case msg of
       DragDropMsg msg_ ->
           let
               ( model_, result ) =
                   DragDrop.update msg_ model.dragDrop

               ( nextBallot, nextUnsorted ) =
                   case model.unsortedBallots of
                       x :: xs -> ( Just x , xs )
                       x :: Nil -> ( Just x , Nil )
                       Nil -> ( Nothing , Nil )

               correctDrag =
                   case result of
                       Just ( ballot, candidateName, _) ->
                           choiceForRound ballot == candidateName
                       Nothing -> False
                           
               newModel =
                   -- on drag success, three outcomes
                   -- ballot dragged on incorrect place. Update lastgess and don't change anything else
                   -- ballot dragged on correct place, and further ballots remain. Update votes and iterate
                   -- ballot dragged on correct candidate, and no further ballots remain. Move to elimination
                   case (correctDrag, model.unsortedBallots) of
                       ( False, _ ) => { model | lastGuess = Just Incorrect, dragDrop = model_ }

                       ( True, next :: nextUnsorted ) ->
                           { model
                               | unsortedBallots = nextUnsorted
                               , currentBallot = next
                               , currentRound = castBallot game.ballot game.round
                               , lastGuess = Just Correct
                               , dragDrop = model_
                           }

                       ( True, Nil ) -> Elimination round

           in
               newModel

castBallot : Ballot -> Round -> Round
castBallot ballot round = 
    let
        target = ballotTarget round ballot
    in
      List.map \c -> if target == c.name then  { c | votes = votes +1 } else c

view : Model -> Html Msg
view model =
    case model of
        Intro ->
            div [ attribute "draggable" "true"
                , style "background-color" "red"] [
             button [onClick PlayGame] [text "Hello, world"]
            ]
        Game game -> 
            renderGame game
        Elimination round ->
            text [ "Fill me in later" ]
        Complete ->
            div [] [ text "Yasss kweeen"]


renderGame : GameState -> Html Msg
renderGame game =
    let
        dropId
            = DragDrop.getDropId game.dragDrop
    in
        div []
            [ div [ style "display" "flex"
                  , style "flex-wrap" "wrap"] (List.map (renderCandidate dropId) game.allCandidates)
            , div [ style "display" "flex"] [ renderBallot ballot ]
        ]
        
renderCandidate : CandidateName -> Candidate -> Html Msg
renderCandidate target candidate =
    let
        outline =
            if
                target == candidate.name
            then
                [ style "outline-color" "green" ]
            else
                [ style "outline-color" "red" ]

        droppable =
            DragDrop.droppable DragDropMsg candidate.name
    in
        div outline
            ++ droppable
            ++ [ style "width" "250px"
              , style "height" "250px"
              , style "margin" "40px"
              , style "background-color" "lightgray"
              ] [ text candidate.name ]

-- Todo: if pos is set, render it absolutely
renderBallot : Ballot -> Html Msg
renderBallot ballot =
    div [ DragDrop.draggable DragDropMsg ballot
        , style "width" "250px"
        , style "margin" "auto"
        ] [ p [] [ text ballot.choice1 ]
          , p [] [ text ballot.choice2 ]
          , p [] [ text ballot.choice3 ]  ]

subscriptions model =
    Sub.none

main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
