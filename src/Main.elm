port module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (style, attribute)
import Html.Events exposing (onClick)
import Extra exposing (maybeFlatten)
import Set
import Browser exposing (element)
import Ballots exposing (Ballot)
import Candidates exposing (..)
import Html5.DragDrop as DragDrop
import Json.Decode exposing (Value)

-- development

port dragstart : Value -> Cmd msg

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
    if (List.map .name candidates |> List.member ballot.choice1) then
        ballot.choice1
    else if (List.map .name candidates |> List.member ballot.choice2) then
        ballot.choice2
    else ballot.choice3

-- Message types
type Msg
    = NoOp
    | PlayGame
    | DragDropMsg (DragDrop.Msg Ballot CandidateName)

init : () -> (Model, Cmd Msg)
init () =
    (Intro, Cmd.none)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        Game game ->
            updateGame msg game
        _ ->
            case msg of
                PlayGame ->
                    ( initializeGame Ballots.ballots, Cmd.none )
                _ -> ( model, Cmd.none )

initializeGame : List Ballot -> Model
initializeGame ballots =
    case ballots of
        first :: rest :: tail ->
            let
                candidates = List.map (\c -> { name = c, votes = 0 }) defaultCandidates
            in
                Game ( GameState (rest :: tail) first candidates Nothing DragDrop.init )
        only :: [] -> Winner { name = only.choice1, votes = 1 }
        [] -> Winner { name = "Vladmir Putin", votes = 1000000 }

updateGame : Msg -> GameState -> ( Model, Cmd Msg )
updateGame msg game =
   case msg of
       DragDropMsg msg_ ->
           let
               ( model_, result ) =
                   DragDrop.update msg_ game.dragDrop

               dragPort =
                   DragDrop.getDragstartEvent msg_
                       |> Maybe.map (.event >> dragstart)
                       |> Maybe.withDefault Cmd.none

               correctDrag =
                   case result of
                       Just ( ballot, candidateName, _) ->
                           ballotTarget game.currentRound ballot == candidateName
                       Nothing -> False
                           
               newModel : ( Model, Cmd Msg )
               newModel =
                   -- on drag success, three outcomes
                   -- ballot dragged on incorrect place. Update lastgess and don't change anything else
                   -- ballot dragged on correct place, and further ballots remain. Update votes and iterate
                   -- ballot dragged on correct candidate, and no further ballots remain. Move to elimination
                   case (correctDrag, game.unsortedBallots) of
                       ( False, _ ) -> ( Game { game | lastGuess = Just Incorrect, dragDrop = model_ }
                                      , Cmd.none )

                       ( True, next :: nextUnsorted ) ->
                           ( Game { game | unsortedBallots = nextUnsorted
                               , currentBallot = next
                               , currentRound = castBallot game.currentBallot game.currentRound
                               , lastGuess = Just Correct
                               , dragDrop = model_
                           }, Cmd.none )

                       ( True, [] ) -> ( Elimination game.currentRound, Cmd.none )

           in
               newModel
       _ -> ( Game game, Cmd.none )

castBallot : Ballot -> Round -> Round
castBallot ballot round = 
    let
        target = ballotTarget round ballot
        cast = \c -> if target == c.name then  { c | votes = c.votes +1 } else c
    in
      List.map cast round

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
            text "Fill me in later"
        Winner candidate ->
            div [] [ text "Yasss kweeen"]


renderGame : GameState -> Html Msg
renderGame game =
    let
        dropId
            = DragDrop.getDropId game.dragDrop

        lastGuess =
            case game.lastGuess of
                Just Correct -> div [] [ text "Correct!" ]
                Just Incorrect -> div [] [ text "Incorrect :|" ]
                Nothing -> div [] []
    in
        div []
            [ lastGuess
            , div [ style "display" "flex"
                  , style "flex-wrap" "wrap"] (List.map (renderCandidate dropId) game.currentRound)
            , div [ style "display" "flex"] [ renderBallot game.currentBallot ]
        ]
        
renderCandidate : Maybe CandidateName -> Candidate -> Html Msg
renderCandidate target candidate =
    let
        outline =
            case target of
                Just candidateName ->
                    if candidateName == candidate.name
                        then [ style "outline-color" "green" ]
                    else [style "outline-color" "red"]
                Nothing -> []

        droppable =
            DragDrop.droppable DragDropMsg candidate.name
    in
        div (outline
            ++ droppable
            ++ outline
            ++ [ style "width" "250px"
              , style "height" "250px"
              , style "margin" "40px"
              , style "background-color" "lightgray"
              ]) [ text candidate.name ]

-- Todo: if pos is set, render it absolutely
renderBallot : Ballot -> Html Msg
renderBallot ballot =
    div ( style "width" "250px" ::
        style "margin" "auto" ::
        DragDrop.draggable DragDropMsg ballot )
         [ p [] [ text ballot.choice1 ]
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
