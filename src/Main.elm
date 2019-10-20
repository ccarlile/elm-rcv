port module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (style, attribute, class, href)
import Html.Events exposing (onClick)
import Extra exposing (maybeFlatten)
import Set
import Browser exposing (element)
import Ballots exposing (Ballot)
import Candidates exposing (..)
import Html5.DragDrop as DragDrop
import Json.Decode exposing (Value)

port dragstart : Value -> Cmd msg

type Model
    = Intro
    | Game GameState
    | Elimination EliminationState
    | PreElimination PreEliminationState
    | PostElimination PostEliminationState
    | PickWinner PickWinnerState
    | Winner Candidate

type alias Round = List Candidate

type LastGuess
    = Correct
    | Incorrect

type alias EliminationState =
    { round : Round
    , wasCorrect : Maybe LastGuess
    }

type alias PickWinnerState =
    { round : Round
    , winners : List Candidate
    , wasCorrect : Maybe LastGuess
    }

type alias PreEliminationState =
    { round: Round
    , wasCorrect : Maybe LastGuess
    }

type alias PostEliminationState =
    { nextRound: Round
    , eliminatedCandidate : CandidateName
    , nextBallots : List Ballot
    }
    
type alias GameState =
    -- Ballots reset every round
    { unsortedBallots: List Ballot 
    , currentBallot: Ballot
    , currentRound: Round
    , lastGuess: Maybe LastGuess
    , dragDrop : DragDrop.Model Ballot CandidateName
    , votesCounted : Int
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

eliminateBallots: Round -> List Ballot ->  List Ballot
eliminateBallots round allBallots =
    List.filter ( keepBallot round ) allBallots

losers : Round -> List CandidateName
losers round =
    let
        allCandidatesSet = Set.fromList defaultCandidates
        roundSet = List.map .name round |> Set.fromList
    in
        Set.diff allCandidatesSet roundSet |> Set.toList
    

keepBallot : Round -> Ballot -> Bool
keepBallot round ballot =
    let
        eliminated = losers round
        toElim = List.length eliminated
        names = List.map .name round

    in
        if ( toElim == 1
                 && List.member ballot.choice1 eliminated )
            then True

        else if ( toElim == 2 &&
                      (
                       ( List.member ballot.choice1 eliminated && List.member ballot.choice2 eliminated )
                      || (List.member ballot.choice2 names && List.member ballot.choice3 names) ) )
            then True
        else False

-- Message types
type Msg
    = NoOp
    | PlayGame
    | DragDropMsg (DragDrop.Msg Ballot CandidateName)
    | IsCorrectToEliminate LastGuess
    | AttemptEliminate Candidate
    | HasNineVotes PickWinnerState
    | ChooseWinner Candidate
    | NextRound

init : () -> (Model, Cmd Msg)
init () =
    (Intro, Cmd.none)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        Elimination state ->
            case msg of 
                AttemptEliminate candidate ->
                    let
                        correctGuess
                            = List.sortBy .votes state.round
                            |> List.head
                            |> Maybe.map (\el -> el == candidate)
                            |> Maybe.withDefault False

                        nextCandidates
                            = if correctGuess then
                                  List.filter (\c -> not (c.name == candidate.name)) state.round
                              else state.round

                        newModel : Model
                        newModel 
                            = if correctGuess then
                                  eliminateBallots nextCandidates Ballots.ballots
                                      |> PostEliminationState nextCandidates candidate.name
                                      |> PostElimination
                              else Elimination( EliminationState state.round ( Just Incorrect ) )
                    in
                        ( newModel, Cmd.none )
                _ -> ( model , Cmd.none )
                    
        PickWinner state ->
            case msg of
                ChooseWinner candidate ->
                    ( Winner candidate, Cmd.none )
                _ -> ( model, Cmd.none )
        Game game ->
            updateGame msg game
        PostElimination state ->
            case msg of
                NextRound ->
                    ( initializeGame state.nextRound state.nextBallots, Cmd.none )
                _ -> ( model, Cmd.none )
                    
        PreElimination round ->
            case msg of
              IsCorrectToEliminate Correct ->
                  ( Elimination ( EliminationState round.round Nothing ), Cmd.none )
              IsCorrectToEliminate Incorrect ->
                  ( PreElimination  { round | wasCorrect = Just Incorrect }, Cmd.none )
              HasNineVotes winners ->
                  ( PickWinner winners, Cmd.none )
              _ -> (model, Cmd.none)
              
        _ ->
            case msg of
                PlayGame ->
                    (initializeGame (List.map (\name -> Candidate name 0) defaultCandidates) Ballots.ballots
                    , Cmd.none )
                _ -> ( model, Cmd.none )

initializeGame : Round -> List Ballot -> Model
initializeGame round ballots =
    case ballots of
        first :: rest  ->
            Game ( GameState rest first round Nothing DragDrop.init 0)
        [] -> Winner { name = "Vladmir Putin", votes = 1000000 }

updateGame : Msg -> GameState -> ( Model, Cmd Msg )
updateGame msg game =
   case msg of
       DragDropMsg msg_ ->
           let
               ( model_, result ) =
                   DragDrop.update msg_ game.dragDrop

               dropId = DragDrop.getDropId model_

               dragPort =
                   DragDrop.getDragstartEvent msg_
                       |> Maybe.map (.event >> dragstart)
                       |> Maybe.withDefault Cmd.none

               correctDrag =
                   case result of
                       Just ( ballot, candidateName, _) ->
                           if ballotTarget game.currentRound ballot == candidateName then
                               Just True
                           else Just False
                       _ -> Nothing
                           
               newModel : ( Model, Cmd Msg )
               newModel =
                   -- on drag success, three outcomes
                   -- ballot dragged on incorrect place. Update lastgess and don't change anything else
                   -- ballot dragged on correct place, and further ballots remain. Update votes and iterate
                   -- ballot dragged on correct candidate, and no further ballots remain. Move to elimination
                   case (correctDrag, game.unsortedBallots) of
                       ( Just False, _ ) -> ( Game { game | dragDrop = model_, lastGuess = Just Incorrect }
                                           , dragPort )

                       ( Nothing, _ ) -> ( Game { game | dragDrop = model_ }
                                           , dragPort )

                       ( Just True, next :: nextUnsorted ) ->
                           ( Game { game | unsortedBallots = nextUnsorted
                               , currentBallot = next
                               , currentRound = castBallot game.currentBallot game.currentRound
                               , lastGuess = Just Correct
                               , dragDrop = model_
                           }, dragPort )

                       ( Just True, [] ) ->
                           let
                               lastRound = game.currentRound
                               elimRound = castBallot game.currentBallot game.currentRound
                           in
                               ( PreElimination (PreEliminationState elimRound Nothing ), dragPort )

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
            div [ style "text-align" "center"] [
                 div [ class "htb-container htb-align-content-center"
                     , style "display" "inline-block" ]
                     [ h1 [] [ text "Welcome to the RCV simulator!"]
                     , p [] [ text "Sixteen people are trying to determine which kind of pizza to buy. Every person lists their top 3 pizzas, in order of preference, on their ballot." ]
                     , p [] [ text "In the first round, drag each ballot to the stack that matches its first vote." ]
                     , button [ onClick PlayGame,  class "htb-btn", style "margin-top" "1em" ] [ text "Count Votes"]
                 ]
                ]
        Game game -> 
            renderGame game
        Elimination state ->
            renderElimination state
        PreElimination state ->
            renderPreElimination state
        PostElimination state ->
            renderPostElimination state
        PickWinner winners ->
            renderPickWinners winners
        Winner candidate ->
            div [ style "text-align" "center" ]
                [ div [ style "display" "inline-block" ]
                      [ h1 [] [ text "Congratulations!" ]
                         , text "As you can see, Ranked Choice Voting allows us to easily determine which type of pizza is most preferred by most people. To play again, "
                         , a [ onClick PlayGame
                             , style "color" colorHex
                             , href "#"] [ text "click here." ] ]
                , div [ style "display" "flex"
                      , style "justify-content" "center"
                      , style "flex-wrap" "wrap"] [ div (candidateStyle) (candidateHtml candidate) ]
                ]

renderPickWinners : PickWinnerState -> Html Msg
renderPickWinners state =
    let
        winners = state.winners
        last = state.round

        isWinner : Candidate -> Bool
        isWinner cand =
            List.map .name winners
                |> List.member cand.name

        render : Candidate -> Html Msg
        render cand =
            isWinner cand |> renderCandidateWinner cand
    in
        div [ style "text-align" "center" ]
            [ div [ style "display" "inline-block" ]
                  [ text "Now, look at the total votes and click the winning pizza"
                  , div [ style "display" "flex"
                        , style "justify-content" "center"
                        , style "flex-wrap" "wrap"]
                      (List.map render last)
              ]
        ]
        
renderPostElimination : PostEliminationState -> Html Msg
renderPostElimination state =
    let
        lastLoserText = String.join "" [ "Correct! Now you will see the ballots of the voters who chose "
                                       , state.eliminatedCandidate
                                       , ". On each ballot, "
                                       , state.eliminatedCandidate
                                       , " will be crossed out, since it is eliminated."
                                       ]

    in
        div [ style "text-align" "center" ]
            [ div [ style "display" "inline-block" ]
                  [ text lastLoserText
                  , div [] [ button [ onClick NextRound, class "htb-btn", style "margin-top" "1em" ] [ text "Next Round" ] ]
                  ]
        ]
        
renderPreElimination: PreEliminationState -> Html Msg
renderPreElimination state =
    let
        round = state.round

        hasAnyCandidateRecievedNineVotes
            = List.map .votes round
            |> List.maximum
            |> Maybe.map (\v -> v >= 9)
            |> Maybe.withDefault False

        yesButtonOnClick
            = if hasAnyCandidateRecievedNineVotes then
                  HasNineVotes (PickWinnerState round (List.filter (\c -> c.votes >= 9) round) Nothing)
              else IsCorrectToEliminate Incorrect

        noButtonOnClick
            = if hasAnyCandidateRecievedNineVotes then
                  IsCorrectToEliminate Incorrect
              else IsCorrectToEliminate Correct

    in
                 
        div [ style "text-align" "center" ]
            [ div [ style "display" "inline-block"] [text "Next we need to determine if any candidate has a majority. With 16 ballots, a majority would be 9 votes. Has any pizza received 9 votes?"]
            , div [] [ button [ onClick yesButtonOnClick, class "htb-btn", style "margin-top" "1em" ] [ text "Yes!" ]
                     , button [ onClick noButtonOnClick, class "htb-btn", style "margin-top" "1em" ] [ text "No" ]
                     ]
            , div [ style "display" "flex"
                  , style "justify-content" "center"
                  , style "flex-wrap" "wrap"] ( [ renderLastGuess state.wasCorrect ] ++ (List.map renderCandidatePreElim round) )
        ]
    

renderElimination: EliminationState -> Html Msg
renderElimination state =
    div [ style "text-align" "center" ]
        [ div [ style "display" "inline-block" ] [text "Since no candidate received a majority, we eliminate the least popular pizza and move those ballots to the voters' next available choices. Click on the least popular pizza"]
        , div [ style "display" "flex"
              , style "justify-content" "center"
              , style "flex-wrap" "wrap"] ( [ renderLastGuess state.wasCorrect ] ++ (List.map renderCandidateElim state.round) )
        ]

renderLastGuess : Maybe LastGuess -> Html Msg
renderLastGuess lastGuess =
    let
        commonStyle =
            [ style "width" "100%"
            , style "margin-left" "3em"
            , style "margin-right" "3em"
            , style "margin-top" ".5em"
            , style "padding-bottom" ".5em"
            , style "text-align" "center"
            , style "border-radius" "5px"
            , style "color" "white"
            ]

        correct =
            style "background-color" "#387e82"
        incorrect =
            style "background-color" "#D75835"

    in
        case lastGuess of
            Just Correct -> div ( correct :: commonStyle ) [ text "Correct!" ]
            Just Incorrect -> div ( incorrect :: commonStyle ) [ text "Incorrect. Please try again" ]
            Nothing -> div [] []
                                                              
    
renderGame : GameState -> Html Msg
renderGame game =
    let
        dropId
            = DragDrop.getDropId game.dragDrop

        target = ballotTarget game.currentRound game.currentBallot

        instructions = div [ style "display" "inline-block"
                           , style "margin" "1em" ]
                       [ text "Drag each ballot onto the matching candidate."]

    in
        div [ style "text-align" "center" ]
            [ instructions,
                  div [ style "display" "flex"
                  , style "justify-content" "center"
                  , style "flex-wrap" "wrap"]
                  ([renderLastGuess game.lastGuess]
                       ++ ( List.map (renderCandidate dropId target) game.currentRound ))
            , div [ style "display" "flex"] [ renderBallot game.currentBallot game.currentRound ]
        ]

candidateStyle =
     [ style "width" "161px"
         , style "height" "100px"
         , style "margin" "1.5em 1em"
         , style "padding-top" "1em"
         , style "padding-bottom" "2em"
         , style "border-radius" "5px"
         , style "background-color" "#387e82"
         , style "text-align" "center"
         , style "color" "white"
         ]

candidateHtml : Candidate -> List (Html Msg)
candidateHtml candidate =
    [ h3 [] [ text candidate.name ]
    , h5 [] [ text (String.join "" [ "(Votes: " , (String.fromInt candidate.votes) , ")" ])
            ]
    ]
    
                          
        
renderCandidateElim : Candidate -> Html Msg
renderCandidateElim candidate =
    div ( onClick (AttemptEliminate candidate) :: candidateStyle) (candidateHtml candidate)
        
renderCandidatePreElim : Candidate -> Html Msg
renderCandidatePreElim candidate =
    div ( candidateStyle) (candidateHtml candidate)
    
renderCandidateWinner : Candidate -> Bool -> Html Msg
renderCandidateWinner candidate isWinner =
    let
        onclick : List ( Attribute Msg )
        onclick = if isWinner then
                      [ onClick (ChooseWinner candidate) ]
                  else []
    in 
        div (onclick ++ candidateStyle) (candidateHtml candidate)
        -- div "poop" (candidateHtml candidate)
    
                      
        
renderCandidate : Maybe CandidateName -> CandidateName -> Candidate -> Html Msg
renderCandidate currentlyDroppingOver correctCandidate candidateToRender =
    -- no target: regular outline
    -- drop target, and dropTarget == this candi
    let
        outline =
            case currentlyDroppingOver of
                Just candidateName ->
                    if ( (candidateToRender.name == correctCandidate) && (candidateName == correctCandidate) )
                        then [ style "border" "3px dotted green" ]
                    else [style "border" "3px dotted red"]
                Nothing -> [ style "border" "3px solid #387e82"]

        droppable =
            DragDrop.droppable DragDropMsg candidateToRender.name
    in
        div (outline
            ++ droppable
            ++ candidateStyle
            ) (candidateHtml candidateToRender)

-- Todo: if pos is set, render it absolutely
renderBallot : Ballot -> Round -> Html Msg
renderBallot ballot round =
    let
        choiceStyle
            = \name ->
              if (losers round |> List.member name) then
                  [ style "text-decoration" "line-through"
                  , style "color" "red"
                  ]
              else []
    in
        div ( style "width" "161px" ::
                  style "border" "3px solid black" ::
                  style "border-radius" "5px" ::
                  style "padding" "1em" ::
                  style "margin" "auto" ::
                  DragDrop.draggable DragDropMsg ballot )
            [ h4 [] 
                  [ text "Favorite Pizza Ballot"]
            , ol [] [ li ( choiceStyle ballot.choice1) [ text ballot.choice1  ]
                    , li ( choiceStyle ballot.choice2 ) [ text ballot.choice2  ]
                    , li ( choiceStyle ballot.choice3 ) [ text ballot.choice3  ]
                 ]
         ]

subscriptions model =
    Sub.none

colorHex : String
colorHex =
    "#387e82"

main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
