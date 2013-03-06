
%ensure that the skeleton file is loaded 
:-ensure_loaded(war_of_life).

test_strategy(N, P1Strategy, P2Strategy) :-
    write('----------------------------------------------'), write('\n'),
    statistics(runtime, [T0|_]),
    test_strategy_helper(N, P1Strategy, P2Strategy, Results, GamesMovesNumber),
    statistics(runtime, [T1|_]),
    T is T1 - T0,
    AvgT is T/N,
    delete(GamesMovesNumber,250,PGames),
    max_member(Max,PGames),
    min_member(Min,GamesMovesNumber),
    sumlist(GamesMovesNumber,SumGames), length(GamesMovesNumber,GamesLength),
    AvgGames is SumGames/GamesLength,
    findall(B, nth1(I,Results,r), Red), length(Red,RedWins),
    findall(B, nth1(I,Results,b), Blue), length(Blue,BlueWins),
    findall(B, nth1(I,Results,draw), Draws), length(Draws,DrawWins),
    write('Number of draws:'), write(DrawWins), write('\n'), 
    write('Number of wins for player 1 (blue):'), write(BlueWins), write('\n'),
    write('Number of wins for player 2 (red):'), write(RedWins), write('\n'),
    write('Longest (non-exhaustive) game: '), write(Max), write('\n'),
    write('Shortest Game: '), write(Min), write('\n'),
    write('Average game length (including exhaustives): '), write(AvgGames), write('\n'),
    format('Average game time: ~3f seconds.~n', [AvgT/1000]).
    
/*
    This is helper function for testing the strategy. It loops N times to obtain
    the information which would be required by the main function.
 */
test_strategy_helper( 0, _, _, [], []) :- !.
test_strategy_helper( NGames, P1s, P2s, Results, Games) :-
    play(quiet,P1s,P2s,NumMoves,Winner),
    N is NGames - 1,
    test_strategy_helper(N,P1s,P2s,R,G),
    append(R,[Winner],Results),
    append(G,[NumMoves],Games).


opponent(r,b).
opponent(b,r).

colour_num(b,1).
colour_num(r,2).

empty(X, Y, [Red,Blue]) :-
    \+ member([X,Y], Red),
    \+ member([X,Y], Blue).

find_all_possible_moves(PieceColour, Board, AllMoves) :-
    findall(
        [X,Y,X1,Y1],
        (   cell(X,Y),
            what_in_cell(Board, X, Y, PieceColour),
            neighbour_position(X,Y, [X1,Y1]),
            empty(X1, Y1, Board)
        ),
        AllMoves
        ).

%case when moving a blue piece
make_move([X,Y,X1,Y1],[Blue,Red],[NewBlue,NewRed]) :-
    member([X,Y],Blue),
    delete(Blue,[X,Y],PartialBlue),
    append([[X1,Y1]],PartialBlue,NewBlue),
    NewRed = Red, !.

% case when moving a red piece.
make_move([X,Y,X1,Y1],[Blue,Red],[NewBlue,NewRed]) :-
    member([X,Y],Red),
    delete(Red,[X,Y],PartialRed),
    append([[X1,Y1]],PartialRed,NewRed),
    NewBlue = Blue, !.

minimum_comparison([NumPieces, _, _],[NumPieces1, _, _]) :-
    NumPieces < NumPieces1.

%This function is used to figure out the next move using the 
%bloodlust strategy which means the move which causes most pieces of
%opponent is chosen.
bloodlust(PieceColour, [Blue, Red], [NewBlue, NewRed], [X,Y,X1,Y1]) :-
    opponent(PieceColour, Opponent),
    colour_num(Opponent,OppNum),
    find_all_possible_moves(PieceColour, [Blue,Red], AllMoves),
    findall(
        [OppPiecesNumber, [X,Y,X1,Y1],AfterCrankState],
        (  member([X,Y,X1,Y1],AllMoves),
           make_move([X,Y,X1,Y1], [Blue,Red], NewState),
           next_generation(NewState,AfterCrankState),
           nth1(OppNum,AfterCrankState,OPStates), % gets us the part of the player 
                                                  % (i.e AfterCrankState[1] is for 
                                                  % blue and AfterCrankState[2] is for red)
           length(OPStates,OppPiecesNumber)
        ),
        MovesResults),
        min_member(minimum_comparison,Min,MovesResults),
        nth1(2,Min,[X,Y,X1,Y1]),
        make_move([X,Y,X1,Y1], [Blue,Red], [NewBlue,NewRed]).



self_preservation(PieceColour, [Blue,Red], [NewBlue,NewRed], [X,Y,X1,Y1]) :-
    colour_num(PieceColour, PlayerIndexNum),
    find_all_possible_moves(PieceColour, [Blue,Red], AllMoves),
    findall(
        [PiecesNumber, [X,Y,X1,Y1],AfterCrankState],
        (  member([X,Y,X1,Y1],AllMoves),
           make_move([X,Y,X1,Y1], [Blue,Red], NewState),
           next_generation(NewState,AfterCrankState),
           nth1(PlayerIndexNum,AfterCrankState,PStates),
           length(PStates,PiecesNumber)
        ),
        MovesResults),
    max_member(minimum_comparison, Max, MovesResults),
    nth1(2,Max,[X,Y,X1,Y1]),
    make_move([X,Y,X1,Y1], [Blue,Red], [NewBlue,NewRed]).

% the Score that we refer to is the (Number of Player’s pieces – Number of Opponent’s pieces)
% that we have to maximise in land_grab. We generate this in the land_grab_condition function.

land_grab(PieceColour, [Blue,Red], [NewBlue,NewRed], [X,Y,X1,Y1]) :-
    findall([Score, [PosX, PosY, PosX1, PosY1], AfterCrankState ],
            (land_grab_condition(PieceColour, [Blue,Red], Score, [PosX, PosY, PosX1, PosY1], AfterCrankState)),
            MovesResults),
    max_member(minimum_comparison, Max, MovesResults),
    nth1(2,Max,[X,Y,X1,Y1]),
    make_move([X,Y,X1,Y1], [Blue,Red], [NewBlue,NewRed]).

land_grab_condition(PieceColour, [Blue, Red], Score, [X,Y,X1,Y1], AfterCrankState) :-
    opponent(PieceColour, Opponent),
    colour_num(PieceColour, PlayerIndexNum),
    colour_num(Opponent, OpponentIndexNum),
    find_all_possible_moves(PieceColour, [Blue, Red], AllMoves),
    member([X,Y,X1,Y1], AllMoves),
    make_move([X,Y,X1,Y1],[Blue,Red], UpdatedState),
    next_generation(UpdatedState, AfterCrankState),
    nth1(PlayerIndexNum, AfterCrankState, PlayerStates),
    length(PlayerStates, PiecesNumber),
    nth1(OpponentIndexNum, AfterCrankState, OpponentStates),
    length(OpponentStates, OppPiecesNumber),
    Score is PiecesNumber-OppPiecesNumber.


minimax('b',[Blue,Red],[NewBlue,Red],Move) :- 
    minimax_make_move('b',Blue,Red,Move),
    alter_board(Move,Blue,NewBlue).

minimax('r',[Blue,Red],[Blue,NewRed],Move) :- 
     minimax_make_move('r',Red,Blue,Move),
     alter_board(Move,Red,NewRed).

%minimax(PieceColour, State, UpdatedState, [X,Y,X1,Y1]) :-

minimax_make_move(Colour,PlayerPieces,OppPieces,Move) :-
    findall(([X,Y,X1,Y1],Score),
            (
             member([X,Y],PlayerPieces),neighbour_position(X,Y,[X1,Y1]),
             \+member([X1,Y1],PlayerPieces),
             \+member([X1,Y1],OppPieces),
             minimax_score(Colour,[X,Y,X1,Y1],[PlayerPieces,OppPieces],Score)
            ),
            PossibleMoveScore),
     findall(Score, member((_,Score),PossibleMoveScore),ScoreList),
     max_member(minimum_comparison, MaxScore, ScoreList),
     member((Move,MaxScore),PossibleMoveScore).

minimax_score('b',Move,[Blue,Red],Score)
   :- alter_board(Move,Blue,NewBlue),
      next_generation([NewBlue,Red],NextState),
      land_grab('r',NextState,EndState,_AfterCrank),
      next_generation(EndState,[EndBlue,EndRed]),
      length(EndBlue,PlayerScore),
      length(EndRed,OppScore),
      Score is PlayerScore - OppScore.

minimax_score('r',Move,[Red,Blue],Score)
   :- alter_board(Move,Red,NewRed),
      next_generation([Blue,NewRed],NextState),
      land_grab('b',NextState,EndState,_AfterCrank),
      next_generation(EndState,[EndBlue,EndRed]),
      length(EndBlue,OppScore),
      length(EndRed,PlayerScore),
      Score is PlayerScore - OppScore.