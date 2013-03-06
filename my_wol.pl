
%ensure that the skeleton file is loaded 
:-ensure_loaded(war_of_life).

test_strategy(N, P1Strategy, P2Strategy) :-
    write('----------------------------------------------'), write('\n'),
    statistics(runtime, [T0|_]),
    test_strategy_helper(N, P1Strategy, P2Strategy, Results, GamesMovesNumber),
    statistics(runtime, [T1|_]),
    T is T1 - T0,
    write(N), write('\n'),
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
    
% This is helper function for testing the strategy. It loops N times to obtain
% the information which would be required by the main function.
test_strategy_helper( 0, _, _, [], []) :- !.
test_strategy_helper( NGames, P1s, P2s, Results, Games) :-
    play(quiet,P1s,P2s,NumMoves,Winner),
    N is NGames - 1,
    test_strategy_helper(N,P1s,P2s,R,G),
    append(R,[Winner],Results),
    append(G,[NumMoves],Games).


opponent(r,b).
opponent(b,r).

empty(X, Y, [Red,Blue]) :-
    \+ member([X,Y], Red),
    \+ member([X,Y], Blue).

find_all_possible_moves(PieceColour, Board, AllMoves) :-
    findall(
        [X,Y,X1,Y1],
        (   cell(X,Y),
            what_in_cell(Board, X, Y, P),
            nieghbour_position(X,Y, [X1,Y1]),
            empty(X1, Y1, Board)
        ),
        AllMoves
        ).

make_move([X,Y,A1,Y1],[Blue,Red],[NewBlue,NewRed]) :-
    (
        member([X,Y],Blue),
        delete(Blue,[X,Y],PartialBlue),
        append([[X1,Y1]],PartialBlue,NewBlue),
        NewRed = Red, !
    ) ;
    (
        member([X,Y],Red),
        delete(Red,[X,Y],PartialRed),
        append([[X1,Y1]],PartialRed,NewRed),
        NewBlue = Blue, !
    ).

%This function is used to figure out the next move using the 
%bloodlust strategy which means the move which causes most pieces of
%opponent is chosen.
bloodlust(PieceColour, [Blue, Red], [NewBlue, NewRed], Move) :-
    opponent(PieceColour, Opp),
    find_all_possible_moves(PieceColour, [Blue,Red], AllMoves),

