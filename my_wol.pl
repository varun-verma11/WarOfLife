
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
    format('Average game time: ~3f sec.~n', [AvgT/1000]).
    
    

test_strategy_helper( 0, _, _, [], []) :- !.
test_strategy_helper( NGames, P1s, P2s, Results, Games) :-
    play(quiet,P1s,P2s,NumMoves,Winner),
    N is NGames - 1,
    test_strategy_helper(N,P1s,P2s,R,G),
    append(R,[Winner],Results),
    append(G,[NumMoves],Games).