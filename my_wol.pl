
%ensure that the skeleton file is loaded 
:-ensure_loaded(war_of_life).



test_strategy( NGames, P1Strategy, P2Strategy) :-
    write('----------------------------------------------'), write('\n'),
    write('P1 using: '), write(P1Strategy), write('  | P2 using: '), write(P2Strategy),write('\n'),
    statistics(runtime, [T0|_]),
    test_strategy(NGames, P1Strategy, P2Strategy, Results, Games),
    statistics(runtime, [T1|_]),
    T is T1 - T0,                   % Execution time
    TAvg is T/NGames,
    %max_list(Games,Max), % For swipl
    %min_list(Games,Min),
    delete(Games,250,PGames),
    max_member(Max,PGames), % Pruned games, exclude exhaustion
    min_member(Min,Games),
    sumlist(Games,SumGames), length(Games,GamesLength),
    AvgGames is SumGames/GamesLength,
    findall(B, nth1(I,Results,r), Red), length(Red,RedWins),
    findall(B, nth1(I,Results,b), Blue), length(Blue,BlueWins),
    findall(B, nth1(I,Results,draw), Draws), length(Draws,DrawWins),
    Stalemates is (NGames - RedWins - BlueWins - DrawWins),
    write('Number of wins for player 1 (blue):'), write(BlueWins), write('\n'),
    write('Number of wins for player 2 (red):'), write(RedWins), write('\n'),
    write('Number of draws:'), write(DrawWins), write('\n'),
    write('Number of stalemates:'), write(Stalemates), write('\n'),    
    write('Longest (non-exhaustive) game: '), write(Max), write('\n'),
    write('Shortest Game: '), write(Min), write('\n'),
    write('Average Game Length (including exhaustives): '), write(AvgGames), write('\n'),
    format('Average game time: ~3f sec.~n', [TAvg/1000]).

test_strategy( 0, _, _, [], []) :- !.
test_strategy( NGames, P1s, P2s, Results, Games) :-
    play(quiet,P1s,P2s,NumMoves,Winner),
    N is NGames - 1,
    test_strategy(N,P1s,P2s,R,G),
    append(R,[Winner],Results),
    append(G,[NumMoves],Games).