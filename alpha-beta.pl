max(X,Y,Z):- ( 
    X > Y ->
    Z = X 
    ;
    Z = Y
).

min(X,Y,Z):- ( 
    X < Y ->
    Z = X 
    ;
    Z = Y
).

is_winning_state_x(S) :-
    winning_state_x(S), !.

winning_state_x(['x','x','x',_,_,_,_,_,_]). % [1,2,3]
winning_state_x([_,_,_,'x','x','x',_,_,_]). % [4,5,6]
winning_state_x([_,_,_,_,_,_,'x','x','x']). % [7,8,9]
winning_state_x(['x',_,_,'x',_,_,'x',_,_]). % [1,4,7]
winning_state_x([_,'x',_,_,'x',_,_,'x',_]). % [2,5,8]
winning_state_x([_,_,'x',_,_,'x',_,_,'x']). % [3,6,9]
winning_state_x(['x',_,_,_,'x',_,_,_,'x']). % [1,5,9]
winning_state_x([_,_,'x',_,'x',_,'x',_,_]). % [3,5,7]

is_winning_state_o(S) :-
    winning_state_o(S), !.

winning_state_o(['o','o','o',_,_,_,_,_,_]). % [1,2,3]
winning_state_o([_,_,_,'o','o','o',_,_,_]). % [4,5,6]
winning_state_o([_,_,_,_,_,_,'o','o','o']). % [7,8,9]
winning_state_o(['o',_,_,'o',_,_,'o',_,_]). % [1,4,7]
winning_state_o([_,'o',_,_,'o',_,_,'o',_]). % [2,5,8]
winning_state_o([_,_,'o',_,_,'o',_,_,'o']). % [3,6,9]
winning_state_o(['o',_,_,_,'o',_,_,_,'o']). % [1,5,9]
winning_state_o([_,_,'o',_,'o',_,'o',_,_]). % [3,5,7]

has_empty_position(['-'|_]) :- !.
has_empty_position([_|T]) :- has_empty_position(T).

is_a_draw(S) :-
    not(has_empty_position(S)).

children(Board, Player, Children) :-
    findall(NewBoard, make_move(Player, Board, NewBoard), Children).

make_move(max, ['-'|T], ['x'|T]).
make_move(min, ['-'|T], ['o'|T]).
make_move(Player, [H|T1], [H|T2]) :- make_move(Player, T1, T2).

alpha_beta(max,Board,Value, NextBoard):-
    ab_minimax(max,Board,-inf,inf,Value, NextBoard).
    
ab_minimax(max,Board,_,_,-1, NextBoard):-
    is_winning_state_o(Board), !.
ab_minimax(min,Board,_,_,1, NextBoard):-
    is_winning_state_x(Board), !. 
ab_minimax(_,Board,_,_,0, NextBoard):-
    is_a_draw(Board), !.
ab_minimax(max,Board,Alfa,Beta,Value, NextBoard):-
    children(Board, max, Children),
    ab_max_children(Children,Alfa,Beta,-inf,Value, NB, NextBoard).
ab_minimax(min,Board,Alfa,Beta,Value, NextBoard):-
    children(Board, min, Children),
    ab_min_children(Children,Alfa,Beta,inf,Value, NB, NextBoard).

ab_max_children([],_,_,Max,Max, NextBoard, NextBoard).
ab_max_children([H|T],Alfa,Beta,Max1,Max, NB, NextBoard):-
    ab_minimax(min,H,Alfa,Beta,Value, NextBoardX),
    ( 
        Value > Beta -> % Beta cut
            Max = Beta,
            NextBoard = H
        ; (
            max(Value,Alfa,Alfa1), % updates Alpha
            max(Value,Max1,Max2),
            (Max2 == Value -> NB1 = H; true),
            ab_max_children(T, Alfa1, Beta, Max2, Max, NB1, NextBoard)
        )
    ).

ab_min_children([],_,_,Min,Min, NextBoard, NextBoard).
ab_min_children([H|T],Alfa,Beta,Min1,Min, NB, NextBoard):-
    ab_minimax(max,H,Alfa,Beta,Value, NextBoardX),
    (
        Alfa > Value -> % Alpha cut
            Min = Alfa
            % NextBoard = H
        ; (
            min(Value,Beta,Beta1), % updates Beta
            min(Value,Min1,Min2),
            % (Min2 == Value -> NB1 = H; true),
            ab_min_children(T, Alfa, Beta1, Min2, Min, NB1, NextBoard)
        )
    ).

run(Board) :-
    print_board(Board),
    alpha_beta(max, Board, V, NextBoard),
    write('Valor: '), write(V), nl, nl,
    write('==============='), nl,
    print_board(NextBoard).

print_board([]) :- write('==============='), nl, nl, !.
print_board([A, B, C | Board]) :-
    tab(2), write(A),
    write('  │ '),
    write(B),
    write(' │  '),
    write(C), nl,
    (Board \= [] ->
        write('─────┼───┼─────'), nl;
        true),
    print_board(Board).