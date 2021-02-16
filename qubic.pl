:- dynamic valor/2.

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

tamanho([], 0).
tamanho([_|T], N) :- tamanho(T, N1), N is 1 + N1.

eh_estado_final(S) :-
    eh_estado_ganhador_x(S);
    eh_estado_ganhador_o(S);
    eh_velha(S).

eh_estado_ganhador_x(S) :-
    estado_ganhador_x(S).

estado_ganhador_x(['x','x','x',_,_,_,_,_,_]). % [1,2,3]
estado_ganhador_x([_,_,_,'x','x','x',_,_,_]). % [4,5,6]
estado_ganhador_x([_,_,_,_,_,_,'x','x','x']). % [7,8,9]
estado_ganhador_x(['x',_,_,'x',_,_,'x',_,_]). % [1,4,7]
estado_ganhador_x([_,'x',_,_,'x',_,_,'x',_]). % [2,5,8]
estado_ganhador_x([_,_,'x',_,_,'x',_,_,'x']). % [3,6,9]
estado_ganhador_x(['x',_,_,_,'x',_,_,_,'x']). % [1,5,9]
estado_ganhador_x([_,_,'x',_,'x',_,'x',_,_]). % [3,5,7]

eh_estado_ganhador_o(S) :-
    estado_ganhador_o(S).

estado_ganhador_o(['o','o','o',_,_,_,_,_,_]). % [1,2,3]
estado_ganhador_o([_,_,_,'o','o','o',_,_,_]). % [4,5,6]
estado_ganhador_o([_,_,_,_,_,_,'o','o','o']). % [7,8,9]
estado_ganhador_o(['o',_,_,'o',_,_,'o',_,_]). % [1,4,7]
estado_ganhador_o([_,'o',_,_,'o',_,_,'o',_]). % [2,5,8]
estado_ganhador_o([_,_,'o',_,_,'o',_,_,'o']). % [3,6,9]
estado_ganhador_o(['o',_,_,_,'o',_,_,_,'o']). % [1,5,9]
estado_ganhador_o([_,_,'o',_,'o',_,'o',_,_]). % [3,5,7]

lista_posicoes_vazias([], _, Acc, Acc).
lista_posicoes_vazias(['-'|T], Pos, Acc, ListaPos) :-
    append(Acc, [Pos], Acc1),
    Pos1 is Pos + 1,
    lista_posicoes_vazias(T, Pos1, Acc1, ListaPos).
lista_posicoes_vazias(['x'|T], Pos, Acc, ListaPos) :-
    Pos1 is Pos + 1,
    lista_posicoes_vazias(T, Pos1, Acc, ListaPos).
lista_posicoes_vazias(['o'|T], Pos, Acc, ListaPos) :-
    Pos1 is Pos + 1,
    lista_posicoes_vazias(T, Pos1, Acc, ListaPos).

eh_velha(S) :-
    lista_posicoes_vazias(S,1,[],ListaPosVazias),
    tamanho(ListaPosVazias, 0).

faca_movimento(max, ['-'|T], ['x'|T]).
faca_movimento(min, ['-'|T], ['o'|T]).
faca_movimento(Player, [H|T1], [H|T2]) :- faca_movimento(Player, T1, T2).

alfa_beta(max,Board,Valor):-
    ab_minimax(max,Board,-inf,inf,Valor).

ab_minimax(max,Board,_,_,-1):-
    eh_estado_ganhador_o(Board), !.
ab_minimax(min,Board,_,_,1):-
    eh_estado_ganhador_x(Board), !.
ab_minimax(_,Board,_,_,0):-
    eh_velha(Board), !. % isso pode ser substituído add Profundidade
ab_minimax(max,Board,Alfa,Beta,Valor):-
    ab_max_filhos(Board,Alfa,Beta,-inf,Valor).
ab_minimax(min,Board,Alfa,Beta,Valor):-
    ab_min_filhos(Board,Alfa,Beta,inf,Valor).

ab_max_filhos(Board,_,_,Max,Max) :-
    eh_estado_final(Board), !.
ab_max_filhos(Board,Alfa,Beta,Max1,Max):-
    faca_movimento(max,Board,NewBoard),
    ab_minimax(min,NewBoard,Alfa,Beta,Valor),
    ( 
        Valor > Beta -> % corte Beta : não trata Fs
        Max = Beta 
        ; 
        max(Valor,Alfa,Alfa1), % atualiza Alfa
        max(Valor,Max1,Max2),
        ab_max_filhos(Board,Alfa1,Beta,Max2,Max)
    ).

ab_min_filhos(Board,_,_,Min,Min) :-
    eh_estado_final(Board), !.
ab_min_filhos(Board,Alfa,Beta,Min1,Min):-
    faca_movimento(min,Board,NewBoard),
    ab_minimax(max,NewBoard,Alfa,Beta,Valor),
    (
        Alfa > Valor -> % corte Alfa: não trata Fs
        Min = Alfa
        ; 
        min(Valor,Beta,Beta1), % atualiza Beta
        min(Valor,Min1,Min2),
        ab_min_filhos(Board,Alfa,Beta1,Min2,Min)
    ).

% valor(l, 1).
% valor(m, -1).
% valor(h, -1).
% valor(f, 1).
% valor(g, 1).
% valor(d, -1).

% Testing the alfa_beta algorithm

% ?- T = max(a,[min(b,[max(e,[min(h,[]),min(i,[max(l,[]),max(m,[])])])]),min(c,[max(f,[]),max(g,[])]),min(d,[])]), alfa_beta(T, V).
% T = max(a, [min(b, [max(e, [min(h, []), min(i, [max(..., ...)|...])])]), min(c, [max(f, []), max(g, [])]), min(d, [])]),
% V = 1 .

% It seems to be working, it returned the same result minimax did. 
    

tic_tac_toe_generator(_, Board, Board, Valor) :- 
    (
        eh_estado_ganhador_x(Board) -> Valor = 1,  writeln('max won! :)'), nl; % player wins
        eh_estado_ganhador_o(Board) -> Valor = -1, writeln('max lost! :('), nl; % opponent wins
        eh_velha(Board) -> Valor = 0, writeln('velha! :|'), nl % tie
    ),
    write('==============='), nl, print_board(Board),
    !.

tic_tac_toe_generator(max, CurrBoard, FinalBoard, Valor) :-
    max_filhos(CurrBoard, FinalBoard, -inf, Valor).
tic_tac_toe_generator(min, CurrBoard, FinalBoard, Valor) :- 
    min_filhos(CurrBoard, FinalBoard, inf, Valor).

max_filhos(CurrBoard, FinalBoard, Max0, Max) :-
    faca_movimento(max, CurrBoard, NewBoard),
    write('==============='), nl, print_board(NewBoard),
    tic_tac_toe_generator(min, NewBoard, FinalBoard, Valor),
    max(Valor, Max0, Max).

min_filhos(CurrBoard, FinalBoard, Min0, Min) :-
    faca_movimento(min, CurrBoard, NewBoard),
    write('==============='), nl, print_board(NewBoard),
    tic_tac_toe_generator(max, NewBoard, FinalBoard, Valor),
    min(Valor, Min0, Min).

r :-
    open('hogwarts.txt',write,Stream),
    run(Stream),
    close(Stream).

run(Stream) :-
    tic_tac_toe_generator(max, ['x','-','o','-','x','o','x','-','-'], B, Max),
    write(Stream,'Max: '), write(Stream,Max),  nl(Stream),
    %write('==============='), nl, print_board(B), 
    %write(Max), nl,
    fail. 
run(Stream).

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