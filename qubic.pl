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
    eh_estado_ganhador_x(S), !;
    eh_estado_ganhador_o(S), !;
    eh_velha(S), !.

eh_estado_ganhador_x(S) :-
    estado_ganhador_x(S), !.

set_element_at_nth0([_ | List1], CurrentIndex, Index, Elem, List2) :-
    CurrentIndex = Index,
    List2 = [Elem | List1].
set_element_at_nth0([First | List1], CurrentIndex, Index, Elem, List2) :-
    NextIndex is CurrentIndex + 1,
    set_element_at_nth0(List1, NextIndex, Index, Elem, List3),
    List2 = [First | List3].

/* set_element_at_nth0(+List1, +Index, +Elem, -List2)
 * Altera o elemento de List1 no Index para o Elem, gerando List2. */
set_element_at_nth0(List1, Index, Elem, List2) :-
    set_element_at_nth0(List1, 0, Index, Elem, List2).

/* query_symbols_by_indexes(+Indexes, +Board, -Symbols)
 * As posições na lista Indexes em Board correspondem aos símbolos na lista 
 * Symbols */
query_symbols_by_indexes([], _, []) :- !.
query_symbols_by_indexes([Index | Indexes], Board, [Symbol | Symbols]) :-
    nth0(Index, Board, Symbol),
    query_symbols_by_indexes(Indexes, Board, Symbols).

winning_sequence([0,1,2]).
winning_sequence([3,4,5]).
winning_sequence([6,7,8]).
winning_sequence([0,3,6]).
winning_sequence([1,4,7]).
winning_sequence([2,5,8]).
winning_sequence([0,4,8]).
winning_sequence([2,4,6]).

estado_ganhador_x(['x','x','x',_,_,_,_,_,_]). % [1,2,3]
estado_ganhador_x([_,_,_,'x','x','x',_,_,_]). % [4,5,6]
estado_ganhador_x([_,_,_,_,_,_,'x','x','x']). % [7,8,9]
estado_ganhador_x(['x',_,_,'x',_,_,'x',_,_]). % [1,4,7]
estado_ganhador_x([_,'x',_,_,'x',_,_,'x',_]). % [2,5,8]
estado_ganhador_x([_,_,'x',_,_,'x',_,_,'x']). % [3,6,9]
estado_ganhador_x(['x',_,_,_,'x',_,_,_,'x']). % [1,5,9]
estado_ganhador_x([_,_,'x',_,'x',_,'x',_,_]). % [3,5,7]

eh_estado_ganhador_o(S) :-
    estado_ganhador_o(S), !.

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

/* child(+Board, +Turn, -Child)
 * Dado o tabuleiro Board, se Turn ('X' ou 'O') jogar em alguma posição vazia
 * de Board, irá gerar Child */
child(Board, Turn, Child) :-
    nth0(Index, Board, '-'),
    set_element_at_nth0(Board, 0, Index, Turn, Child).

children(Board, _, []) :-
    bagof(Symbols, Indexes^(
        winning_sequence(Indexes),
        query_symbols_by_indexes(Indexes, Board, Symbols),
        (Symbols = ['X', 'X', 'X', 'X']; Symbols = ['O', 'O', 'O', 'O'])),
        Sequences), !,
    Sequences \= [].
children(Board, Turn, Children) :-
    (   bagof(Child, Turn^child(Board, Turn, Child), Children), !
    ;   Children = []).

tem_posicao_vazia(['-'|T]) :- !.
tem_posicao_vazia([_|T]) :- tem_posicao_vazia(T).

eh_velha(S) :-
    not(tem_posicao_vazia(S)).

filhos(Board, Player, Filhos) :-
    findall(NewBoard, faca_movimento(Player, Board, NewBoard), Filhos).

faca_movimento(max, ['-'|T], ['x'|T]).
faca_movimento(min, ['-'|T], ['o'|T]).
faca_movimento(Player, [H|T1], [H|T2]) :- faca_movimento(Player, T1, T2).

alfa_beta(max,Board,Valor):-
    open('output.txt',write,NextBoard),
    ab_minimax(max,Board,-inf,inf,Valor, NextBoard).

% ab_minimax(max,Board,_,_,1):-
%     eh_estado_ganhador_x(Board), !.
% ab_minimax(min,Board,_,_,-1):-
%     eh_estado_ganhador_o(Board), !.

ab_minimax(max,Board,_,_,-1, NextBoard):-
    eh_estado_ganhador_o(Board),
    write(NextBoard, '+\n'), !.
ab_minimax(min,Board,_,_,1, NextBoard):-
    eh_estado_ganhador_x(Board),  
    write(NextBoard, '+\n'), !.
ab_minimax(_,Board,_,_,0, NextBoard):-
    eh_velha(Board), 
    write(NextBoard, '+\n'), !. % isso pode ser substituído add Profundidade
ab_minimax(max,Board,Alfa,Beta,Valor, NextBoard):-
    filhos(Board, max, Filhos),
    ab_max_filhos(Filhos,Alfa,Beta,-inf,Valor, NextBoard).
ab_minimax(min,Board,Alfa,Beta,Valor, NextBoard):-
    filhos(Board, min, Filhos),
    ab_min_filhos(Filhos,Alfa,Beta,inf,Valor, NextBoard).

ab_max_filhos([],_,_,Max,Max, NextBoard).
    % eh_estado_final(Board), !.
ab_max_filhos([H|T],Alfa,Beta,Max1,Max, NextBoard):-
    ab_minimax(min,H,Alfa,Beta,Valor, NextBoard),
    ( 
        Valor > Beta -> % corte Beta : não trata Fs
        Max = Beta,
        write(NextBoard, 'corte beta\n') 
        ; (
        max(Valor,Alfa,Alfa1), % atualiza Alfa
        max(Valor,Max1,Max2),
        ab_max_filhos(T, Alfa1, Beta, Max2, Max, NextBoard))
    ).

ab_min_filhos([],_,_,Min,Min, NextBoard).
    % eh_estado_final(Board), !.
ab_min_filhos([H|T],Alfa,Beta,Min1,Min, NextBoard):-
    ab_minimax(max,H,Alfa,Beta,Valor, NextBoard),
    (
        Alfa > Valor -> % corte Alfa: não trata Fs
        Min = Alfa,
        write(NextBoard, 'corte alfa\n') 
        ; (
        min(Valor,Beta,Beta1), % atualiza Beta
        min(Valor,Min1,Min2),
        ab_min_filhos(T, Alfa, Beta1, Min2, Min, NextBoard))
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