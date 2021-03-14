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

minimax(Folha,V):- !,
    ( Folha == max(N,[]); 
    Folha == min(N,[])),
    valor_folha(N,V).
    
minimax(max(_,F),V):-
    max_filhos(F,-inf,V).
    
minimax(min(_,F),V):-
    min_filhos(F,inf,V).
    
max_filhos([],Max,Max).

max_filhos([N|Nós],Max0,Max):-
    minimax(N,V),
    max(V,Max0,Max1),
    max_filhos(Nós,Max1,Max).
    
min_filhos([],Min,Min).
min_filhos([N|Nós],Min0,Min):-
    minimax(N,V),
    min(V,Min0,Min1),
    min_filhos(Nós,Min1,Min).