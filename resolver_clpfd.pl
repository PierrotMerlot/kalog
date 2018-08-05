:- module(resolver_clpfd, [
resolver_clpfd/2,
resolver_only_fila_col/3
]).

:-use_module(library(clpfd)).


resolver_clpfd(T, S1):- resolver_clpfd_m(T, S1), transpose(S1, S), resolver_clpfd_m2(S).


resolver_clpfd_m([], []):- !.
resolver_clpfd_m([X|Xs], [Y|Ys]):-  resolver_clpfd_f(X,Y,[],0), resolver_clpfd_m(Xs, Ys).

resolver_clpfd_m2([]):- !.
resolver_clpfd_m2([X|Xs]):-  resolver_clpfd_c(X,[],0), resolver_clpfd_m2(Xs).

resolver_clpfd_f([],[],S, 0):- all_distinct(S), !.
resolver_clpfd_f([X|Xs],[Y|Ys], S, Ac):- var(X), Y in 1..9, Ac2 #= Ac-Y,!, resolver_clpfd_f(Xs, Ys, [Y|S], Ac2).
resolver_clpfd_f([X|Xs],[X|Ys], S, Ac):- integer(X), Ac2 #= Ac-X,!, resolver_clpfd_f(Xs, Ys, [X|S], Ac2). %Abarca el caso de que el nùmero esté instanciado
resolver_clpfd_f([f(X)|Xs],[f(X)|Ys],S,0):- integer(X),!,all_distinct(S), resolver_clpfd_f(Xs, Ys, [], X).
resolver_clpfd_f([c(X)|Xs],[c(X)|Ys],S,0):- integer(X),all_distinct(S),!, resolver_clpfd_f(Xs, Ys, [], 0).
resolver_clpfd_f([p(X, Y)|Xs],[p(X, Y)|Ys],S,0):- integer(X), integer(Y),all_distinct(S),!, resolver_clpfd_f(Xs, Ys, [], X).
resolver_clpfd_f([X|Xs],[X|Ys], S, 0):-nonvar(X),X=n, all_distinct(S),!, resolver_clpfd_f(Xs, Ys, [], 0).


resolver_clpfd_c([],S, 0):- all_distinct(S), label(S).
resolver_clpfd_c([X|Xs], S, Ac):- var(X),!, Ac2 #= Ac-X, resolver_clpfd_c(Xs, [X|S], Ac2).
resolver_clpfd_c([X|Xs], S, Ac):- integer(X),!, Ac2 #= Ac-X, resolver_clpfd_c(Xs, [X|S], Ac2). %Abarca el caso de que el nùmero esté instanciado
resolver_clpfd_c([X|Xs], S, 0):-nonvar(X),X=n,!, all_distinct(S), label(S), resolver_clpfd_c(Xs, [], 0).
resolver_clpfd_c([c(X)|Xs],S,0):- integer(X),!, all_distinct(S), label(S), resolver_clpfd_c(Xs, [], X).
resolver_clpfd_c([f(X)|Xs],S,0):- integer(X),!,all_distinct(S), label(S), resolver_clpfd_c(Xs, [], 0).
resolver_clpfd_c([p(Y, X)|Xs],S,0):- integer(X), integer(Y),!, all_distinct(S), label(S),  resolver_clpfd_c(Xs, [], X).


rowN([H|_],1,H):-!.
rowN([_|T],I,X) :-
    I1 is I-1,
    rowN(T,I1,X).

columnN([],_,[]).
columnN([H|T], I, [R|X]):-
   rowN(H, I, R), 
columnN(T,I,X).

resolver_clpfd_f_aux([],[],S, 0):- all_distinct(S), !.
resolver_clpfd_f_aux([X|Xs],[Y|Ys], S, Ac):- var(X), Y in 1..9, Ac2 #= Ac-Y,!, resolver_clpfd_f_aux(Xs, Ys, [Y|S], Ac2).
resolver_clpfd_f_aux([X|Xs],[X|Ys], S, Ac):- integer(X), Ac2 #= Ac-X,!, resolver_clpfd_f_aux(Xs, Ys, [X|S], Ac2). %Abarca el caso de que el nùmero esté instanciado
resolver_clpfd_f_aux([f(X)|Xs],[f(X)|Ys],S,0):- integer(X),all_distinct(S),!, resolver_clpfd_f_aux(Xs, Ys, [], 0).
resolver_clpfd_f_aux([c(X)|Xs],[c(X)|Ys],S,0):- integer(X),!,all_distinct(S), resolver_clpfd_f_aux(Xs, Ys, [], X).
resolver_clpfd_f_aux([p(X, Y)|Xs],[p(X, Y)|Ys],S,0):- integer(X), integer(Y),all_distinct(S),!, resolver_clpfd_f_aux(Xs, Ys, [], Y).
resolver_clpfd_f_aux([X|Xs],[X|Ys], S, 0):-nonvar(X),X=n, all_distinct(S),!, resolver_clpfd_f_aux(Xs, Ys, [], 0).

%resolver(-T,+F,+C)-> chequea que sea posible que la columna C y la fila F resuelvan el tablero
resolver_only_fila_col(T,F,C):-rowN(T,F,Row), columnN(T,C,Col), resolver_clpfd_f(Row,_,[],0), resolver_clpfd_f_aux(Col,_,[],0).