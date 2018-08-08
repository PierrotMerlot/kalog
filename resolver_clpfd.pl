:- module(resolver_clpfd, [
resolver_clpfd/1,
chequeo_fila_col/3
]).

:-use_module(library(clpfd)).

restriccionesMatrizFila([], []).
restriccionesMatrizFila([X|Xs], Constraints) :- restriccionesMatrizFila(Xs, Constraints1), 
                                                restriccionesFila(X, Constraints2, [], 0), 
                                                append(Constraints2, Constraints1, Constraints).

restriccionesFila([], [], [], 0) :- !.
restriccionesFila([], [ [[V|Vs]|[S]] ], [V|Vs], S) :- !.
restriccionesFila([X|Xs], Constraints, Vars, S) :- var(X), !, restriccionesFila(Xs, Constraints, [X|Vars], S).
restriccionesFila([X|Xs], Constraints, Vars, S) :- integer(X), !, restriccionesFila(Xs, Constraints, [X|Vars], S).
restriccionesFila([f(X)|Xs], Constraints, [], 0) :- integer(X), !, restriccionesFila(Xs, Constraints, [], X).
restriccionesFila([f(X)|Xs], [ [[V|Vs]|[S]] | Constraints], [V|Vs], S) :- integer(X), !, restriccionesFila(Xs, Constraints, [], X).
restriccionesFila([c(X)|Xs], Constraints, [], 0) :- integer(X), !, restriccionesFila(Xs, Constraints, [], 0).
restriccionesFila([c(X)|Xs], [ [[V|Vs]|[S]] | Constraints], [V|Vs], S) :-  integer(X), !, restriccionesFila(Xs, Constraints, [], 0).
restriccionesFila([p(X, Y)|Xs], Constraints, [], 0) :- integer(X), integer(Y), !, restriccionesFila(Xs, Constraints, [], X).
restriccionesFila([p(X, Y)|Xs], [ [[V|Vs]|[S]] | Constraints], [V|Vs], S) :- integer(X), integer(Y), !, restriccionesFila(Xs, Constraints, [], X).
restriccionesFila([X|Xs], Constraints, [], 0) :- atomic(X), X=n, !, restriccionesFila(Xs, Constraints, [], 0).
restriccionesFila([X|Xs], [ [[V|Vs]|[S]] | Constraints], [V|Vs], S) :- atomic(X), X=n, !, restriccionesFila(Xs, Constraints, [], 0).

restriccionesMatrizColumna([], []).
restriccionesMatrizColumna([X|Xs], Constraints) :- restriccionesMatrizColumna(Xs, Constraints1), 
                                                   restriccionesColumna(X, Constraints2, [], 0), 
                                                   append(Constraints2, Constraints1, Constraints).

restriccionesColumna([], [], [], 0) :- !.
restriccionesColumna([], [ [[V|Vs]|[S]] ], [V|Vs], S) :- !.
restriccionesColumna([X|Xs], Constraints, Vars, S) :- var(X), !, restriccionesColumna(Xs, Constraints, [X|Vars], S).
restriccionesColumna([X|Xs], Constraints, Vars, S) :- integer(X), !, restriccionesColumna(Xs, Constraints, [X|Vars], S).
restriccionesColumna([f(X)|Xs], Constraints, [], 0) :- integer(X), !, restriccionesColumna(Xs, Constraints, [], 0).
restriccionesColumna([f(X)|Xs], [ [[V|Vs]|[S]] | Constraints], [V|Vs], S) :- integer(X), !, restriccionesColumna(Xs, Constraints, [], 0).
restriccionesColumna([c(X)|Xs], Constraints, [], 0) :- integer(X), !, restriccionesColumna(Xs, Constraints, [], X).
restriccionesColumna([c(X)|Xs], [ [[V|Vs]|[S]] | Constraints], [V|Vs], S) :-  integer(X), !, restriccionesColumna(Xs, Constraints, [], X).
restriccionesColumna([p(X, Y)|Xs], Constraints, [], 0) :- integer(X), integer(Y), !, restriccionesColumna(Xs, Constraints, [], Y).
restriccionesColumna([p(X, Y)|Xs], [ [[V|Vs]|[S]] | Constraints], [V|Vs], S) :- integer(X), integer(Y), !, restriccionesColumna(Xs, Constraints, [], Y).
restriccionesColumna([X|Xs], Constraints, [], 0) :- atomic(X), X=n, !, restriccionesColumna(Xs, Constraints, [], 0).
restriccionesColumna([X|Xs], [ [[V|Vs]|[S]] | Constraints], [V|Vs], S) :- atomic(X), X=n, !, restriccionesColumna(Xs, Constraints, [], 0).

restriccionesMatriz(Tablero, Constraints) :- restriccionesMatrizFila(Tablero, C1), 
                                             transpose(Tablero, Traspuesta), 
                                             restriccionesMatrizColumna(Traspuesta, C2),
                                             append(C1, C2, Constraints).

imponerRestricciones([], Ac, Ac).
imponerRestricciones([ [Vars, Suma] |R ], Ac, Variables) :- 
    all_distinct(Vars),
    equation(Vars, Suma),
    append(Ac, Vars, Ac2),
    imponerRestricciones(R, Ac2, Variables).

equation([], 0) :- !.
equation([X|Xs], Ac):- var(X), X in 1..9, Ac2 #= Ac-X, !, equation(Xs, Ac2).
equation([X|Xs], Ac):- integer(X), Ac2 #= Ac-X, !, equation(Xs, Ac2). % Abarca el caso de que el número esté instanciado

resolver_clpfd(Tablero) :- 
    restriccionesMatriz(Tablero, Restricciones), 
    imponerRestricciones(Restricciones, [], Vs),
    list_to_set(Vs, Vss),
    label(Vss).


fila([F|_],1,F) :- !.
fila([_|Fs],I,X) :- I1 is I-1, fila(Fs,I1,X).

columna(T, N, C) :- columnaAc(T, N, N, [], C).
columnaAc([],_, _, Ac, C) :- !, reverse(Ac, C).
columnaAc([[X|_]|Fs],1, N, Ac, C) :- !, columnaAc(Fs, N, N, [X|Ac], C).
columnaAc([[_|Xs]|Fs],I, N, Ac, C) :- I2 is I-1, columnaAc([Xs|Fs], I2, N, Ac, C).

chequeo_fila_col(T, Fila, Columna) :-
    fila(T, Fila, F),
    columna(T, Columna, C),
    restriccionesFila(F, C1, [], 0),
    restriccionesColumna(C, C2, [], 0),
    append(C1, C2, Constraints),
    imponerRestricciones(Constraints, [], Vs),
    list_to_set(Vs, Vss),
    label(Vss).
