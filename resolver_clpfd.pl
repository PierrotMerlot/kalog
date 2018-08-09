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

hBlock([], I, Ac, Ac) :- I=<0, !.
hBlock([X|Xs], I, Ac, Bloque) :- var(X), !, I2 is I-1, hBlock(Xs, I2, [X|Ac], Bloque).
hBlock([X|Xs], I, Ac, Bloque) :- integer(X), !, I2 is I-1, hBlock(Xs, I2, [X|Ac], Bloque).
hBlock([f(X)|_], I, Ac, Ac) :- integer(X), I=<0, !.
hBlock([f(X)|Xs], I, _, Bloque) :- integer(X), !, I2 is I-1, hBlock(Xs, I2, [f(X)], Bloque).
hBlock([c(X)|_], I, Ac, Ac) :- integer(X), I=<0, !.
hBlock([c(X)|Xs], I, _, Bloque) :- integer(X), !, I2 is I-1, hBlock(Xs, I2, [], Bloque).
hBlock([p(X, Y)|_], I, Ac, Ac) :- integer(X), integer(Y), I=<0, !.
hBlock([p(X, Y)|Xs], I, _, Bloque) :- integer(X), integer(Y), !, I2 is I-1, hBlock(Xs, I2, [p(X, Y)], Bloque).
hBlock([X|_], I, Ac, Ac) :- atomic(X), X=n, I=<0, !.
hBlock([X|Xs], I, _, Bloque) :- atomic(X), X=n, !, I2 is I-1, hBlock(Xs, I2, [], Bloque).

bloqueHorizontal(Fila, IndiceCol, Bloque) :- hBlock(Fila, IndiceCol, [], Bloque2), reverse(Bloque2, Bloque).

columna(T, N, C) :- columnaAc(T, N, N, [], C).
columnaAc([],_, _, Ac, C) :- !, reverse(Ac, C).
columnaAc([[X|_]|Fs],1, N, Ac, C) :- !, columnaAc(Fs, N, N, [X|Ac], C).
columnaAc([[_|Xs]|Fs],I, N, Ac, C) :- I2 is I-1, columnaAc([Xs|Fs], I2, N, Ac, C).

vBlock([], I, Ac, Ac) :- I=<0, !.
vBlock([X|Xs], I, Ac, Bloque) :- var(X), !, I2 is I-1, vBlock(Xs, I2, [X|Ac], Bloque).
vBlock([X|Xs], I, Ac, Bloque) :- integer(X), !, I2 is I-1, vBlock(Xs, I2, [X|Ac], Bloque).
vBlock([f(X)|_], I, Ac, Ac) :- integer(X), I=<0, !.
vBlock([f(X)|Xs], I, _, Bloque) :- integer(X), !, I2 is I-1, vBlock(Xs, I2, [], Bloque).
vBlock([c(X)|_], I, Ac, Ac) :- integer(X), I=<0, !.
vBlock([c(X)|Xs], I, _, Bloque) :- integer(X), !, I2 is I-1, vBlock(Xs, I2, [c(X)], Bloque).
vBlock([p(X, Y)|_], I, Ac, Ac) :- integer(X), integer(Y), I=<0, !.
vBlock([p(X, Y)|Xs], I, _, Bloque) :- integer(X), integer(Y), !, I2 is I-1, vBlock(Xs, I2, [p(X, Y)], Bloque).
vBlock([X|_], I, Ac, Ac) :- atomic(X), X=n, I=<0, !.
vBlock([X|Xs], I, _, Bloque) :- atomic(X), X=n, !, I2 is I-1, vBlock(Xs, I2, [], Bloque).

bloqueVertical(Columna, IndiceFila, Bloque) :- vBlock(Columna, IndiceFila, [], Bloque2), reverse(Bloque2, Bloque).

chequeo_fila_col(T, Fila, Columna) :-
    fila(T, Fila, F),
    columna(T, Columna, C),
    bloqueHorizontal(F, Columna, BloqueH),
    bloqueVertical(C, Fila, BloqueV),
    restriccionesFila(BloqueH, C1, [], 0),
    restriccionesColumna(BloqueV, C2, [], 0),
    append(C1, C2, Constraints),
    imponerRestricciones(Constraints, [], _).
