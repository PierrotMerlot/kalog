:- module(resolver_std, [
resolver_std/2
]).

% asignarBloque(+Digitos, +Suma, ?Bloque) ← Bloque es es un subconjunto de Digitos tal que la suma de sus elementos es S
asignarBloque(_, 0, []).
asignarBloque(L1, S, [X|L2]) :- select(X, L1, Resto), S2 is S-X, S2>=0, asignarBloque(Resto, S2, L2).

chequearFilas([]) :- !.
chequearFilas([X|Xs]) :- chequearValoresFila(X, [], 0), chequearFilas(Xs).

chequearValoresFila([], _, 0).
chequearValoresFila([X|Xs], Vb, S) :- integer(X), !, not(member(X, Vb)), S2 is S-X, S2>=0, chequearValoresFila(Xs, [X|Vb], S2).
chequearValoresFila([f(X)|Xs], _, _) :- integer(X), !, chequearValoresFila(Xs, [], X).
chequearValoresFila([c(X)|Xs], _, _) :- integer(X), !, chequearValoresFila(Xs, [], 0).
chequearValoresFila([p(X, Y)|Xs], _, _) :- integer(X), integer(Y), !, chequearValoresFila(Xs, [], X).
chequearValoresFila([X|Xs], _, _) :- atomic(X), X=n, !, chequearValoresFila(Xs, [], 0).

asignarColumnas([]) :- !.
asignarColumnas([X|Xs]) :- asignarValoresColumna(X, [], 0), asignarColumnas(Xs).

asignarValoresColumna([], Vb, S) :- asignarBloque([1,2,3,4,5,6,7,8,9], S, Vb).
asignarValoresColumna([X|Xs], Vb, S) :- var(X), !, asignarValoresColumna(Xs, [X|Vb], S).
asignarValoresColumna([f(X)|Xs], Vb, S) :- integer(X), asignarBloque([1,2,3,4,5,6,7,8,9], S, Vb), asignarValoresColumna(Xs, [], 0).
asignarValoresColumna([c(X)|Xs], Vb, S) :- integer(X), asignarBloque([1,2,3,4,5,6,7,8,9], S, Vb), asignarValoresColumna(Xs, [], X).
asignarValoresColumna([p(X, Y)|Xs], Vb, S) :- integer(X), integer(Y), asignarBloque([1,2,3,4,5,6,7,8,9], S, Vb), asignarValoresColumna(Xs, [], Y).
asignarValoresColumna([X|Xs], Vb, S) :- atomic(X), X=n, asignarBloque([1,2,3,4,5,6,7,8,9], S, Vb), asignarValoresColumna(Xs, [], 0).

resolver_std(Tablero, TableroInstanciado) :- instanciarTableroColumnas(Tablero, TableroInstanciado), chequearTableroFilas(TableroInstanciado).

chequearTableroFilas(Tablero) :- chequearFilas(Tablero).
instanciarTableroColumnas(Tablero, Tablero) :- traspuesta(Tablero, Traspuesta), asignarColumnas(Traspuesta).

% traspuesta(+A, ?T) ← T es la matriz traspuesta de A.
traspuesta([], []).
traspuesta(A, [Ts|Tss]) :- trasponer_columna(A, Ts, A1), traspuesta(A1, Tss).

% trasponer_columna(+A,?L,?B) ← Recoge en formato lista la primera columna de la matriz A (en el argumento L) mientras que B es la matriz resultante de recortar dicha columna.
trasponer_columna([[X]], [X], []). % matriz fila de un solo elemento
trasponer_columna([[X,X1|Xs]], [X], [[X1|Xs]]). % matriz fila de más de un elemento
trasponer_columna([[X]|As], [X|Ys], Bs) :- trasponer_columna(As, Ys, Bs). % CASO fila de un elemento
trasponer_columna([[X,X1|Xs]|As], [X|Ys], [[X1|Xs]|Bs]) :- trasponer_columna(As, Ys, Bs). % CASO fila de más de un elemento
