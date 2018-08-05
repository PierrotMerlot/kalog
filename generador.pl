:- module(generador, [
generarTableroNumeros/3
]).

:- use_module(library(clpfd)).


% --------------------------
% Módulo para generar tableros.
% --------------------------

% --------------------------
% 1) Generador de tableros vacíos (matrices) con casillas 'b' (blancas) o casillas 'n' (negras).
% --------------------------

% darValor(-X) ← X es 'b' (blanco) con probabilidad 0.75 o 'n' (negro) con probabilidad 0.25.
darValor(b) :- maybe(0.70), !. % maybe(+P) simula una Bernoulli de parámetro p
darValor(n).

% generarFila_n_or_b(+N, -F, -Cont) ← N es el largo de la fila, F es una fila con valores 'n' o 'b', 
% Cont es un contador de las cantidad de casillas blancas contiguas.
generarFila_n_or_b(0, [], _) :- !.
generarFila_n_or_b(N, [n|Xs], 9) :- !, N2 is N-1, generarFila_n_or_b(N2, Xs, 0).
generarFila_n_or_b(N, [X|Xs], _) :- N2 is N-1, darValor(X), X=n, !, generarFila_n_or_b(N2, Xs, 0).
generarFila_n_or_b(N, [b|Xs], Cont) :- N2 is N-1, Cont2 is Cont+1, generarFila_n_or_b(N2, Xs, Cont2).

% generarMatriz_n_or_b(+M, +N, -Matriz) ← Matriz es una matriz de elementos 'b' o 'n' de tamaño MxN.
generarMatriz_n_or_b(0, _, []) :- !.
generarMatriz_n_or_b(M, N, [F|Fs]) :- M2 is M-1, generarFila_n_or_b(N, F, 0), generarMatriz_n_or_b(M2, N, Fs).

% ajustarBloqueColumna(+F1, ?F2, ?Cont) ← F1 es una fila con columnas 'n' o 'b'. F2 se obtiene de F1 haciendo
% que los bloques de b's seguidas tengan largo menor a 10.
ajustarBloqueColumna([], [], _) :- !.
ajustarBloqueColumna([b|Xs], [n|Ys], 9) :- !, ajustarBloqueColumna(Xs, Ys, 0).
ajustarBloqueColumna([n|Xs], [n|Ys], _) :- !, ajustarBloqueColumna(Xs, Ys, 0).
ajustarBloqueColumna([b|Xs], [b|Ys], Cont) :- Cont2 is Cont+1, ajustarBloqueColumna(Xs, Ys, Cont2).

% ajustarBloque(+X,-Y) ← La matriz Y se obtiene de X aplicando el predicado ajustarBloqueColumna a cada una de sus filas 
% (columnas, ya que se utilizará con la matriz trapuesta).
ajustarBloque([],[]) :- !.
ajustarBloque([X|Xs],[Y|Ys]):- ajustarBloqueColumna(X,Y,0), ajustarBloque(Xs,Ys).

% agregarFila_llena_de_n(+N, +X, -Matriz) ← Matriz es el resultado de agregar una fila llena de n's a la matriz X, N es el tamaño de la fila.
agregarFila_llena_de_n(N, Xs, [F|Xs]) :- fila_llena_de_n(N, F).
fila_llena_de_n(0, []) :- !.
fila_llena_de_n(K, [n|Ys]) :- K2 is K-1, fila_llena_de_n(K2, Ys).

% agregarColumna_llena_de_n(+X, -Matriz) ← Matriz es el resultado de agregar una columna llena de n's a la matriz X.
agregarColumna_llena_de_n([],[]).
agregarColumna_llena_de_n([X|Xs], [[n|X]|Ys]) :- agregarColumna_llena_de_n(Xs, Ys).

% agregarColumnaFila_llena_de_n(+N ,+X, -Y) ← Y es el resultado de agregar una fila y una columna llenas de n's (N es el tamaño de la fila original), a la matriz X (si X era MxN, Y será (M+1)x(N+1)).
agregarColumnaFila_llena_de_n(N, X, Y) :- agregarFila_llena_de_n(N, X, Z), agregarColumna_llena_de_n(Z, Y).

% generarTablero_n_or_b(+M, +N, -S) ← S es una matriz MxN de elementos 'b' o 'n', donde la primera columna y la primera fila son todas n's.
generarTablero_n_or_b(M, N, S) :- M2 is M-1, N2 is N-1,
                                  generarMatriz_n_or_b(M2, N2, T), 
                                  agregarColumnaFila_llena_de_n(N2, T, S).

% --------------------------
% 2) Generador de tableros con números.
% --------------------------

% situarVariablesMatriz(+X, -Y) ← X es una matriz con casillas 'b' o 'n', Y se genera a partir de X sustituyendo las casillas blancas 
% por variables sin instanciar con la restricción de que sean diferentes en cada bloque de fila.
situarVariablesMatriz([], []) :- !.
situarVariablesMatriz([X|Xs], [Y|Ys]) :- restriccionesFila(X, Y, []), situarVariablesMatriz(Xs, Ys).

% restriccionesFila(+F1, -F2, -Ac) ← F1 es una fila de casillas 'b' o 'n', F2 se obtiene de F1 sustituyendo las 'b' 
% por variables sin instanciar comprendidas en el rango 1..9 y que sean distintas dentro de un mismo bloque de fila, 
% Ac es un acumulador de variables de bloque.
restriccionesFila([], [], Ac) :- !, all_distinct(Ac).
restriccionesFila([n|Xs], [n|Ys], Ac) :- !, all_distinct(Ac), restriccionesFila(Xs, Ys, []).
restriccionesFila([b|Xs], [X|Ys], Ac) :- !, X in 1..9, restriccionesFila(Xs, Ys, [X|Ac]).

% chequearRepetidos(+X) ← Aplica el predicado chequearRepetidos a cada fila de X.
chequearRepetidosMatriz([]):- !.
chequearRepetidosMatriz([X|Xs]) :- chequearRepetidos(X, []), chequearRepetidosMatriz(Xs).

% chequearRepetidos(+F, -Ac) ← F es una fila con n's o variables sin instanciar con ciertas restricciones, Ac es un acumulador de 
% las variables que se encuentran entre 2 casillas negras. Cuando se termina un bloque, se impone la restricción de que las variables
% sean distintas y se instancian con label.
chequearRepetidos([], Ac) :- !,all_distinct(Ac), labeling([random_variable(10), random_value(10)], Ac).
chequearRepetidos([X|Xs], Ac) :- atomic(X), X=n, !, all_distinct(Ac), label(Ac), chequearRepetidos(Xs, []).
chequearRepetidos([X|Xs], Ac) :- !, chequearRepetidos(Xs, [X|Ac]).

% --------------------------
% 3) Generador de tableros con números y casillas cabeza de bloque.
% --------------------------

% procSumaFila(+F1, -F2, ?Ac) ← F1 es una fila con números del 1 al 9 o 'n' tal que no hay repetidos en un mismo bloque horizontal de números, 
% F2 es F1 sustituyendo la 'n' a la izquierda de cada bloque por 'f(Suma)' siendo Suma la suma de los valores del bloque, Ac es un acumulador.
procSumaFila([], [], 0).
procSumaFila([n|Xs], [f(T)|Ys], 0) :- procSumaFila(Xs, Ys, T), not(T is 0), !.
procSumaFila([X|Xs], [X|Ys], N) :- integer(X), procSumaFila(Xs, Ys, R), N is X+R.
procSumaFila([n|Xs], [n|Ys], 0) :- procSumaFila(Xs, Ys, 0).

% procSumaColumna(+F1, -F2, ?Ac) ← F1 es una columna (este predicado se utiliza con la matriz traspuesta) que cumple las propiedades del Kakuro con las 'f(Suma)' ya situadas en las casillas cabeza de bloque de fila, F2 se obtiene de F1 añadiendo las casillas cabeza de bloque de: -columna ('c(Suma)') y -fila y columna ('p(SumaFila,SumaColumna)').
procSumaColumna([], [], 0).
procSumaColumna([n|Xs], [c(T)|Ys], 0) :-procSumaColumna(Xs, Ys, T), not(T is 0), !.
procSumaColumna([f(F)|Xs], [p(F,T)|Ys], 0) :- procSumaColumna(Xs, Ys, T), not(T is 0), !.
procSumaColumna([f(X)|Xs], [f(X)|Ys],0) :- procSumaColumna(Xs, Ys, 0), !.
procSumaColumna([X|Xs], [X|Ys], N) :- integer(X), procSumaColumna(Xs, Ys, R), N is X+R.
procSumaColumna([n|Xs], [n|Ys], 0) :- procSumaColumna(Xs, Ys, 0).

% procFilas(+X,-Y) ← La matriz Y se obtiene de X aplicando el predicado procSumaFila a cada una de sus filas.
procFilas([],[]) :- !.
procFilas([X|Xs],[Y|Ys]):- procSumaFila(X,Y,0), procFilas(Xs,Ys).

% procColumnas(+X,-Y) ← La matriz Y se obtiene de X aplicando el predicado procSumaColumnas a cada una de sus filas 
% (columnas, ya que se utilizará con la matriz trapuesta).
procColumnas([],[]) :- !.
procColumnas([X|Xs],[Y|Ys]):- procSumaColumna(X,Y,0), procColumnas(Xs,Ys).

proc(T, S) :- procFilas(T, S1), transpose(S1, S2), procColumnas(S2, S3), transpose(S3, S). 
 
% --------------------------
% 4) Generador final del tablero.
% --------------------------

generarTableroNumeros(M,N,Tablero):- generarTablero_n_or_b(M, N, T1), 
                                     transpose(T1, T2), 
                                     ajustarBloque(T2, T3), 
                                     situarVariablesMatriz(T3, T4), 
                                     transpose(T4, T5), 
                                     chequearRepetidosMatriz(T5), 
                                     proc(T4, Tablero).
