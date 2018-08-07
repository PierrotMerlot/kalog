:- module(lab1, [
                    % 1) Predicados sobre listas
                    largo/2,
                    largoAc/3,
                    ultimo/2,
                    penultimo/2,
                    diferentes/1,
                    diferentes/2,
                    simplificada/2,
                    empaquetada/2,
                    comprimida/2,
                    descomprimida/2,

                    % 2) Predicados sobre matrices
                    matrizFija/4,
                    valor_celda/4,
                    nuevo_valor_celda/5,
                    cartesiano/3,
                    composicion/3,

                    % 3) Cuadrados latinos
                    latino/3,

                    % 4) Cuadrados greco-latinos
                    cuadrado_GL/4
                  ]).

% --------------------------
% 1) Predicados sobre listas
% --------------------------

% ----------
% Auxiliares
% ----------
% not_member(+X,+L) ← X no pertenece a la lista L.
not_member(_,[]).
not_member(X,[H|T]) :- X\=H, not_member(X,T).
% -.-

% largo(+L,?N) ← N es el largo de la lista L.
largo([],0).
largo([_|T],N) :- largo(T,M), N is M+1.

% largoAc(+L,+Ac,?N) ← N es el largo de la lista L, el predicado es tail-recursive.
largoAc([_|T],Ac,L) :- Ac1 is Ac+1, largoAc(T,Ac1,L).
largoAc([],L,L).

% ultimo(?L,?U) ← el último elemento de la lista L unifica con U.
ultimo([X],X).
ultimo([_|Zs],X) :- ultimo(Zs,X).

% penultimo(?L,?U) ← el penúltimo elemento de la lista L unifica con U.
penultimo([X,_],X).
penultimo([_|Zs],X) :- penultimo(Zs,X).

% diferentes(+L) ← todos los elementos de la lista L son distintos entre sí.
diferentes([]).
diferentes([H|T]) :- not_member(H,T), diferentes(T).

% diferentes (+L1,+L2) ← L1 y L2 tiene el mismo largo y todos los elementos correspondientes son distintos.
diferentes([],[]).
diferentes([X|Xs], [Y|Ys]) :- X\=Y, diferentes(Xs,Ys).

% simplificada(+L1,?L2) ← L2 contiene los mismo elementos que L1 y en el mismo orden, pero con una sola instancia de elementos consecutivos repetidos en L1.
simplificada([],[]).
simplificada([X],[X]).
simplificada([X,X|Xs], Ys) :- simplificada([X|Xs], Ys).
simplificada([X,Y|Xs], [X|Zs]) :- X\=Y, simplificada([Y|Xs], Zs).

% empaquetada(+L1,?L2) ← L2 contiene listas con los elementos consecutivos repetidos (ocurren 1 o más veces consecutivas) de L1, en el mismo orden.
empaquetada([],[]).
empaquetada([X],[[X]]).
empaquetada([X,X|T], [[X|Xs]|Zs]) :- empaquetada([X|T], [Xs|Zs]).
empaquetada([X,Y|T], [[X]|Zs]) :- X\=Y, empaquetada([Y|T], Zs).

% comprimida(+L1,?L2) ← L2 contiene pares formados por los elementos de L1, en el mismo orden y la cantidad de veces consecutivas que ocurren.
comprimida([], []).
comprimida([X], [p(X, 1)]).
comprimida([X,X|Xs], [p(X, N)|Zs]) :- comprimida([X|Xs], [p(X,M)|Zs]), N is M+1.
comprimida([X,Y|Xs], [p(X, 1)|Zs]) :- X \= Y, comprimida([Y|Xs], Zs).

% descomprimida(?L1,+L2) ← L2 contiene pares formados por los elementos de L1, en el mismo orden y la cantidad de veces consecutivas que ocurren.
descomprimida([],[]).
descomprimida([X|T], [p(X, 1)|Ps]) :- descomprimida(T, Ps).
descomprimida([X|T], [p(X, N)|Ps]) :- M is N-1, M>0, descomprimida(T, [p(X, M)|Ps]).

% ----------------------------
% 2) Predicados sobre matrices
% ----------------------------

% ----------
% Auxiliares
% ----------
% fila(+N,+E,?F) ← F es una fila de largo N cuyos elementos son todos iguales a E.
fila(0,_,[]).
fila(N,E,[E|Es]) :- N>0, P is N-1, fila(P,E,Es).

% buscar_en_fila(+J,+F,?E) ← La fila F tiene al elemento E en su posición J.
buscar_en_fila(1, [E|_], E).
buscar_en_fila(J, [_|Xs], E) :- K is J-1, buscar_en_fila(K, Xs, E).

% nueva_fila(+J,+F1,+E,?F2) ← F2 se obtiene de F1 cambiando el elemento J-ésimo por E.
nueva_fila(1, [_|Xs], E, [E|Xs]).
nueva_fila(J, [X|Xs], E, [X|Ys]):- K is J-1, nueva_fila(K, Xs, E, Ys).

% suma_filas(+F1,+F2,?F3) ← F3 se obtiene sumando entrada a entrada las filas F1 y F2.
suma_filas([],[],[]).
suma_filas([X|Xs],[Y|Ys],[T|Ts]) :- T = X + Y, suma_filas(Xs,Ys,Ts).

% cartesiano_filas(E+,+V1,?V2) ← Si V1 = [x1,x2,...,xn] entonces V2 = [(E,x1),(E,x2),...,(E,xn)].
cartesianos_filas(_,[],[]).
cartesianos_filas(E, [X|Xs], [(E,X)|Ps]) :- cartesianos_filas(E,Xs,Ps).
% -.-

% matrizFija(?M,?N,+E,?A) ← A  es una matriz de M filas y N columnas. Cada celda debe tener el valor E. La matriz se representa mediante una lista de M filas donde cada fila es una lista de N celdas.
matrizFija(0,0,_,[]).
matrizFija(M,N,E,[F|Fs]) :- length([F|Fs],M), length(F, N), matriz(M,N,E,[F|Fs]).
matriz(1,N,E,[F]) :- N>0, fila(N,E,F).
matriz(M,N,E,[F|Fs]) :- fila(N,E,F), M>1, P is M-1, matriz(P,N,E,Fs). 

% valor_celda(+I,+J,+A,?E) ← E es el contenido de la celda (I,J) de la matriz A.
valor_celda(1, J, [F|_], E) :- buscar_en_fila(J, F, E).
valor_celda(I, J, [_|Fs], E) :- K is I-1, valor_celda(K, J, Fs, E).

% nuevo_valor_celda(+I,+J,+A1,+E,?A2) ← A2 es una matriz que contiene el valor E en la celda (I,J) y en el resto de las celdas contiene los mismos valores que A1.
nuevo_valor_celda(1,J, [F1|Fs], E, [F2|Fs]) :- nueva_fila(J, F1, E, F2).
nuevo_valor_celda(I,J, [F1|Xs], E, [F1|Ys]) :- K is I-1, nuevo_valor_celda(K,J, Xs, E, Ys).

% cartesiano(+V1,+V2,?M) ← V1 y V2 son vectores de igual dimensión N, M es una matriz NxN de pares (V1i,V2j) como elemento fila i y columna j de la matriz, siendo V1i el i-ésimo elemento de V1 y V2j el j-ésimo elemento de V2.
cartesiano(V1,V2,M):- length(V1, N), length(V2, N), cartesiano_(V1,V2,M).
cartesiano_([],_,[]).
cartesiano_([X|Xs],V2,[F|Fs]):- cartesianos_filas(X,V2,F), cartesiano_(Xs,V2,Fs).

% composicion(+M1,+M2,?M) ← M es la composición elemento a elemento con el el operador '+' de las matrices de igual dimensión M1 y M2.
composicion([],[],[]).
composicion([X|Xs], [Y|Ys], [T|Ts]):- suma_filas(X,Y,T), composicion(Xs,Ys,Ts).

% --------------------
% 3) Cuadrados latinos
% --------------------

% ----------
% Auxiliares
% ----------
% permutacion(+L1,?L2) ← L2 es una permutacion de L1.
permutacion([], []).
permutacion(L1, [H|T]) :- quitar(H, L1, Resto), permutacion(Resto, T).

% quitar(+E,+L1,L2?) ← L2 se obtiene de L1 eliminando todas las ocurrencias del elemento E.
quitar(E, [E|Xs], Xs).
quitar(E, [X|Xs], [X|Ys]) :- quitar(E, Xs, Ys).

% trasponer_columna(+A,?L,?B) ← Recoge en formato lista la primera columna de la matriz A (en el argumento L) mientras que B es la matriz resultante de eliminar dicha columna.
trasponer_columna([[X]], [X], []). % matriz fila de un solo elemento
trasponer_columna([[X,X1|Xs]], [X], [[X1|Xs]]). % matriz fila de más de un elemento
trasponer_columna([[X]|As], [X|Ys], Bs) :- trasponer_columna(As, Ys, Bs). % CASO fila de un elemento
trasponer_columna([[X,X1|Xs]|As], [X|Ys], [[X1|Xs]|Bs]) :- trasponer_columna(As, Ys, Bs). % CASO fila de más de un elemento
% -.-

% latino(+N,+E,?Lat) ← Lat es un cuadrado latino de orden N sobre el conjunto de elementos E.
latino(N, E, C):- latino_filas(N,E,C), latino_columnas(E,C).

% todas las filas son permutaciones de E.
latino_filas(0,_,[]).
latino_filas(N, E, [F|Fs]):- permutacion(E,F), M is N-1, N>0, latino_filas(M, E, Fs).

% todas las columnas son permutaciones de E.
latino_columnas(_,[]).
latino_columnas(E, C) :- trasponer_columna(C, T, C1), permutacion(E, T), latino_columnas(E, C1).

% --------------------------
% 4) Cuadrados greco-latinos
% --------------------------

% ----------
% Auxiliares
% ----------
% no_repetidos(+A) ← La matriz A no tiene elementos repetidos.
no_repetidos([]).
no_repetidos([[X|Xs]|Fs]) :- not_member(X,Xs), no_pertenece(X,Fs), no_repetidos([Xs|Fs]).
no_repetidos([[]|Fs]) :- no_repetidos(Fs).

% no_pertenece(+E,+A) ← El elemento E no aparece en la matriz A.
no_pertenece(_, []).
no_pertenece(E, [F|Fs]) :- not_member(E, F), no_pertenece(E, Fs).
% -.-

% cuadrado_GL(+N,+L1,+L2,?C) ← C es un cuadrado grecolatino de orden N, siendo L1 y L2 los elementos de S y T respectivamente.
cuadrado_GL(N,L1,L2,C) :- latino(N, L1, M1), latino(N, L2, M2), composicion(M1, M2, C), no_repetidos(C).
