%------------------Predicados predefinidos:------------------%

%fliplength(?Longitud, ?Lista)
fliplength(N, L) :- length(L, N).

%matriz(?Matriz, ?Filas, ?Columnas)
matriz(M, F, C) :- length(M, F), maplist(fliplength(C), M).

%dif1(+N1, ?N2)
dif1(N1, N2) :- N2 is N1 + 1.
dif1(N1, N2) :- N2 is N1 - 1.

%adyacente(+F1, +C1, ?F2, ?C2)
adyacente(F1,C1,F1,C2) :- dif1(C1,C2).
adyacente(F1,C1,F2,C1) :- dif1(F1,F2).
adyacente(F1,C1,F2,C2) :- dif1(C1,C2), dif1(F1,F2).

%enRango(+Matriz, +Fila, +Columna)
enRango([Fila|Filas], F, C) :- F > 0, C > 0, length([Fila|Filas], FMax), F =< FMax, length(Fila, CMax), C =< CMax.

%adyacenteEnRango(+Tablero, +F1, +C1, ?F2, ?C2)
adyacenteEnRango(T,F1,C1,F2,C2) :- adyacente(F1,C1,F2,C2), enRango(T,F2,C2).

%------------------Predicados a definir:------------------%

%contenido(+?Tablero, ?Fila, ?Columna, ?Contenido)
contenido(T, F, C, Cont) :- nth1(F, T, Fila), nth1(C, Fila, Elem), nonvar(Cont), Elem==Cont.
contenido(T, F, C, Cont) :- nth1(F, T, Fila), nth1(C, Fila, Elem), var(Cont), Elem=Cont.
contenido(T, F, C, Cont) :- nth1(F, T, Fila), nth1(C, Fila, Elem), nonvar(Cont), var(Elem), Elem=Cont.

%disponible(+Tablero, ?Fila, ?Columna)
disponible(T, F, C) :- contenido(T, F, C, Elem), var(Elem), 
                       forall(adyacenteEnRango(T, F, C, F2, C2), (contenido(T, F2, C2, Elemady), var(Elemady))).

%puedoColocar(+CantPiezas, ?Direccion, +Tablero, ?Fila, ?Columna)
puedoColocar(0, _, _, _, _).
puedoColocar(Cpiezas, horizontal, T, F, C) :- disponible(T, F, C), 
                                              RemainingPieces is Cpiezas-1,
                                              NextColumn is C+1,
                                              puedoColocar(RemainingPieces, horizontal, T, F, NextColumn).
puedoColocar(Cpiezas, vertical, T, F, C) :- disponible(T, F, C), 
                                            RemainingPieces is Cpiezas-1,
                                            NextRow is F+1,
                                            puedoColocar(RemainingPieces, vertical, T, NextRow, C).


% Esto esta logrado de una manera fea. Si el caso base (CantPiezas = 1) no tiene
% hardcodeada la direccion (vertical u horizontal) entonces se cumple 
% para una pieza con direccion horizontal o vertical. Esto inevitablemente genera
% repetidos. La manera en la cual salvamos esto, es teniendo dos casos bases.
% Uno para cuando se estaba poniendo un barco de CantPiezas >= 2, con caso 
% base CantPiezas = 2, y otro para cuando se estaba poniendo un barco de 
% una sola pieza (al cual le hardcodeamos la Dir vertical para que no genere repetidos).
ubicarBarco(1, vertical, T, F, C) :- disponible(T, F, C), contenido(T, F, C, o).
ubicarBarco(2, horizontal, T, F, C) :- contenido(T, F, C, o),
                                       NextColumn is C+1,
                                       contenido(T, F, NextColumn, o).
ubicarBarco(Cpiezas, horizontal, T, F, C) :- contenido(T, F, C, o),
                                             RemainingPieces is Cpiezas-1,
                                             NextColumn is C+1,
                                             ubicarBarco(RemainingPieces, horizontal, T, F, NextColumn).
ubicarBarco(2, vertical, T, F, C) :- contenido(T, F, C, o),
                                     NextRow is F+1,
                                     contenido(T, NextRow, C, o).
ubicarBarco(Cpiezas, vertical, T, F, C) :-  contenido(T, F, C, o), 
                                            RemainingPieces is Cpiezas-1,
                                            NextRow is F+1,
                                            ubicarBarco(RemainingPieces, vertical, T, NextRow, C).

%ubicarBarcos(+Barcos, +?Tablero)
ubicarBarcos([], _).
ubicarBarcos([Barco|Barcos], T) :- puedoColocar(Barco, Dir, T, F, C), ubicarBarco(Barco, Dir, T, F, C), ubicarBarcos(Barcos, T). 

%completarConAgua(+?Tablero)
% Utilizamos la funcion asignar porque por 
% alguna razon, cuando hacemos X=~ nos tira
% un syntax error.
asignarUnificando(X, Atomo) :- X=Atomo.
predicado(X) :- var(X), asignarUnificando(X, agua).
predicado(X) :- nonvar(X).
completarConAgua([]).
completarConAgua([Fila|Filas]) :- maplist(predicado, Fila), completarConAgua(Filas).

%golpear(+Tablero, +NumFila, +NumColumna, -NuevoTab)
asignarIgualando(Atomo, Atomo) :- X \= Atomo.
xxx(T, Fila, Columna) :- nth1(Fila, T, F), nth1(Columna, F, Elem), asignarIgualando(Elem, agua). 
golpear(T, Fila, Columna, Nuevotab) :- enRango(T, Fila, Columna), NuevoTab = T, xxx(NuevoTab, Fila, Columna).

%crearTablero(T, FAtacada, CAtacada, NuevoTab) :- matriz(T, F, C), forall(between(1, F, I), 
%(between(1, C, J),  contenido(T, I, J, X), contenido(NuevoTab, I, J, X))).

% No anda para todos los casos
% En particular este: Tablero = [[o, o], [_, _], [_, o]], completarConAgua(Tablero), crearTablero(Tablero, 1, 2, T2).
crearTablero([], FAtacada, CAtacada, []).
crearTablero([Fila|Filas], 1, 1, [FilaACopiar|Filas]) :- length(Fila, N),
                                                         length(FilaACopiar, N),
                                                         nth1(1, FilaACopiar, agua),
                                                         I is 2,
                                                         between(I, N, K), nth1(K, Fila, Elem2), 
                                                         nth1(K, FilaACopiar, Elem2).

crearTablero([Fila|Filas], 1, CAtacada, [FilaACopiar|Filas]) :- CAtacada > 1,
                                                                C is CAtacada-1,
                                                                between(1, C, J), nth1(J, Fila, Elem), 
                                                                nth1(J, FilaACopiar, Elem), 
                                                                nth1(CAtacada, FilaACopiar, agua), 
                                                                I is CAtacada+1,
                                                                length(Fila, N),
                                                                between(I, N, K), nth1(K, Fila, Elem2), 
                                                                nth1(K, FilaACopiar, Elem2).
crearTablero([Fila|Filas], FAtacada, CAtacada, [Fila|FilasACopiar]) :- FAtacada > 1, F is FAtacada-1, crearTablero(Filas, F, CAtacada, FilasACopiar).

%crearTablero(T, FAtacada, CAtacada, NuevoTab) :- matriz(T, F, C), forall(between(1, F, I), 
%(between(1, C, J), I \= FAtacada, J \= CAtacada, contenido(T, I, J, X), contenido(NuevoTab, I, J, X))).
%contenido(T, F, C, Cont) :- nth1(F, T, Fila), nth1(C, Fila, Elem), nonvar(Cont), Elem==Cont.

% Completar instanciación soportada y justificar.
%atacar(Tablero, Fila, Columna, Resultado, NuevoTab)

%------------------Tests:------------------%

%test(1) :- matriz(M,2,3), adyacenteEnRango(M,2,2,2,3).
%test(2) :- matriz(M,2,3), setof((F,C), adyacenteEnRango(M,1,1,F,C), [ (1, 2), (2, 1), (2, 2)]).
%test(3) :- T = matriz([[_, _, 'o'],[_, _, _]],_,_), contenido(T, 1, 3, 'o').
% Tests contenido
test(1) :- contenido([[_, _, o],[_, _, _]], 1, 3, o).
test(2) :- matriz(M,2,3), contenido(M, 1, 3, o), contenido(M, 1, 3, o).
test(3) :- matriz(M,2,3), contenido(M, 1, 3, o), not(contenido(M, 1, 3, a)).
test(4) :- matriz(M,2,3), contenido(M, 1, 3, o), contenido(M, 1, 3, X), X==o.
test(5) :- contenido([[_, _, l],[_, _, _]], 1, 3, C), C==l.
test(6) :- contenido([[_, _, l],[_, _, _]], 1, 3, C), nonvar(C).
test(7) :- contenido([[_, _, _],[_, _, _]], 1, 3, C), var(C).

% Tests disponible
test(8) :- not(disponible([[_, _, o],[_, _, _]], 2, 2)).
test(9) :- disponible([[_,_, _],[_,_, _]], 2, 2).
test(10) :- disponible([[_,_, _], [_,_, _], [_,_, _]], 2, 2).
test(11) :- disponible([[_,_, _], [_,_, _], [o,o, o]], 1, 1).
test(12) :- not(disponible([[o,_, _], [_,_, _], [o,o, o]], 1, 1)).
test(13) :- disponible([[_,_, _], [_,_, o], [o,o, o]], 1, 1).

% Tests puedoColocar
%test(14) :- matriz(M, 2, 4), puedoColocar(3, _, _, _, _).
test(14) :- matriz(M, 2, 2), puedoColocar(1, horizontal, M, 1, 1).
test(15) :- matriz(M, 2, 2), not( puedoColocar(3, horizontal, M, 1, 1) ).
test(16) :- M= not( puedoColocar(3, vertical, [[_,_,_], [_,o,_], [_,_,_]], 0, 0)).

% Tests ubicarBarco
%ubicarBarco(Cpiezas, horizontal, T, F, C)
test(17) :- matriz(M,3,3), ubicarBarco(2, horizontal, M, 1, 1), contenido(M, 1, 1, Y), Y==o.
test(18) :- matriz(M,3,3), contenido(M, 1, 2, o), ubicarBarco(2, horizontal, M, 1, 1), contenido(M, 1, 1, X), contenido(M, 1, 2, Y), X==o, Y==o.
test(19) :- matriz(M,1,1), not( ubicarBarco(2, horizontal, M, 1, 1) ).

% Queda escrito como correrlo pero da varias soluciones -> correr a mano.
% Tests ubicarBarcos
% matriz(M,3,2), ubicarBarcos([2,1],M).

% Tests completarConAgua
test(20) :- matriz(M,1,1), completarConAgua(M), contenido(M, 1, 1, X), X==agua.
test(21) :- matriz(M,1,2), contenido(M, 1, 1, o), completarConAgua(M), contenido(M, 1, 1, X), contenido(M, 1, 2, Y), X==o, Y==agua.

% Tests golpear
test(22) :- matriz(M,1,1), contenido(M, 1, 1, o), golpear(M, 1, 1, N), contenido(N, 1, 1, X), X==agua.
test(23) :- matriz(M,1,1), completarConAgua(M), golpear(M, 1, 1, N), M==N.

% Tests atacar
test(24) :- matriz(M,1,1), contenido(M, 1, 1, o), atacar(M, 1, 1, E, N), E==hundido, contenido(N, 1, 1, X), X==agua.
test(25) :- matriz(M,1,2), ubicarBarco(2, horizontal, M, 1, 1), atacar(M, 1, 1, E, N), E==tocado, contenido(N, 1, 1, X), X==agua.
test(26) :- matriz(M,1,1), completarConAgua(M), atacar(M, 1, 1, E, N), E==agua, N==M.
test(27) :- matriz(M,1,1), not( atacar(M, 1, 1, E, N) ).
test(28) :- atacar([[o,agua,agua], [agua,o,agua], [agua,agua,o]], 1, 1, E, N), E==hundido, contenido(N, 1, 1, X), X==agua.

tests :- forall(between(1,28,N), test(N)).
