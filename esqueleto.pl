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


%ubicarbarco anda cuando la dir está instanciada
ubicarBarco(0, _, _, _, _).
ubicarBarco(Cpiezas, horizontal, T, F, C) :- contenido(T, F, C, o),
                                             RemainingPieces is Cpiezas-1,
                                             NextColumn is C+1,
                                             ubicarBarco(RemainingPieces, horizontal, T, F, NextColumn).
ubicarBarco(Cpiezas, vertical, T, F, C) :-  contenido(T, F, C, o), 
                                            RemainingPieces is Cpiezas-1,
                                            NextRow is F+1,
                                            ubicarBarco(RemainingPieces, vertical, T, NextRow, C).

%ubicarBarcos(+Barcos, +?Tablero)
ubicarBarcos([], _).
ubicarBarcos([Barco|Barcos], T) :- puedoColocar(Barco, Dir, T, F, C), ubicarBarco(Barco, Dir, T, F, C), ubicarBarcos(Barcos, T). 

%completarConAgua(+?Tablero)

%golpear(+Tablero, +NumFila, +NumColumna, -NuevoTab)

% Completar instanciación soportada y justificar.
%atacar(Tablero, Fila, Columna, Resultado, NuevoTab)

%------------------Tests:------------------%

%test(1) :- matriz(M,2,3), adyacenteEnRango(M,2,2,2,3).
%test(2) :- matriz(M,2,3), setof((F,C), adyacenteEnRango(M,1,1,F,C), [ (1, 2), (2, 1), (2, 2)]).
%test(3) :- T = matriz([[_, _, 'o'],[_, _, _]],_,_), contenido(T, 1, 3, 'o').
% tests contenido
test(1) :- contenido([[_, _, o],[_, _, _]], 1, 3, o).
test(2) :- matriz(M,2,3), contenido(M, 1, 3, o), contenido(M, 1, 3, o).
test(3) :- matriz(M,2,3), contenido(M, 1, 3, o), not(contenido(M, 1, 3, a)).
test(4) :- matriz(M,2,3), contenido(M, 1, 3, o), contenido(M, 1, 3, X), X==o.
test(5) :- contenido([[_, _, l],[_, _, _]], 1, 3, C), C==l.
test(6) :- contenido([[_, _, l],[_, _, _]], 1, 3, C), nonvar(C).
test(7) :- contenido([[_, _, _],[_, _, _]], 1, 3, C), var(C).

% tests disponible
test(8) :- not(disponible([[_, _, o],[_, _, _]], 2, 2)).
test(9) :- disponible([[_,_, _],[_,_, _]], 2, 2).
test(10) :- disponible([[_,_, _], [_,_, _], [_,_, _]], 2, 2).
test(11) :- disponible([[_,_, _], [_,_, _], [o,o, o]], 1, 1).
test(12) :- not(disponible([[o,_, _], [_,_, _], [o,o, o]], 1, 1)).
test(13) :- disponible([[_,_, _], [_,_, o], [o,o, o]], 1, 1).

% tests puedoColocar
test(14) :- matriz(M,2,4), puedoColocar(3,Dir,M,F,C).

tests :- forall(between(1,14,N), test(N)). % Cambiar el 2 por la cantidad de tests que tengan.
