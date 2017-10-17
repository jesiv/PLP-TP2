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

%adyacenteOrtogonal(+F1, +C1, ?F2, ?C2)
% -------------- Consultar si se puede hacer esto --------------
adyacenteOrtogonal(F1,C1,F1,C2) :- dif1(C1,C2).
adyacenteOrtogonal(F1,C1,F2,C1) :- dif1(F1,F2).

%enRango(+Matriz, +Fila, +Columna)
enRango([Fila|Filas], F, C) :- F > 0, C > 0, length([Fila|Filas], FMax), F =< FMax, length(Fila, CMax), C =< CMax.

%adyacenteEnRango(+Tablero, +F1, +C1, ?F2, ?C2)
adyacenteEnRango(T,F1,C1,F2,C2) :- adyacente(F1,C1,F2,C2), enRango(T,F2,C2).

%adyacenteEnRango(+Tablero, +F1, +C1, ?F2, ?C2)
adyacenteOrtogonalEnRango(T,F1,C1,F2,C2) :- adyacenteOrtogonal(F1,C1,F2,C2), enRango(T,F2,C2).

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
predicado(X) :- var(X), X=agua.
predicado(X) :- nonvar(X).

completarConAgua([]).
completarConAgua([Fila|Filas]) :- maplist(predicado, Fila), completarConAgua(Filas).


%golpear(+Tablero,+NumFila,+NumColumna,-NuevoTab)
crearTablero([], FAtacada, CAtacada, []).
crearTablero([Fila|Filas], 1, CAtacada, [FilaACopiar|Filas]) :- copiarLista(CAtacada, Fila, FilaACopiar).
crearTablero([Fila|Filas], FAtacada, CAtacada, [Fila|FilasACopiar]) :- FAtacada > 1, F is FAtacada-1, crearTablero(Filas, F, CAtacada, FilasACopiar).

%copiarLista(+Posicion, +Lista, -Lista)
copiarLista(0, XS, XS).
copiarLista(1, [X|XS], [Y|YS]) :- Y = agua, copiarLista(0, XS, YS).
copiarLista(Posicion, [X|XS], [X|YS]) :- Posicion > 1, NextPosicion is Posicion - 1, copiarLista(NextPosicion, XS, YS).

golpear(T, FAtacada, CAtacada, NuevoTab) :- enRango(T, FAtacada, CAtacada), crearTablero(T, FAtacada, CAtacada, NuevoTab).


%atacar(+Tablero,+NumFila,+NumColumna,-Resultado,-NuevoTab)
rodeadoPorAgua(T, F, C) :- contenido(T, F, C, Elem), Elem == agua,
                           forall(adyacenteOrtogonalEnRango(T, F, C, F2, C2), (contenido(T, F2, C2, Elemady), Elemady == agua)).

encontrarDiferencia(T, F, C, Resultado, T2) :- contenido(T, F, C, X), X == agua, Resultado = agua.
encontrarDiferencia(T, F, C, Resultado, T2) :- rodeadoPorAgua(T2, F, C), contenido(T, F, C, X), X == o, Resultado = hundido.
encontrarDiferencia(T, F, C, Resultado, T2) :- not(rodeadoPorAgua(T2, F, C)), 
                                               contenido(T, F, C, X), X == o, Resultado = tocado.


atacar(Tablero, Fila, Columna, Resultado, NuevoTab) :- golpear(Tablero, Fila, Columna, NuevoTab), 
                                                       encontrarDiferencia(Tablero, Fila, Columna, Resultado, NuevoTab).

% Completar instanciación soportada y justificar.
%atacar(+Tablero, +Fila, +Columna, Resultado, -NuevoTab)
% - NuevoTab: Si no está instanciado devuelve el tablero resultado, luego de realizar
% el ataque en dicha Fila y Columna. Si viene instanciado, el predicado será True
% sii se instancia el resultado de atacar a 'Tablero' en dicha Fila y Columna.
% - Fila y Columna: Deben estar instanciadas ya que al atacar una posición, se utilizán
% operaciones aritméticas sobre las mismas, que requieren que esten instanciadas.
% - Tablero: El tablero debe estar instanciado. Probamos instanciar el tablero resultado
% a ver si deducía el tablero inicial, pero se traba.
% - Resultado: Puede venir instanciado, pero en dicho caso, deberá coincidir con 
% el resultado de la acción, para que el predicado sea verdadero.

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

% En este caso fallaba -> miraba si habia en diagonal una o y daba tocado.
% Agregue adyacenteOrtogonal para salvar el caso pero nose si es valido.
test(28) :- atacar([[o,agua,agua], [agua,o,agua], [agua,agua,o]], 1, 1, E, N), E==hundido, contenido(N, 1, 1, X), X==agua.

tests :- forall(between(1,28,N), test(N)).
