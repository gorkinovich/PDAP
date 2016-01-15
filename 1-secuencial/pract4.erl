%%%-------------------------------------------------------------------
%%% PDAP: Práctica 3
%%% @author Gorka Suárez García
%%% Created : 28. oct 2015 5:30 PM
%%%-------------------------------------------------------------------

-module(pract4).
-author("Gorka Suárez García").
-export([validarCarta/1, validarMano/1, pares/2, parejas/1,
         persona/9, edad/1, vecinos/2, habitantes/2,
         incluye/2, mapSafe/2]).
-include("pract4.hrl").

%--------------------------------------------------
% Apartado 1.1
%--------------------------------------------------

validarCarta(#carta{valor=V, palo=P}) ->
    validarValorCarta(V) andalso validarPaloCarta(P);
validarCarta(_) ->
    false.

validarValorCarta(X) ->
    Victims = [as, 2, 3, 4, 5, 6, 7, 8, 9, 10, j, q, k],
    length(lists:filter(fun(Y) -> X =:= Y end, Victims)) > 0.

validarPaloCarta(X) ->
    Victims = [picas, rombos, corazones, treboles],
    length(lists:filter(fun(Y) -> X =:= Y end, Victims)) > 0.

validarMano(#mano{cartas=[]}) ->
    true;
validarMano(#mano{cartas=CS}) ->
    length([C || C <- CS, not validarCarta(C)]) =< 0;
validarMano(_) ->
    false.

% rr(“pract4.hrl”).
% Mano = #mano{cartas=[#carta{valor=2,palo=corazones},
% #carta{valor=3,palo=rombos},#carta{valor=as,palo=treboles},
% #carta{valor=2,palo=rombos},#carta{valor=3,palo=picas}]}.

%--------------------------------------------------
% Apartado 1.2
%--------------------------------------------------

pares(A, B) ->
    L = lists:seq(A, B),
    [{I, J} || I <- L, J <- lists:seq(I + 1, B), I =/= J].

parejas(#mano{cartas=[]}) ->
    [];
parejas(#mano{cartas=CS}) ->
    PS = pares(1, length(CS)),
    FV = fun(N) -> (lists:nth(N,CS))#carta.valor end,
    [X || {X, Y} <- [{FV(I), FV(J)} || {I, J} <- PS], X =:= Y].

%--------------------------------------------------
% Apartado 2
%--------------------------------------------------

persona(Nombre, Apellidos, DNI, Edad, Nacimiento,
    Calle, Ciudad, CP, Pais) ->
    #persona{nombre = Nombre, apellidos = Apellidos, dni = DNI,
        edad = Edad, nacimiento = Nacimiento, calle = Calle,
        ciudad = Ciudad, cp = CP, pais = Pais}.

edad(#persona{edad=E}) ->
    E.

% Persona1=#persona{edad=25, nombre="bertoldo", apellidos="cacaseno", dni=1,
% calle="C/Jazmin", ciudad="Madrid", nacimiento="2000", cp=28765, pais="España"}.
% pract4:edad(Persona1).

vecinos(#persona{ciudad=C1}, #persona{ciudad=C2}) -> C1 =:= C2;
vecinos(_, _) -> false.

% Persona2=#persona{edad=28, nombre="herminia", apellidos="Filón", dni=2,
% calle="C/Jazmin", ciudad="Madrid", nacimiento="2000", cp=28765, pais="España"}.
% pract4:vecinos(Persona1,Persona2).
% true
% pract4:vecinos(Persona1,Persona2#persona{ciudad="Lugo"}).
% false

habitantes(PS, C) ->
    [P || P <- PS, P#persona.ciudad =:= C].

% pract4:habitantes([Persona1,Persona1#persona{ciudad="Lugo"},Persona2],"Madrid").

incluye(P, PS) ->
    N = length([Q || Q <- PS, P#persona.dni =:= Q#persona.dni]),
    if N =< 0 -> [P|PS];
        N >= 0 -> PS
    end.

% pract4:incluye(Persona1,[Persona1,Persona2]).
% pract4:incluye(Persona1,[Persona2]).

%--------------------------------------------------
% Apartado 3
%--------------------------------------------------

mapSafe(_, []) ->
    [];
mapSafe(F, LS) ->
    [try F(L) catch _:_ -> error end || L <- LS].

% pract4:mapSafe(fun(X)->1/X end,[1,2,3,4,0,5]).
