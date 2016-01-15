%%%-------------------------------------------------------------------
%%% PDAP: Práctica 3
%%% @author Gorka Suárez García
%%% Created : 28. oct 2015 5:30 PM
%%%-------------------------------------------------------------------

-author("Gorka Suárez García").

%--------------------------------------------------
% Apartado 1
%--------------------------------------------------

-record(carta, {valor = as, palo = corazones}).
-record(mano, {cartas = []}).

%--------------------------------------------------
% Apartado 2
%--------------------------------------------------

-record(persona, {nombre, apellidos, dni, edad, nacimiento,
    calle, ciudad, cp, pais}).
