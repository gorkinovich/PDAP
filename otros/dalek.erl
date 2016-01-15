%%%-------------------------------------------------------------------
%%% PDAP: Práctica 2 de concurrencia
%%% @author Gorka Suárez García
%%% Created : 18. nov 2015 5:48 PM
%%%-------------------------------------------------------------------

-module(dalek).
-author("Gorka Suárez García").
-export([start/3, dalek_start/2, dalek_fight/3]).

start(0, _, _) ->
    ok;
start(_, 0, _) ->
    ok;
start(M, N, Msg) ->
    % Primero, fijaremos el PID del maestro Davros padre de los daleks:
    Davros = self(),
    % Segundo, lanzaremos todos los nodoss para obtener sus PIDs para luego
    % crear una segunda lista con los PIDs desplazados de modo que podamos
    % crear una lista de tuplas con el nodo y su siguiente víctima:
    Daleks1 = [spawn(?MODULE, dalek_start, [Davros, M]) || _ <- lists:seq(1, N)],
    Daleks2 = tl(Daleks1) ++ [hd(Daleks1)],
    SkaroDaleks = lists:zip(Daleks1, Daleks2),
    % Tercero, se envía a cada nodo quién será su siguiente víctima,
    % indicando que es el glorioso Davros quien le manda dicha información:
    [Dalek ! {Davros, NextDalek} || {Dalek, NextDalek} <- SkaroDaleks],
    % Por último, Davros seleccionará a uno de sus hijos para iniciar
    % el ciclo de envío en forma de anillo:
    hd(Daleks1) ! {Davros, hd(Daleks1), Msg}.

dalek_start(Davros, Shots) ->
    % Al principio, hasta que empiece la lucha estaremos esperando quién
    % será la víctima del dalek actual:
    receive
        {Davros, Victim} ->
            dalek_fight(Davros, Victim, Shots)
    end.

dalek_fight(Davros, Victim, Shots) ->
    % Obtenemos el PID del dalek actual:
    ThisDalek = self(),
    Ammo = receive
               {Davros, ThisDalek, Msg} ->
                   % Cualquier dalek estará a la espera de que que su padre Davros
                   % lo convoque para empezar la lucha para la gloria de Skaro:
                   io:format("Davros called you to fight for the glory of Skaro!~n"),
                   io:format("[~p] Dalek ~w received: ~p~n", [Shots, ThisDalek, Msg]),
                   Msg;
               {OtherDalek, ThisDalek, Msg} when is_pid(OtherDalek) ->
                   % El dalek ha recibido de otro dalek un mensaje:
                   io:format("[~p] Dalek ~w received from ~w this message: ~p~n",
                       [Shots, ThisDalek, OtherDalek, Msg]),
                   Msg
           end,
    % El dalek enviará al siguiente en la cadena el mensaje recibido:
    Victim ! {ThisDalek, Victim, Ammo},
    % Solo si queda munición al dalek se seguirá vivo para esperar la
    % llegada de otro mensaje dentro del ciclo, si no se auto exterminará:
    if Shots > 1 ->
        dalek_fight(Davros, Victim, Shots - 1);
        true ->
            auto_exterminate
    end.
