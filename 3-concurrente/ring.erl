%%%-------------------------------------------------------------------
%%% PDAP: Práctica 2 de concurrencia
%%% @author Gorka Suárez García
%%% Created : 18. nov 2015 5:48 PM
%%%-------------------------------------------------------------------

-module(ring).
-author("Gorka Suárez García").
-export([start/3, node_launch/2, node_loop/3]).

start(0, _, _) ->
    ok;
start(_, 0, _) ->
    ok;
start(M, N, Msg) ->
    % Primero, fijaremos el PID del maestro padre de los nodos:
    MasterPID = self(),
    % Segundo, lanzaremos todos los nodos para obtener sus PIDs para luego
    % crear una segunda lista con los PIDs desplazados de modo que podamos
    % crear una lista de tuplas con el nodo y su siguiente víctima:
    NodesPID1 = [spawn(?MODULE, node_launch, [MasterPID, M]) || _ <- lists:seq(1, N)],
    NodesPID2 = tl(NodesPID1) ++ [hd(NodesPID1)],
    % Tercero, se envía a cada nodo quién será su siguiente víctima,
    % indicando que es el maestro quien le manda dicha información:
    [PID ! {MasterPID, NextPID} || {PID, NextPID} <- lists:zip(NodesPID1, NodesPID2)],
    % Por último, el maestro seleccionará a uno de sus hijos para iniciar
    % el ciclo de envío en forma de anillo:
    hd(NodesPID1) ! {MasterPID, hd(NodesPID1), Msg}.

node_launch(MasterPID, Loops) ->
    % Al principio, hasta que empiece el ciclo estaremos esperando quién
    % será la víctima del nodo actual:
    receive
        {MasterPID, Victim} ->
            node_loop(MasterPID, Victim, Loops)
    end.

node_loop(MasterPID, Victim, Loops) ->
    % Obtenemos el PID del nodo actual:
    ThisPID = self(),
    Ammo = receive
        {MasterPID, ThisPID, Msg} ->
            % Cualquier nodo estará a la espera de que que su maestro
            % lo convoque para empezar el ciclo de envío:
            io:format("Davros called you to fight for the glory of Skaro!~n"),
            io:format("[~p] Dalek ~w received from ~w this message: ~p~n",
                [Loops, ThisPID, MasterPID, Msg]),
            Msg;
        {OtherPID, ThisPID, Msg} when is_pid(OtherPID) ->
            % El dalek ha recibido de otro dalek un mensaje:
            io:format("[~p] Dalek ~w received from ~w this message: ~p~n",
                [Loops, ThisPID, OtherPID, Msg]),
            Msg
    end,
    % El nodo enviará al siguiente en la cadena el mensaje recibido:
    Victim ! {ThisPID, Victim, Ammo},
    % Solo si quedan ciclos al nodo se seguirá vivo para esperar la
    % llegada de otro mensaje, si no se morirá:
    if Loops > 1 ->
        node_loop(MasterPID, Victim, Loops - 1);
    true ->
        finish
    end.
