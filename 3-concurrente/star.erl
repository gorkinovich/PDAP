%%%-------------------------------------------------------------------
%%% PDAP: Práctica 2 de concurrencia
%%% @author Gorka Suárez García
%%% Created : 18. nov 2015 6:37 PM
%%%-------------------------------------------------------------------

-module(star).
-author("Gorka Suárez García").
-export([start/3, master_launch/3, master_loop/3, node_loop/1]).

start(0, _, _) -> ok;
start(_, 0, _) -> ok;
start(M, N, Msg) -> spawn(?MODULE, master_launch, [M, N, Msg]).

master_launch(Loops, NumNodes, Msg) ->
    Server = self(),
    Clients = [spawn(?MODULE, node_loop, [Server]) || _ <- lists:seq(1, NumNodes)],
    master_loop(Clients, Loops, Msg).

master_loop(Clients, 0, _) ->
    % No hay más mensajes que enviar salvo la muerte y destrucción de los nodos:
    [PID ! {self(), exterminate} || PID <- Clients];
master_loop(Clients, Loops, Msg) ->
    % Enviar primero todos los mensajes a los nodos esclavos:
    io:format("[MASTER: ~p] Start broadcast number: ~p~n", [self(), Loops]),
    [PID ! {self(), Msg} || PID <- Clients],
    % Esperar a recibir la respuesta de todos los nodos:
    [receive {PID, obey_to_the_dalek} ->
        io:format("[MASTER: ~p] Received answer from: ~p~n", [self(), PID])
     end || PID <- Clients],
    % Invocar el siguiente ciclo de envio:
    master_loop(Clients, Loops - 1, Msg).

node_loop(Server) ->
    receive
        {Server, exterminate} ->
            skaro_will_prevail;
        {Server, Msg} ->
            io:format("[NODE: ~p] Received from ~p message: ~p~n", [self(), Server, Msg]),
            Server ! {self(), obey_to_the_dalek},
            node_loop(Server)
    end.
