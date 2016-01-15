%%%-------------------------------------------------------------------
%%% PDAP: Práctica 1 de concurrencia
%%% @author Gorka Suárez García
%%% Created : 11. nov 2015 6:38 PM
%%%-------------------------------------------------------------------

-module(listen_server).
-author("Gorka Suárez García").
-export([]).
-export([start/0, start_and_register/0, loop/0,
    print/1, print/2, stop/0, stop/1]).

start() ->
    spawn(listen_server, loop, []).

loop() ->
    receive
        {ClientPID, stop} ->
            io:format("ECHO: ~w~n", [stop]),
            io:format("[ECHO] KILLED BY DEATH!!!~n"),
            ClientPID ! {self(), ok};
        {ClientPID, Victim} ->
            io:format("ECHO: ~p~n", [Victim]),
            ClientPID ! {self(), ok},
            loop()
    after 5000 ->
        io:format("[ECHO] But I still haven't found what I'm looking for...~n"),
        loop()
    end.

print(PID, Victim) ->
    PID ! {self(), Victim},
    receive
        {PID, ok} ->
            io:format("Done.~n");
        {PID, nil} ->
            io:format("Error!~n")
    after 5000 ->
        io:format("Time out!~n")
    end.

stop(PID) ->
    print(PID, stop).

start_and_register() ->
    register(listen_server_loop, start()).

print(Victim) ->
    print(whereis(listen_server_loop), Victim).

stop() ->
    print(whereis(listen_server_loop), stop).
