%%%-------------------------------------------------------------------
%%% PDAP: Práctica 1 de concurrencia
%%% @author Gorka Suárez García
%%% Created : 11. nov 2015 5:51 PM
%%%-------------------------------------------------------------------

-module(echo_server).
-author("Gorka Suárez García").
-export([start/0, start_and_register/0, loop/0,
         print/1, print/2, stop/0, stop/1]).

start() ->
    spawn(echo_server, loop, []).

loop() ->
    receive
        {ClientPID, stop} ->
            io:format("ECHO: ~w~n", [stop]),
            ClientPID ! {self(), ok};
        {ClientPID, Victim} when is_atom(Victim) ->
            io:format("ECHO: ~w~n", [Victim]),
            ClientPID ! {self(), ok},
            loop();
        {ClientPID, _} ->
            ClientPID ! {self(), nil},
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
    register(echo_server_loop, start()).

print(Victim) ->
    print(whereis(echo_server_loop), Victim).

stop() ->
    print(whereis(echo_server_loop), stop).
