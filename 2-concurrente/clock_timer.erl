%%%-------------------------------------------------------------------
%%% PDAP: Práctica 1 de concurrencia
%%% @author Gorka Suárez García
%%% Created : 11. nov 2015 6:15 PM
%%%-------------------------------------------------------------------

-module(clock_timer).
-author("Gorka Suárez García").
-export([start/0, loop/0, stop/0]).

start() ->
    register(clock_timer_loop, spawn(clock_timer, loop, [])).

loop() ->
    receive
    after 1000 ->
        io:format("Time: ~w~n", [time()])
    end,
    loop().

stop() ->
    exit(whereis(clock_timer_loop), kill).
