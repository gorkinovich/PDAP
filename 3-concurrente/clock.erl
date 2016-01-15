%%%-------------------------------------------------------------------
%%% PDAP: Práctica 2 de concurrencia
%%% @author Gorka Suárez García
%%% Created : 18. nov 2015 5:15 PM
%%%-------------------------------------------------------------------

-module(clock).
-author("Gorka Suárez García").
-export([start/2, stop/0, tick/2]).

start(Time, Func) ->
    case whereis(clock) of
        undefined ->
            launch(Time, Func);
        PID ->
            unregister(clock),
            PID ! stop,
            launch(Time, Func)
    end.

stop() ->
    clock ! stop.

launch(Time, Func) ->
    register(clock, spawn(?MODULE, tick, [Time, Func])).

tick(Time, Func) ->
    receive
        stop ->
            void
    after Time ->
        Func(),
        tick(Time, Func)
    end.
