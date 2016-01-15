%%%-------------------------------------------------------------------
%%% PDAP: Práctica 2 de concurrencia
%%% @author Gorka Suárez García
%%% Created : 18. nov 2015 5:25 PM
%%%-------------------------------------------------------------------

-module(sort).
-author("Gorka Suárez García").
-export([qsort/1, cqsort/1, cqsortp/2]).

qsort([]) -> [];
qsort([X]) -> [X];
qsort([X|XS]) -> qsort([Y || Y <- XS, Y < X]) ++ [X] ++ qsort([Y || Y <- XS, Y >= X]).

cqsort(Victim) ->
    PID = spawn(?MODULE, cqsortp, [self(), Victim]),
    receive {PID, Result} -> Result end.

cqsortp(PID, Victim) ->
    case Victim of
        [] ->
            PID ! {self(), []};
        [X] ->
            PID ! {self(), [X]};
        [X|XS] ->
            LPID = spawn_link(?MODULE, cqsortp, [self(), [Y || Y <- XS, Y < X]]),
            RPID = spawn_link(?MODULE, cqsortp, [self(), [Y || Y <- XS, Y >= X]]),
            LS = receive {LPID, RLS} -> RLS end,
            RS = receive {RPID, RRS} -> RRS end,
            PID ! {self(), LS ++ [X] ++ RS}
    end.
