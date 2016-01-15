%%%-------------------------------------------------------------------
%%% PDAP: Práctica 2 de concurrencia
%%% @author Gorka Suárez García
%%% Created : 26. nov 2015 2:11 PM
%%%-------------------------------------------------------------------

-module(sar).
-author("Gorka Suárez García").
-export([test/1, start/2]).

% sar:start(spock, fun () -> sar:test(30) end).

test(N) when N =< 0 -> io:format("~w Finished!~n", [self()]);
test(N) -> receive after 1000 -> ok end, test(N - 1).

start(Name, Function) when is_atom(Name), is_function(Function) ->
    KilledByDeath = fun (Victim) ->
        exit(Victim, kill), fail
    end,
    PID = spawn(Function),
    try
        case register(Name, PID) of
            true -> PID;
            _ -> KilledByDeath(PID)
        end
    catch
        _:_ -> KilledByDeath(PID)
    end.
