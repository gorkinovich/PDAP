-module(madness).
-author("gorkinovich").
-compile(export_all).
%-import(io, [format/1, format/2]).
%io:format("~p~n", [lists:seq(32,126)]).
%io:format("~w~n", [lists:seq(32,126)]).

launch_factorial() ->
    case whereis(factorial) of
        VID when is_pid(VID) ->
            unregister(factorial),
            exit(VID, kill);
        _ -> ok
    end,
    PID = spawn(fun Factorial() ->
        ThisPID = self(),
        receive
            {CallerPID, stop} when is_pid(CallerPID) ->
                exit(stop);
            {CallerPID, Number} when is_pid(CallerPID),
                                     is_number(Number) ->
                if Number <  0 -> CallerPID ! {ThisPID, invalid_args};
                   Number == 0 -> CallerPID ! {ThisPID, 1};
                   true        -> ThisPID ! {step, CallerPID, Number - 1, Number}
                end;
            {step, CallerPID, 0, Result} when is_pid(CallerPID),
                                              is_number(Result) ->
                CallerPID ! {ThisPID, Result};
            {step, CallerPID, Number, Result} when is_pid(CallerPID),
                                                   is_number(Number),
                                                   is_number(Result) ->
                ThisPID ! {step, CallerPID, Number - 1, Number * Result};
            _ ->
                io:format("[Factorial] ERROR: Invalid call!~n")
        end,
        Factorial()
    end),
    register(factorial, PID).

call_factorial(Number) ->
    case whereis(factorial) of
        undefined -> launch_factorial();
        _ -> ok
    end,
    if is_number(Number) ->
        factorial ! {self(), Number},
        receive
            {_, Value} -> Value
        end;
      true ->
        nothing
    end.
