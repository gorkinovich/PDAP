%%%-------------------------------------------------------------------
%%% PDAP: Práctica 1 de OTP
%%% @author Gorka Suárez García
%%% Created : 12. ene 2016 6:54 PM
%%%-------------------------------------------------------------------

-module(jobclient).
-author("Gorka Suárez García").
-compile(export_all).
-define(RND_TIME, 10).
-define(BASE_TIME, 100).

%---------------------------------------------------------------------
% General:
%---------------------------------------------------------------------

delay_ms(N) when is_number(N) ->
    receive after N -> ok end.

delay(N) when is_number(N) ->
    delay_ms(N * 1000).

get_random_time() ->
    % Select between 100 & 1000 ms:
    random:uniform(?RND_TIME) * ?BASE_TIME.

random_delay() ->
    delay_ms(get_random_time()).

make_test_function() ->
    T = get_random_time(),
    fun () ->
        io:format("~w begin~n", [self()]),
        delay_ms(T),
        io:format("~w end~n", [self()])
    end.

%---------------------------------------------------------------------
% Client:
%---------------------------------------------------------------------

start(J, C) ->
    start(J, C, 0).

start(J, C, T) ->
    jobserver:start(T),
    [jobserver:send_new_job(make_test_function()) || _ <- lists:seq(1, J)],
    [launch_client(CID) || CID <- lists:seq(1, C)].

launch_client(CID) ->
    GetJob = fun (Master) ->
        case jobserver:send_get_job() of
            {R, F} when is_reference(R), is_function(F) ->
                io:format("<~w> Received job with ref. ~w~n", [CID, R]),
                random_delay(),
                spawn_link(fun () ->
                    F(),
                    Master ! {finished, R}
                end),
                R;
            _ ->
                nothing
        end
    end,
    GetJobAndLoop = fun (Master, Loop) ->
        case GetJob(Master) of
            R when is_reference(R) ->
                Loop(R);
            _ ->
                goodbye
        end
    end,
    spawn(fun () ->
        Meself = self(),
        io:format("<~w> Launched with PID: ~w~n", [CID, Meself]),
        Loop = fun InnerLoop (R) ->
            receive
                {finished, R} ->
                    case jobserver:send_job_finished(R) of
                        ok ->
                            io:format("<~w> Job finished with ref. ~w~n", [CID, R]);
                        _ ->
                            io:format("<~w> Job finished with invalid ref. ~w~n", [CID, R])
                    end,
                    GetJobAndLoop(Meself, InnerLoop);
                exterminate ->
                    io:format("<~w> Finished from outside~n", [CID]);
                _ ->
                    InnerLoop(R)
            end
        end,
        GetJobAndLoop(Meself, Loop)
    end).
