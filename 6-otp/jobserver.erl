%%%-------------------------------------------------------------------
%%% PDAP: Práctica 1 de OTP
%%% @author Gorka Suárez García
%%% Created : 16. dic 2015 6:02 PM
%%%-------------------------------------------------------------------

-module(jobserver).
-author("Gorka Suárez García").
-export([start/0, start/1, stop/0, kill/0, send_new_job/1,
         send_get_job/0, send_job_finished/1, init/1, handle_call/3,
         handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-behavior(gen_server).
-define(NAME, ?MODULE).

%---------------------------------------------------------------------
% Interface:
%---------------------------------------------------------------------

start() ->
    gen_server:start_link({local, ?NAME}, ?MODULE, nothing, []).

start(N) when is_number(N) ->
    gen_server:start_link({local, ?NAME}, ?MODULE, N, []).

kill() ->
    exit(whereis(?NAME), kill).

stop() ->
    gen_server:cast(?NAME, {exterminate}).

send_new_job(F) when is_function(F) ->
    gen_server:cast(?NAME, {new_job, F}).

send_get_job() ->
    gen_server:call(?NAME, {get_job}).

send_job_finished(R) when is_reference(R) ->
    gen_server:call(?NAME, {job_finished, R}).

%---------------------------------------------------------------------
% Callbacks:
%---------------------------------------------------------------------

init(Value) ->
    io:format("[~w] Operation: Init server~n", [?NAME]),
    io:format("[~w] >>> Value: ~w~n~n", [?NAME, Value]),
    case Value of
        N when is_number(N), N > 0 ->
            {ok, {[], queue:new(), N}};
        _ ->
            {ok, {[], queue:new(), 0}}
    end. %Page 48

handle_call(Request, From, State) ->
    io:format("[~w] Operation: Call message~n", [?NAME]),
    io:format("[~w] >>> Request: ~w~n", [?NAME, Request]),
    io:format("[~w] >>> From: ~w~n", [?NAME, From]),
    io:format("[~w] >>> State: ~w~n~n", [?NAME, State]),
    case Request of
        {get_job} ->
            case remove_job_from_queue(State) of
                {F, NS} when is_function(F) ->
                    {PID, _} = From,
                    {R, NS2} = add_job_to_assigned(NS, PID, F),
                    TO = get_time_out(State),
                    if TO > 0 ->
                        spawn(fun () ->
                            receive after TO -> ok end,
                            gen_server:cast(?NAME, {check_job, R, PID})
                        end)
                    end,
                    {reply, {R, F}, NS2};
                {_, NS} ->
                    {reply, no_jobs, NS}
            end;
        {job_finished, R} when is_reference(R) ->
            case is_job_in_assigned(State, R) of
                true ->
                    {reply, ok, remove_job_from_assigned(State, R)};
                _ ->
                    {reply, invalid_reference, State}
            end;
        _ ->
            {reply, invalid_request, State}
    end. %Page 54

handle_cast(Request, State) ->
    io:format("[~w] Operation: Cast message~n", [?NAME]),
    io:format("[~w] >>> Request: ~w~n", [?NAME, Request]),
    io:format("[~w] >>> State: ~w~n~n", [?NAME, State]),
    case Request of
        {new_job, F} when is_function(F) ->
            {noreply, add_job_to_queue(State, F)};
        {check_job, R, P} when is_reference(R), is_pid(P) ->
            case get_job_from_assigned(State, R) of
                {R, P, F} ->
                    P ! exterminate,
                    NS = remove_job_from_assigned(State, R),
                    {noreply, add_job_to_queue(NS, F)};
                _ ->
                    {noreply, State}
            end;
        {exterminate} ->
            {stop, killed_by_death, State};
        _ ->
            {noreply, State}
    end. %Page 58

handle_info(Message, State) ->
    io:format("[~w] Operation: Unexpected message~n", [?NAME]),
    io:format("[~w] >>> Message: ~w~n", [?NAME, Message]),
    io:format("[~w] >>> State: ~w~n~n", [?NAME, State]),
    {noreply, State}. %Page 60

terminate(Reason, State) ->
    io:format("[~w] Operation: Terminate server~n", [?NAME]),
    io:format("[~w] >>> Reason: ~w~n", [?NAME, Reason]),
    io:format("[~w] >>> State: ~w~n~n", [?NAME, State]),
    ok. %Page 62

code_change(PrevVersion, State, Extra) ->
    io:format("[~w] Operation: Terminate server~n", [?NAME]),
    io:format("[~w] >>> Prev. version: ~w~n", [?NAME, PrevVersion]),
    io:format("[~w] >>> State: ~w~n", [?NAME, State]),
    io:format("[~w] >>> Extra: ~w~n~n", [?NAME, Extra]),
    {ok, State}. %Page 64

%---------------------------------------------------------------------
% State:
%---------------------------------------------------------------------

get_assigned_jobs({V, _, _}) -> V.
get_unassigned_jobs({_, V, _}) -> V.
get_time_out({_, _, V}) -> V.
set_assigned_jobs({_, UJ, TO}, AJ) -> {AJ, UJ, TO}.
set_unassigned_jobs({AJ, _, TO}, UJ) -> {AJ, UJ, TO}.

add_job_to_queue(S, F) ->
    UJ = queue:in(F, get_unassigned_jobs(S)),
    set_unassigned_jobs(S, UJ).

remove_job_from_queue(S) ->
    case queue:out(get_unassigned_jobs(S)) of
        {{value, F}, UJ} -> {F, set_unassigned_jobs(S, UJ)};
        _ -> {nil, S}
    end.

add_job_to_assigned(S, P, F) ->
    AJ = [{R = make_ref(), P, F} | get_assigned_jobs(S)],
    {R, set_assigned_jobs(S, AJ)}.

remove_job_from_assigned(S, R) ->
    AJ = lists:keydelete(R, 1, get_assigned_jobs(S)),
    set_assigned_jobs(S, AJ).

get_job_from_assigned(S, R) ->
    lists:keyfind(R, 1, get_assigned_jobs(S)).

is_job_in_assigned(S, R) ->
    is_tuple(get_job_from_assigned(S, R)).
