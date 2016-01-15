%%%-------------------------------------------------------------------
%%% PDAP: Práctica 2 de OTP
%%% @author Gorka Suárez García
%%% Created : 13. ene 2016 10:30 PM
%%%-------------------------------------------------------------------

-module(supervisor3).
-author("Gorka Suárez García").
-behaviour(supervisor).
-export([start_link/2, start_link/3, init/1]).
-define(SERVER, ?MODULE).

-define(RESTART_STRATEGY, one_for_one).
-define(MAX_RESTARTS, 1000).
-define(MAX_SECS_BETWEEN_RESTARTS, 3600).
-define(SHUTDOWN, 1000).

%---------------------------------------------------------------------
% Interface:
%---------------------------------------------------------------------

start_link(NumLevels, Module) ->
    start_link(NumLevels, Module, true).

start_link(N, M, true) ->
    io:format("Supervisor: N = ~w, Module = ~w, local register~n", [N, M]),
    supervisor:start_link({local, ?SERVER}, ?MODULE, {N, M});
start_link(N, M, false) ->
    io:format("Supervisor: N = ~w, Module = ~w~n", [N, M]),
    supervisor:start_link(?MODULE, {N, M}).

%---------------------------------------------------------------------
% Callbacks:
%---------------------------------------------------------------------

init({1, M}) ->
    {ok, {get_options(), [
        get_worker_info(make_atom_ref(), M),
        get_worker_info(make_atom_ref(), M)
    ]}};
init({N, M}) when is_number(N), N > 1 ->
    {ok, {get_options(), [
        get_super_info(make_atom_ref(), N, M),
        get_super_info(make_atom_ref(), N, M)
    ]}};
init(_) ->
    ignore.

%---------------------------------------------------------------------
% Private:
%---------------------------------------------------------------------

get_options() ->
    {?RESTART_STRATEGY, ?MAX_RESTARTS, ?MAX_SECS_BETWEEN_RESTARTS}.

get_super_info(ID, N, M) ->
    io:format("ID = ~w, N = ~w, Module = ~w~n", [ID, N, M]),
    {ID, {supervisor3, start_link, [N - 1, M, false]}, permanent,
        ?SHUTDOWN, supervisor, [supervisor3]}.

get_worker_info(ID, M) ->
    io:format("ID = ~w, Module = ~w~n", [ID, M]),
    {ID, {M, start_link, []}, permanent, ?SHUTDOWN, worker, [M]}.

make_atom_ref() ->
    list_to_atom(erlang:ref_to_list(make_ref())).
