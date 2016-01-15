%%%-------------------------------------------------------------------
%%% PDAP: Práctica 2 de OTP
%%% @author Gorka Suárez García
%%% Created : 13. ene 2016 10:29 PM
%%%-------------------------------------------------------------------

-module(supervisor2).
-author("Gorka Suárez García").
-behaviour(supervisor).
-export([start_link/2, init/1]).
-define(SERVER, ?MODULE).

-define(RESTART_STRATEGY, one_for_one).
-define(MAX_RESTARTS, 1000).
-define(MAX_SECS_BETWEEN_RESTARTS, 3600).
-define(SHUTDOWN, 1000).
-define(RESTART, transient).

%---------------------------------------------------------------------
% Interface:
%---------------------------------------------------------------------

start_link(Dir, N) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, {Dir, N}).

%---------------------------------------------------------------------
% Callbacks:
%---------------------------------------------------------------------

init({Dir, N}) ->
    Options = {?RESTART_STRATEGY, ?MAX_RESTARTS,
        ?MAX_SECS_BETWEEN_RESTARTS},
    Victims = [
        get_worker_info(file_id, fileserver, Dir),
        get_worker_info(area_id, areaserver),
        get_worker_info(ticket_id, ticketserver, N)
    ],
    {ok, {Options, Victims}}.

%---------------------------------------------------------------------
% Private:
%---------------------------------------------------------------------

get_worker_info(ID, M) ->
    io:format("ID = ~w, Module = ~w~n", [ID, M]),
    {ID, {M, start_link, []}, ?RESTART, ?SHUTDOWN, worker, [M]}.

get_worker_info(ID, M, A) ->
    io:format("ID = ~w, Module = ~w, Args. = ~w~n", [ID, M, A]),
    {ID, {M, start_link, [A]}, ?RESTART, ?SHUTDOWN, worker, [M]}.
