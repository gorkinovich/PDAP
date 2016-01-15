%%%-------------------------------------------------------------------
%%% PDAP: Práctica 2 de OTP
%%% @author Gorka Suárez García
%%% Created : 14. ene 2016 1:10 AM
%%%-------------------------------------------------------------------

-module(emptyserver).
-author("Gorka Suárez García").
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2,
    handle_info/2, terminate/2, code_change/3]).
-define(SERVER, ?MODULE).

%---------------------------------------------------------------------
% Interface:
%---------------------------------------------------------------------

start_link() ->
    gen_server:start_link(?MODULE, [], []).

%---------------------------------------------------------------------
% Callbacks:
%---------------------------------------------------------------------

init(_) ->
    {ok, ok}.

handle_call(Request, _, State) ->
    io:format("Call { Request: ~w }~n", [Request]),
    {reply, ok, State}.

handle_cast(Request, State) ->
    io:format("Cast { Request: ~w }~n", [Request]),
    {noreply, State}.

handle_info(Msg, State) ->
    io:format("Info { Request: ~w }~n", [Msg]),
    {noreply, State}.

terminate(Reason, _) ->
    io:format("Terminate { Request: ~w }~n", [Reason]).

code_change(_, State, _) ->
    {ok, State}.
