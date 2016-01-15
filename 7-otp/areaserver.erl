%%%-------------------------------------------------------------------
%%% PDAP: Práctica 2 de OTP
%%% @author Gorka Suárez García
%%% Created : 13. ene 2016 10:23 PM
%%%-------------------------------------------------------------------

-module(areaserver).
-author("Gorka Suárez García").
-behaviour(gen_server).
-export([start_link/0, send_rectangle/2, send_circle/1]).
-export([init/1, handle_call/3, handle_cast/2,
    handle_info/2, terminate/2, code_change/3]).
-define(SERVER, ?MODULE).

%---------------------------------------------------------------------
% Interface:
%---------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, 0, []).

send_rectangle(Width, Height) ->
    gen_server:call(?SERVER, {rectangle, Width, Height}).

send_circle(Radius) ->
    gen_server:call(?SERVER, {circle, Radius}).

%---------------------------------------------------------------------
% Callbacks:
%---------------------------------------------------------------------

init(N) ->
    {ok, N}.

handle_call({rectangle, Width, Height}, _, State) ->
    {reply, Width * Height, State};
handle_call({circle, Radius}, _, State) ->
    {reply, math:pi() * Radius * Radius, State};
handle_call(Other, _, State) ->
    {reply, {unknown, Other}, State}.

handle_cast(Request, State) ->
    io:format("Unexpected request: ~w~n", [Request]),
    {noreply, State}.

handle_info(Msg, State) ->
    io:format("Unexpected message: ~w~n", [Msg]),
    {noreply, State}.

terminate(Reason, _) ->
    io:format("Area server finished.~n"),
    io:format("Reason: ~w~n", [Reason]).

code_change(_, N, _) ->
    {ok, N + 1}.
