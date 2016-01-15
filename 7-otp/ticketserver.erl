%%%-------------------------------------------------------------------
%%% PDAP: Práctica 2 de OTP
%%% @author Gorka Suárez García
%%% Created : 13. ene 2016 10:24 PM
%%%-------------------------------------------------------------------

-module(ticketserver).
-author("Gorka Suárez García").
-behaviour(gen_server).
-export([start_link/1, send_get_ticket/0]).
-export([init/1, handle_call/3, handle_cast/2,
    handle_info/2, terminate/2, code_change/3]).
-define(SERVER, ?MODULE).

%---------------------------------------------------------------------
% Interface:
%---------------------------------------------------------------------

start_link(N) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, N, []).

send_get_ticket() ->
    gen_server:call(?SERVER, get_ticket).

%---------------------------------------------------------------------
% Callbacks:
%---------------------------------------------------------------------

init(N) when is_number(N), N > 0 ->
    {ok, lists:seq(1, N)};
init(N) ->
    {stop, {bad_parameter, N}}.

handle_call(get_ticket, _, []) ->
    {stop, no_tickets, error, []};
handle_call(get_ticket, _, [T]) ->
    {stop, normal, T, []};
handle_call(get_ticket, _, [T|TS]) ->
    {reply, T, TS}.

handle_cast(Request, State) ->
    io:format("Unexpected request: ~w~n", [Request]),
    {noreply, State}.

handle_info(Msg, State) ->
    io:format("Unexpected message: ~w~n", [Msg]),
    {noreply, State}.

terminate(Reason, _) ->
    io:format("Ticket server finished.~n"),
    io:format("Reason: ~w~n", [Reason]).

code_change(_, State, _) ->
    {ok, State}.
