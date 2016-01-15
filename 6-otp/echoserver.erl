%%%-------------------------------------------------------------------
%%% PDAP: Práctica 1 de OTP
%%% @author Gorka Suárez García
%%% Created : 12. ene 2016 11:11 PM
%%%-------------------------------------------------------------------

-module(echoserver).
-author("Gorka Suárez García").
-export([start/0, stop/0, print/1, init/1, handle_call/3,
    handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-behavior(gen_server).
-define(NAME, ?MODULE).
-define(MAX_TIME, 5000).

%---------------------------------------------------------------------
% Interface:
%---------------------------------------------------------------------

start() ->
    gen_server:start_link({local, ?NAME}, ?MODULE, 0, []).

stop() ->
    gen_server:call(?NAME, stop).

print(Victim) ->
    try
        case gen_server:call(?NAME, Victim, ?MAX_TIME) of
            ok -> io:format("Done.~n");
            _ -> io:format("Error!~n")
        end
    catch
        _:_ -> io:format("Time out!~n")
    end.

%---------------------------------------------------------------------
% Callbacks:
%---------------------------------------------------------------------

init(_) ->
    io:format("~w init~n", [?NAME]),
    {ok, mayor_tom}.

handle_call(Request, From, State) ->
    io:format("~w call ~w from ~w~n", [?NAME, Request, From]),
    case Request of
        stop ->
            io:format("ECHO: ~w~n", [stop]),
            {stop, stop, ok, State};
        Victim when is_atom(Victim) ->
            io:format("ECHO: ~w~n", [Victim]),
            {reply, ok, State};
        _ ->
            {reply, nil, State}
    end.

handle_cast(Request, State) ->
    io:format("~w cast ~w~n", [?NAME, Request]),
    {noreply, State}.

handle_info(Message, State) ->
    io:format("~w signal ~w~n", [?NAME, Message]),
    {noreply, State}.

terminate(Reason, _) ->
    io:format("~w terminate [~w]~n", [?NAME, Reason]),
    ok.

code_change(_, State, _) ->
    {ok, State}.
