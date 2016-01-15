%%%-------------------------------------------------------------------
%%% PDAP: Práctica 2 de OTP
%%% @author Gorka Suárez García
%%% Created : 13. ene 2016 10:22 PM
%%%-------------------------------------------------------------------

-module(fileserver).
-author("Gorka Suárez García").
-behaviour(gen_server).
-export([start_link/1, send_list_dir/0,
    send_get_file/1, send_change_dir/1]).
-export([init/1, handle_call/3, handle_cast/2,
    handle_info/2, terminate/2, code_change/3]).
-define(SERVER, ?MODULE).

%---------------------------------------------------------------------
% Interface:
%---------------------------------------------------------------------

start_link(Dir) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Dir, []).

send_list_dir() ->
    gen_server:call(?SERVER, list_dir).

send_get_file(File) ->
    gen_server:call(?SERVER, {get_file, File}).

send_change_dir(Dir) ->
    gen_server:cast(?SERVER, {change_dir, Dir}).

%---------------------------------------------------------------------
% Callbacks:
%---------------------------------------------------------------------

init(Dir) ->
    {ok, Dir}.

handle_call(list_dir, _, Dir) ->
    {reply, file:list_dir(Dir), Dir};
handle_call({get_file, File}, _, Dir) ->
    Path = filename:join(Dir, File),
    {reply, file:read_file(Path), Dir}.

handle_cast({change_dir, Dir}, _) ->
    {noreply, Dir}.

handle_info(Msg, Dir) ->
    io:format("Unexpected message: ~w~n", [Msg]),
    {noreply, Dir}.

terminate(Reason, _) ->
    io:format("File server finished.~n"),
    io:format("Reason: ~w~n", [Reason]).

code_change(_, Dir, _) ->
    {ok, Dir}.
