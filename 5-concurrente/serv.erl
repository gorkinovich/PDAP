%%%-------------------------------------------------------------------
%%% PDAP: Práctica 2 de concurrencia
%%% @author Gorka Suárez García
%%% Created : 02. dic 2015 5:40 PM
%%%-------------------------------------------------------------------

-module(serv).
-author("Gorka Suárez García").
-export([start/0, loop/1, play/0, stop/0, players/0]).
-define(SERVER_NAME, 'GSG:WordsGameServer').

%%%-------------------------------------------------------------------
%%% Main functions
%%%-------------------------------------------------------------------

start() ->
    io:format("[SERVER] Creating server process.~n"),
    PID = spawn(?MODULE, loop, [get_initial_state()]),
    register(?SERVER_NAME, PID).

loop(State) ->
    receive
        play                       -> play_message(State);
        stop                       -> stop_message(State);
        players                    -> players_message(State);
        {From, {word, Word}}       -> word_message(State, From, Word);
        {From, {connect, Name}}    -> connect_message(State, From, Name);
        {From, {disconnect, Name}} -> disconnect_message(State, From, Name);
        _                          -> invalid_message(State)
    end.

play() -> ?SERVER_NAME ! play.
stop() -> ?SERVER_NAME ! stop.
players() -> ?SERVER_NAME ! players.

%%%-------------------------------------------------------------------
%%% Message functions
%%%-------------------------------------------------------------------

play_message(State) ->
    io:format("[SERVER] Creating server process.~n"),
    NextState = set_next_word_and_send(State),
    loop(NextState).

stop_message(State) ->
    io:format("[SERVER] Stoping server process.~n"),
    send_to_players(State, {self(), stop}).

players_message(State) ->
    show_players(State),
    loop(State).

word_message(State, From, Word) ->
    Name = get_player_name(State, From),
    case Name =/= [] of
        true ->
            io:format("[SERVER] ~s from ~w sending: ~p~n", [Name, From, Word]),
            case is_current_word(State, Word) of
                true ->
                    io:format("[SERVER] Good awnser from player ~s.~n", [Name]),
                    send_to_players(State, {self(), {wins, Name}}),
                    NextState = set_next_word_and_send(State),
                    loop(NextState);
                _ ->
                    io:format("[SERVER] Failed awnser from player ~s.~n", [Name]),
                    From ! {self(), {fails, Name}},
                    loop(State)
            end;
        _ ->
            io:format("[SERVER] A stranger is trying to play from ~w.~n", [From]),
            loop(State)
    end.

connect_message(State, From, Name) ->
    io:format("[SERVER] User ~s at process ~w is connected.~n", [Name, From]),
    NextState = add_player(State, Name, From),
    loop(NextState).

disconnect_message(State, From, Name) ->
    io:format("[SERVER] User ~s at process ~w is disconnected.~n", [Name, From]),
    NextState = remove_player(State, Name, From),
    loop(NextState).

invalid_message(State) ->
    io:format("[SERVER] Invalid message!~n"),
    loop(State).

%%%-------------------------------------------------------------------
%%% Util functions
%%%-------------------------------------------------------------------

get_initial_state() ->
    % {Players = {Name, Process}, CurrentWord}
    {[], get_random_word()}.

get_game_words() ->
    ["hello", "world", "dalek", "doctor", "davros"].

get_random_word() ->
    get_random_word(get_game_words()).

get_random_word(Words) ->
    Index = random:uniform(length(Words)),
    lists:nth(Index, Words).

add_player({Players, CurrentWord}, Name, Process) ->
    Others = [{N, P} || {N, P} <- Players, N =/= Name],
    {[{Name, Process} | Others], CurrentWord}.

remove_player({Players, CurrentWord}, Name, Process) ->
    {[{N, P} || {N, P} <- Players, not (N =:= Name andalso P =:= Process)], CurrentWord}.

is_current_word({_, CurrentWord}, Word) ->
    CurrentWord =:= Word.

send_to_players({Players, _}, Msg) ->
    [P ! Msg || {_, P} <- Players].

get_player_name({Players, _}, From) ->
    Names = [N || {N, P} <- Players, P =:= From],
    case length(Names) > 0 of
        true -> hd(Names);
        _ -> []
    end.

show_players({Players, _}) ->
    io:format("[SERVER] List of players:~n"),
    [io:format("Name: ~s, From:~w~n", [N, P]) || {N, P} <- Players].

set_next_word_and_send(State) ->
    NextWord = get_random_word(),
    io:format("[SERVER] Next word: ~s~n", [NextWord]),
    send_to_players(State, {self(), {word, NextWord}}),
    {Players, _} = State,
    {Players, NextWord}.
