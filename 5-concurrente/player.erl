%%%-------------------------------------------------------------------
%%% PDAP: Práctica 2 de concurrencia
%%% @author Gorka Suárez García
%%% Created : 02. dic 2015 5:40 PM
%%%-------------------------------------------------------------------

-module(player).
-author("Gorka Suárez García").
-export([start/1, loop/3]).

start(ServerName) ->
    Name = get_user_name(),
    send_msg(ServerName, {self(), {connect, Name}}),
    loop(ServerName, Name, "").

loop(ServerName, Name, CurrentWord) ->
    receive
        {From, stop} when From == self() ->
            io:format("I leave this game, bye cruel world!.~n"),
            send_msg(ServerName, {self(), {disconnect, Name}});
        {From, stop} when node(From) == ServerName ->
            io:format("The game is over, bye.~n");
        {From, {word, Word}} when node(From) == ServerName ->
            get_user_input(Name, From, Word),
            loop(ServerName, Name, Word);
        {From, {wins, Player}} when node(From) == ServerName ->
            case Name =:= Player of
                true -> io:format("You wins this round.~n");
                _ -> io:format("Player ~s wins this round.~n", [Player])
            end,
            loop(ServerName, Name, CurrentWord);
        {From, {fails, Player}} when node(From) == ServerName ->
            if Name =:= Player ->
                io:format("Wrong awnser, try again.~n"),
                get_user_input(Name, From, CurrentWord)
            end,
            loop(ServerName, Name, CurrentWord);
        _ ->
            loop(ServerName, Name, CurrentWord)
    end.

send_msg(ServerName, Msg) ->
    {'GSG:WordsGameServer', ServerName} ! Msg.

get_user_name() ->
    NodeName = atom_to_list(node()),
    Filter = fun (X) -> X =/= hd("@") end,
    {Name, _} = lists:splitwith(Filter, NodeName),
    Name.

get_user_input(PlayerName, From, Word) ->
    InputName = list_to_atom(PlayerName ++ ":InputWord"),
    exterminate_input_process(InputName),
    launch_input_process(From, Word, InputName).

exterminate_input_process(Name) ->
    PID = whereis(Name),
    case is_pid(PID) of
        true -> unregister(Name),
                exit(PID, kill);
        _    -> ok
    end.

launch_input_process(From, Word, Name) ->
    ClientPID = self(),
    register(Name, spawn(fun () ->
        io:format("Write ~p", [Word]),
        Input = io:get_line("> "),
        Anwser = string:sub_string(Input, 1, string:len(Input) - 1),
        case Anwser =:= ":q" of
            true -> ClientPID ! {ClientPID, stop};
            _ -> From ! {ClientPID, {word, Anwser}}
        end
    end)).
