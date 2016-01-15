%%%-------------------------------------------------------------------
%%% PDAP: Práctica 2 de concurrencia
%%% @author Gorka Suárez García
%%% Created : 26. nov 2015 12:19 PM
%%%-------------------------------------------------------------------

-module(game).
-author("Gorka Suárez García").
-export([start/1, player_loop/2]).

start(N) when is_number(N), N > 0 ->
    spawn(fun () ->
        register(simon, self()),
        io:format("simon says: Players, get over here!~n"),
        Instances = make_player_instances(make_player_names(N)),
        send_ball(simon, Instances),
        simon_loop(Instances)
    end);
start(_) ->
    io:format("simon says: Invalid parameters!~n").

make_player_names(N) ->
    [list_to_atom("player" ++ Y) || Y <- [integer_to_list(X) || X <- lists:seq(1, N)]].

make_player_instances(Names) ->
    RawInstances = [make_player_instance(N, Names) || N <- Names],
    [X || X <- RawInstances, X =/= nothing].

make_player_instance(Name, Names) ->
    OtherNames = lists:delete(Name, Names),
    PID = spawn(?MODULE, player_loop, [Name, OtherNames]),
    case register(Name, PID) of
        true -> {Name, PID};
        _ -> exit(PID, kill), nothing
    end.

player_loop(MyName, OtherNames) ->
    io:format("~w says: I'm waiting the ball!~n", [MyName]),
    receive
        {simon, stop} ->
            io:format("~w says: The game is over for me!~n", [MyName]);
        {Name, ball} ->
            io:format("~w says: ~w send me the ball!~n", [MyName, Name]),
            send_ball(MyName, OtherNames),
            player_loop(MyName, OtherNames);
        MSG ->
            io:format("~w says: I don't understand this!~n", [MyName]),
            io:format("[MSG:begin]~n~p~n[MSG:end]~n", [MSG]),
            player_loop(MyName, OtherNames)
    end.

simon_loop(Instances) ->
    GameOver = fun () ->
        io:format("simon says: The game is over!~n"),
        [Name ! {simon, stop} || {Name, _} <- Instances]
    end,
    receive
        stop ->
            GameOver();
        {_, stop} ->
            GameOver();
        MSG ->
            io:format("simon says: I don't understand this!~n"),
            io:format("[MSG:begin]~n~p~n[MSG:end]~n", [MSG]),
            simon_loop(Instances)
    end.

send_ball(Name, Victims) ->
    Send = fun (V) ->
        try
            wait_some_time(),
            io:format("~w says: ~w get this ball!~n", [Name, V]),
            V ! {Name, ball}
        catch _:_ ->
            io:format("~w says: I can't send to ~w the ball!~n", [Name, V])
        end
    end,
    Index = random:uniform(length(Victims)),
    case lists:nth(Index, Victims) of
        Victim when is_atom(Victim) -> Send(Victim);
        {Victim, _} when is_atom(Victim) -> Send(Victim);
        _ -> nothing
    end.

wait_some_time() ->
    Time = random:uniform(10) * 100,
    receive after Time -> ok end.
