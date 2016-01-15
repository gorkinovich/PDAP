%%%-------------------------------------------------------------------
%%% PDAP: Práctica 2 de OTP
%%% @author Gorka Suárez García
%%% Created : 13. ene 2016 10:27 PM
%%%-------------------------------------------------------------------

-module(supervisor1).
-author("Gorka Suárez García").
-export([restarter/3, restarter/1]).

%---------------------------------------------------------------------
% Interface:
%---------------------------------------------------------------------

restarter(Mod, Fun, Args) ->
    process_flag(trap_exit, true),
    PID = spawn_link(Mod, Fun, Args),
    receive
        {'EXIT', PID, normal} -> ok;
        {'EXIT', PID, shutdown} -> ok;
        {'EXIT', PID, _} -> restarter(Mod, Fun, Args)
    end.

restarter(MFAS) ->
    restarter_loop([{launch_with_link(V), V} || V <- MFAS]).

%---------------------------------------------------------------------
% Private:
%---------------------------------------------------------------------

launch_with_link({Mod, Fun, Args}) ->
    spawn_link(Mod, Fun, Args).

restarter_loop(State) ->
    receive
        {'EXIT', PID, normal} ->
            NS = lists:keydelete(PID, 1, State),
            restarter_loop(NS);
        {'EXIT', PID, shutdown} ->
            NS = lists:keydelete(PID, 1, State),
            restarter_loop(NS);
        {'EXIT', PID, _} ->
            case lists:keyfind(PID, 1, State) of
                {PID, MFA} ->
                    NS = lists:keydelete(PID, 1, State),
                    NPID = launch_with_link(MFA),
                    restarter_loop([{NPID, MFA} | NS]);
                _ ->
                    restarter_loop(State)
            end
    end.
