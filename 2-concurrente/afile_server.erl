%%%-------------------------------------------------------------------
%%% PDAP: Práctica 1 de concurrencia
%%% @author Gorka Suárez García
%%% Created : 11. nov 2015 5:20 PM
%%%-------------------------------------------------------------------

-module(afile_server).
-author("Gorka Suárez García").
-export([start/1, loop/1]).

start(InitialDir) ->
    spawn(afile_server, loop, [InitialDir]).

loop(CurrentDir) ->
    receive
        {ClientPID, list_dir} ->
            ClientPID ! {self(), file:list_dir(CurrentDir)},
            loop(CurrentDir);
        {ClientPID, get_file, FileName} ->
            FilePath = filename:join(CurrentDir, FileName),
            ClientPID ! {self(), file:read_file(FilePath)},
            loop(CurrentDir);
        {ClientPID, change_dir, NextDir} ->
            case filelib:is_dir(NextDir) of
               true ->
                   ClientPID ! {self(), dir_changed},
                   loop(NextDir);
                _ ->
                    ClientPID ! {self(), invalid_dir},
                    loop(CurrentDir)
            end
    end.
