%%%-------------------------------------------------------------------
%%% PDAP: Práctica 1 de concurrencia
%%% @author Gorka Suárez García
%%% Created : 11. nov 2015 5:31 PM
%%%-------------------------------------------------------------------

-module(afile_client).
-author("Gorka Suárez García").
-export([list_dir/1, get_file/2, change_dir/2]).

list_dir(ServerPID) ->
    ServerPID ! {self(), list_dir},
    receive
        {ServerPID, FileList} ->
            FileList
    end.

get_file(ServerPID, FileName) ->
    ServerPID ! {self(), get_file, FileName},
    receive
        {ServerPID, FileContent} ->
            FileContent
    end.

change_dir(ServerPID, NextDir) ->
    ServerPID ! {self(), change_dir, NextDir},
    receive
        {ServerPID, Response} ->
            Response
    end.
