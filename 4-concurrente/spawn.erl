%%%-------------------------------------------------------------------
%%% PDAP: Práctica 3 de concurrencia
%%% @author Gorka Suárez García
%%% Created : 25. nov 2015 5:30 PM
%%%-------------------------------------------------------------------

-module(spawn).
-author("Gorka Suárez García").
-export([test/1, my_spawn_1a/3, my_spawn_1b/3, on_exit/2, my_spawn_2/3,
         my_spawn_3/4, launch_4/1, launch_5/1, launch_6/1]).

%====================================================================================================
% Funciones generales del módulo:
%====================================================================================================

test(N) when N =< 0 -> ok;
test(N) -> receive after 1000 -> ok end, test(N - 1).

get_time_stamp() ->
    {MegaSecs, Secs, MicroSecs} = erlang:timestamp(),
    (MegaSecs * 1000000.0) + Secs + (MicroSecs / 1000000.0).

%====================================================================================================
% 1. Escribir una función my spawn(Mod, Func, Args) que se comporte como spawn(Mod,Func, Args),
%    pero con una diferencia: si el proceso generado muere, debe escribir un mensaje informando de la
%    causa cuánto tiempo ha vivido el proceso antes de morir.
%====================================================================================================

my_spawn_1a(Module, Function, Args) ->
    % Lanzamos el proceso y obtenemos una marca de tiempo:
    PID = spawn(Module, Function, Args),
    StartTime = get_time_stamp(),
    % Lanzamos el proceso que hará de monitor del anterior:
    spawn(fun () ->
        % Configuramos el proceso para que esté vinculado al proceso lanzado y
        % que además pueda capturar las señales de terminación mediante mensajes:
        process_flag(trap_exit, true),
        link(PID),
        receive
            % Si se recibe un mensaje de salida del proceso vigilado, se obtendrá una
            % nueva marca de tiempo para ver lo que ha durado y se informará de ello:
            {'EXIT', PID, Reason} ->
                EndTime = get_time_stamp(),
                io:format("~w -> ~p => ~p~n", [PID, Reason, EndTime - StartTime])
        end
    end),
    PID.

my_spawn_1b(Module, Function, Args) ->
    % Lanzamos el proceso y obtenemos una marca de tiempo:
    PID = spawn(Module, Function, Args),
    StartTime = get_time_stamp(),
    % Lanzamos el proceso que hará de monitor del anterior:
    spawn(fun () ->
        % Configuramos el proceso para que monitorice al proceso lanzado:
        Reference = erlang:monitor(process, PID),
        receive
            % Si se recibe un mensaje de salida del proceso vigilado, se obtendrá una
            % nueva marca de tiempo para ver lo que ha durado y se informará de ello:
            {'DOWN', Reference, process, PID, Reason} ->
                EndTime = get_time_stamp(),
                io:format("~w -> ~p => ~p~n", [PID, Reason, EndTime - StartTime])
        end
    end),
    PID.

%====================================================================================================
% 2. Resolver el ejercicio previo usando la función on exit vista en clase.
%====================================================================================================

my_spawn_2(Module, Function, Args) ->
    % Lanzamos el proceso y obtenemos una marca de tiempo:
    PID = spawn(Module, Function, Args),
    StartTime = get_time_stamp(),
    % Mediante la función on_exit registraremos una función que capture el evento de finalización,
    % para que cuando ocurra se obtenga una nueva marca de tiempo para ver lo que ha durado y
    % se informe de ello por pantalla:
    on_exit(PID, fun (Reason) ->
        EndTime = get_time_stamp(),
        io:format("~w -> ~p => ~p~n", [PID, Reason, EndTime - StartTime])
    end),
    PID.

on_exit(PID, Handler) ->
    spawn(fun () ->
        % Configuramos el proceso para que monitorice al proceso lanzado:
        Reference = erlang:monitor(process, PID),
        receive
            % Si se recibe un mensaje de salida del proceso vigilado, se invocará a
            % la función que manejará el evento pasándole la razón de la finalización:
            {'DOWN', Reference, process, PID, Reason} ->
                Handler(Reason)
        end
    end).

%====================================================================================================
% 3. Escribir una función my spawn(Mod, Func, Args, Time) que se comporte como spawn(Mod, Func, Args),
%    pero con una diferencia: si el proceso generado vive más de Time segundos, este debe ser matado.
%====================================================================================================

my_spawn_3(Module, Function, Args, Time) ->
    % Calculamos el tiempo máximo de duración en milisegundos,
    % lanzamos el proceso y obtenemos una marca de tiempo:
    TimeInMs = Time * 1000,
    PID = spawn(Module, Function, Args),
    StartTime = get_time_stamp(),
    % Lanzamos el proceso que hará de monitor del anterior:
    spawn(fun () ->
        % Configuramos el proceso para que monitorice al proceso lanzado:
        Reference = erlang:monitor(process, PID),
        receive
            % Si se recibe un mensaje de salida del proceso vigilado, se obtendrá una
            % nueva marca de tiempo para ver lo que ha durado y se informará de ello:
            {'DOWN', Reference, process, PID, Reason} ->
                EndTime = get_time_stamp(),
                io:format("~w -> ~p => ~p~n", [PID, Reason, EndTime - StartTime])
        after TimeInMs ->
            % Si ha pasado el tiempo máximo de duración, se manda la señal de
            % finalización inmediata al proceso:
            io:format("Davros commands you to kill process ~w.~n", [PID]),
            io:format("EXTERMINATE! ANNIHILATE! DESTROY!~n", []),
            exit(PID, kill)
        end
    end),
    PID.

%====================================================================================================
% 4. Escribir una función que genere un proceso registrado que escriba "Estoy vivo" cada 5 segundos.
%    Escribir una función que monitorice este proceso y lo relance si muere.
%====================================================================================================

launch_4(DelayInSeconds) when is_number(DelayInSeconds), DelayInSeconds > 0 ->
    TimeDelay = DelayInSeconds * 1000,
    Launcher = fun InnerLauncher() ->
        % Lanzamos el proceso que nos va a estar dando la brasa por pantalla, en este
        % caso se trata de uno encargado de ejecutar la función de "cantar una
        % canción", poniendo cada línea después de un tiempo máximo en milisegundos:
        PID = spawn(fun _GLaDOS() ->
            sing_song(get_glados_song(), TimeDelay)
        end),
        % Adicionalmente tendremos registrado un monitor que relance el proceso
        % en caso de que termine antes de lo esperado:
        on_exit(PID, fun (_) ->
            NextPID = InnerLauncher(),
            io:format("GLaDOS is still alive in: ~w~n", [NextPID]),
            io:format("She can't be killed, sorry! ;)~n"),
            NextPID
        end),
        PID
    end,
    Launcher().

sing_song(Lines, TimeDelay) ->
    sing_song(Lines, Lines, TimeDelay).
sing_song(Current, Lines, TimeDelay) ->
    % Para mostar una canción tendremos que tener en cuenta lo que queda de la
    % misma, la canción entera y el tiempo máximo para lanzar la siguiente frase:
    receive after TimeDelay ->
        case Current of
            [] ->
                % Si no queda más líneas en la canción, retomamos toda la canción,
                % mostramos la primera línea y continuamos con el resto:
                [A|AS] = Lines,
                io:format("~s~n", [A]),
                sing_song(AS, Lines, TimeDelay);
            [B|BS] ->
                % Si queda más líneas, mostramos la actual y continuamos con el resto:
                io:format("~s~n", [B]),
                sing_song(BS, Lines, TimeDelay)
        end
    end.

get_glados_song() -> [
    "This was a triumph!", "I'm making a note here:", "\"Huge success!!\"",
    "It's hard to overstate", "My satisfaction.", "Aperture science:", "We do what me must",
    "Because we can.", "For the good of all of us.", "Except the ones who are dead.",
    "But there's no sense crying", "Over every mistake.", "You just keep on trying",
    "Till you run out of cake.", "And the science gets done.", "And you make a neat gun",
    "For the people who are", "Still alive.", "I'm not even angry...",
    "I'm being so sincere right now-", "Even though you broke my heart,",
    "And killed me.", "And tore me to pieces.", "And threw every piece into a fire.",
    "As they burned it hurt because", "I was so happy for you!", "Now, these points of data",
    "Make a beautiful line.", "And we're out of beta.", "We're releasing on time!",
    "So I'm glad i got burned-", "Think of all the things we learned-",
    "For the people who are", "Still alive.", "Go ahead and leave me...",
    "I think I'd prefer to stay inside...", "Maybe you'll find someone else",
    "To help you?", "Maybe black mesa?", "That was a joke! ha ha!! fat chance!!",
    "Anyway this cake is great!", "It's so delicious and moist!", "Look at me: still talking",
    "When there's science to do!", "When I look out there,", "'It makes me glad I'm not you!'",
    "I've experiments to run.", "There is research to be done.", "On the people who are",
    "Still alive.", "And believe me I am", "Still alive.", "I'm doing science and I'm",
    "Still alive.", "I feel fantastic and I'm", "Still alive.",
    "While you're dying I'll be", "Still alive.", "And when you're dead I will be",
    "Still alive.", "Still alive.", "Still alive.", "(End Of Song)"].

%====================================================================================================
% 5. Escribir una función que arranque y monitorice varios procesos trabajadores. Si alguno de
%    ellos muere anormalmente, debe relanzarlo.
%====================================================================================================

% Examples:
% spawn:launch_5([{spawn, test, [30]},{spawn, test, [3]}]).
% exit(pid(0,0,0), kill).

launch_5(Victims) ->
    % Esta función lanza una lista de invocaciones mediante una función
    % de spawn auxiliar creada en el módulo, devolviendo los PIDs y la
    % información utilizada para la invocación:
    Spawner = fun (VS) ->
        Aux = [{spawn_launcher(Item), Item} || Item <- VS],
        [{PID, Item} || {PID, Item} <- Aux, is_pid(PID)]
    end,
    % Esta función busca el primer elemento que tenga la referencia
    % indicada y lo devuelve junto a una nueva lista sin él:
    GetAndRemove = fun (Ref, Refs) ->
        Victim = hd([{R, Item} || {R, Item} <- Refs, Ref =:= R]),
        {Victim, lists:delete(Victim, Refs)}
    end,
    % Esta función simplemente comprueba si existe una referencia
    % indicada dentro de la lista de referencias que se maneja:
    IsInside = fun (Ref, Refs) ->
        lists:any(fun ({R, _}) -> Ref =:= R end, Refs)
    end,
    % Esta es la función que va a estar encargada de monitorizar
    % los procesos lanzados:
    Monitor = fun InnerMonitor(References) ->
        receive
            % Recibimos una señal de finalización normal del proceso:
            {'DOWN', Reference, process, PID, normal}  ->
                case IsInside(Reference, References) of
                    true ->
                        % En caso de ser un proceso registrado lo eliminamos de la
                        % lista actual, avisamos de su muerte y seguimos:
                        {_, NextRefs} = GetAndRemove(Reference, References),
                        io:format("Registered process ~w is finished.~n", [PID]),
                        InnerMonitor(NextRefs);
                    false ->
                        % Si no es un proceso registrado simplemente avisamos:
                        io:format("Process ~w is finished.~n", [PID]),
                        InnerMonitor(References)
                end;
            % Recibimos una señal de finalización anormal del proceso:
            {'DOWN', Reference, process, PID, Reason} ->
                case IsInside(Reference, References) of
                    true ->
                        % Eliminamos de la lista la referencia actual y relanzamos
                        % el proceso, para luego registrarlo a fin de monitorizarlo:
                        {{_, Item}, AuxRefs} = GetAndRemove(Reference, References),
                        NextPID = spawn_launcher(Item),
                        NextRefs = [{erlang:monitor(process, NextPID), Item} | AuxRefs],
                        % Avisamos del evento ocurrido y continuamos:
                        io:format("Registered process ~w finished by signal: ~w~n", [PID, Reason]),
                        io:format("Process re-launched and registered in: ~w~n", [NextPID]),
                        InnerMonitor(NextRefs);
                    false ->
                        % Si no es un proceso registrado simplemente avisamos:
                        io:format("Process ~w finished by signal: ~w~n", [PID, Reason]),
                        InnerMonitor(References)
                end;
            MSG ->
                % En caso de recibir un mensaje cualquiera avisamos del evento:
                io:format("Received invalid message: ~w~n", [MSG]),
                InnerMonitor(References)
        end
    end,
    % Lanzamos los procesos y el monitor que va a estar encargado de controlar
    % la ejecución de dichos procesos:
    PIDS = Spawner(Victims),
    spawn(fun () ->
        Monitor([{erlang:monitor(process, PID), Item} || {PID, Item} <- PIDS, is_pid(PID)])
    end),
    PIDS.

spawn_launcher({Module, Function, Args}) when is_atom(Module), is_atom(Function), is_list(Args) ->
    spawn(Module, Function, Args);
spawn_launcher(Function) when is_function(Function) ->
    spawn(Function);
spawn_launcher(_) ->
    ok.

%====================================================================================================
% 6. Escribir una función que arranque y monitorice varios procesos trabajadores. Si alguno de
%    ellos muere anormalmente, debe matar al resto de trabajadores y después relanzarlos todos.
%====================================================================================================

% Examples:
% spawn:launch_6([{spawn, test, [30]},{spawn, test, [20]},{spawn, test, [3]}]).
% exit(pid(0,0,0), kill).

launch_6(Victims) ->
    % Esta función lanza una lista de invocaciones mediante una función
    % de spawn auxiliar creada en el módulo, devolviendo los PIDs y la
    % información utilizada para la invocación:
    Spawner = fun (VS) ->
        Aux = [{spawn_launcher(Item), Item} || Item <- VS],
        [{PID, Item} || {PID, Item} <- Aux, is_pid(PID)]
    end,
    % Esta función se encarga de registrar una lista de procesos, para
    % iniciar su monitorización por parte de proceso invocador:
    Register = fun (PIDS) ->
        [{erlang:monitor(process, PID), PID, Item} || {PID, Item} <- PIDS, is_pid(PID)]
    end,
    % Esta función se encarga de eliminar la monitorización de una lista de
    % procesos, matarlos y devolver la información de invocación:
    UnregisterAndKill = fun (Refs) ->
        Aux1 = [{erlang:demonitor(Ref), PID, Item} || {Ref, PID, Item} <- Refs, is_reference(Ref)],
        Aux2 = [{exit(PID, kill), Item} || {_, PID, Item} <- Aux1],
        [Item || {_, Item} <- Aux2]
    end,
    % Esta función busca el primer elemento que tenga la referencia
    % indicada y lo devuelve junto a una nueva lista sin él:
    GetAndRemove = fun (Ref, Refs) ->
        Victim = hd([{R, PID, Item} || {R, PID, Item} <- Refs, Ref =:= R]),
        {Victim, lists:delete(Victim, Refs)}
    end,
    % Esta función simplemente comprueba si existe una referencia
    % indicada dentro de la lista de referencias que se maneja:
    IsInside = fun (Ref, Refs) ->
        lists:any(fun ({R, _, _}) -> Ref =:= R end, Refs)
    end,
    % Esta es la función que va a estar encargada de monitorizar
    % los procesos lanzados:
    Monitor = fun InnerMonitor(References) ->
        receive
            % Recibimos una señal de finalización normal del proceso:
            {'DOWN', Reference, process, PID, normal}  ->
                case IsInside(Reference, References) of
                    true ->
                        % En caso de ser un proceso registrado lo eliminamos de la
                        % lista actual, avisamos de su muerte y seguimos:
                        {_, NextRefs} = GetAndRemove(Reference, References),
                        io:format("Registered process ~w is finished.~n", [PID]),
                        InnerMonitor(NextRefs);
                    false ->
                        % Si no es un proceso registrado simplemente avisamos:
                        io:format("Process ~w is finished.~n", [PID]),
                        InnerMonitor(References)
                end;
            % Recibimos una señal de finalización anormal del proceso:
            {'DOWN', Reference, process, PID, Reason} ->
                case IsInside(Reference, References) of
                    true ->
                        % Matamos todos los procesos que quedan, los relanzamos
                        % y los volvemos a registrar para monitorizarlos:
                        NextRefs = Register(Spawner(UnregisterAndKill(References))),
                        % Avisamos del evento ocurrido y continuamos:
                        io:format("Registered process ~w finished by signal: ~w~n", [PID, Reason]),
                        io:format("All process re-launched and registered:~n~w~n", [NextRefs]),
                        InnerMonitor(NextRefs);
                    false ->
                        % Si no es un proceso registrado simplemente avisamos:
                        io:format("Process ~w finished by signal: ~w~n", [PID, Reason]),
                        InnerMonitor(References)
                end;
            MSG ->
                % En caso de recibir un mensaje cualquiera avisamos del evento:
                io:format("Received invalid message: ~w~n", [MSG]),
                InnerMonitor(References)
        end
    end,
    % Lanzamos los procesos y el monitor que va a estar encargado de controlar
    % la ejecución de dichos procesos:
    PIDS = Spawner(Victims),
    spawn(fun () -> Monitor(Register(PIDS)) end),
    PIDS.
