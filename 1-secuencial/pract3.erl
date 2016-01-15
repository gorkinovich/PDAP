%=============================%
% PDAP:   Práctica 3          %
% Alumno: Gorka Suárez García %
%=============================%

-module(pract3).
-export([lexico/1, solutions/3, eval/1, amistad/2]).

%--------------------------------------------------
% Apartado 1
%--------------------------------------------------

% [2] 1) Supongamos que L es una lista de parejas de números
% {X,Y}. Excribe una función lexico/1 que devuelve la pareja
% menor siguiendo el órden lexicogŕafico. En este orden
% {A,B} <= {A',B'} cuando A<A o (A=A' y B<=B')

lexico(L) ->
    case lexico_check(L) of
        true -> [X|XS] = L, lexico_imp(XS, X);
        _ -> invalid_input
    end.

lexico_check([X|XS]) ->
    R = lexico_check_list([X|XS]),
    io:format("lexico_check([X|XS]) => R: ~w.~n", [R]),
    R;
lexico_check(_) ->
    false.

lexico_check_list([]) ->
    true;
lexico_check_list([X|XS]) ->
    case is_tuple(X) andalso lexico_check_tuple(X)
         andalso lexico_check_list(XS) of
        true ->
            io:format("lexico_check_list([X|XS]) => Everything ok.~n"),
            true;
        _ ->
            MSG01 = "lexico_check_list([X|XS]) => Current list: ~w.~n",
            MSG02 = "lexico_check_list([X|XS]) => Is tuple: ~w.~n",
            MSG03 = "lexico_check_list([X|XS]) => Is tuple of numbers: ~w.~n",
            io:format(MSG01, [[X|XS]]),
            io:format(MSG02, [is_tuple(X)]),
            io:format(MSG03, [lexico_check_tuple(X)]),
            false
    end.

lexico_check_tuple({A, B}) ->
    is_number(A) andalso is_number(B);
lexico_check_tuple(_) ->
    false.

lexico_imp([], Victim) ->
    Victim;
lexico_imp([X|XS], Victim) ->
    case lexico_less_than(X, Victim) of
        true ->
            MSG01 = "lexico_imp([X|XS], Victim) => ~w, ~w -> ~w.~n",
            io:format(MSG01, [X, Victim, X]),
            lexico_imp(XS, X);
        _ ->
            MSG02 = "lexico_imp([X|XS], Victim) => ~w, ~w -> ~w.~n",
            io:format(MSG02, [X, Victim, Victim]),
            lexico_imp(XS, Victim)
    end.

lexico_less_than(Left, Right) ->
    {LA, LB} = Left,
    {RA, RB} = Right,
    (LA < RA) orelse ((LA == RA) andalso (LB < RB)).

%--------------------------------------------------
% Apartado 2
%--------------------------------------------------

% [2] 2) Escribir una función solutions/3 que recibe
% como entrada:
%   - Una función F con dos parámetros enteros
%   - Un entero Min
%   - Un entero Max
% y devuelve todas los valores {X,Y} que son soluciones
% enteros de F en el intervalo [Min,Max]x[Min,Max], es
% decir tales que Min <= X,Y <= Max, F(X,Y)=0
% Nota: Hace falta utilizar listas intensionales
% Ejemplo para probar:
%   prac3:solutions(fun(X,Y)->X*X+Y*Y-16 end, -20,20).
%   [{-4,0},{0,-4},{0,4},{4,0}]

solutions(Func, Min, Max) ->
    case solutions_check(Func, Min, Max) of
        true -> [{X, Y} || X <- lists:seq(Min, Max), (Min =< X) and (X =< Max),
            Y <- lists:seq(Min, Max), (Min =< Y) and (Y =< Max), Func(X, Y) == 0];
        _ -> invalid_input
    end.

solutions_check(Func, Min, Max) ->
    is_function(Func) andalso is_number(Min) andalso
    is_number(Max) andalso (Min =< Max).

%--------------------------------------------------
% Apartado 3
%--------------------------------------------------

% [2] 3) Tenemos expresiones aritméticas de la forma: {int,5}
% {op,expr1,expr2} donde op puede ser cualquiera de los átomos
% suma, resta, multiplica o divide. Escribe una función eval
% que evalúe una expresión de este tipo. Si se produce un
% error de formato (operación no permitida) o una división
% por 0 debe devolver el átomo error.
% Ejemplos para probar:
%   prac3:eval({suma,{multiplica,{int,5},{int,6}}, {int,2}}).
%   32
%   prac3:eval({suma,{multiplica,{intirititi,5},{int,6}}, {int,2}}).
%   Error

eval({int, Victim}) ->
    Victim;
eval({suma, Left, Right}) ->
    eval_exec_op(fun(X,Y) -> X+Y end, Left, Right);
eval({resta, Left, Right}) ->
    eval_exec_op(fun(X,Y) -> X-Y end, Left, Right);
eval({multiplica, Left, Right}) ->
    eval_exec_op(fun(X,Y) -> X*Y end, Left, Right);
eval({divide, Left, Right}) ->
    eval_exec_op(fun(X,Y) -> X/Y end, Left, Right);
eval(_) ->
    error.

eval_exec_op(Func, Left, Right) ->
    VL = eval(Left), VR = eval(Right),
    case (VL /= error) andalso (VR /= error) of
        true -> Func(VL, VR);
        _ -> error
    end.

%--------------------------------------------------
% Apartado 4
%--------------------------------------------------

% [4] 4) Dada una lista de personas, donde cada persona
% es una tupla de la forma {edad, genero, aficiones}
% Elaborar un sistema de búsqueda de amistad con las
% siguientes características:
%   amistad(L, FiltroGenero), donde
%     - L es una lista de personas
%     - FiltroGenero puede ser uno de los siguientes
%       + mismo: indica que se busca amistad entre persona
%         del mismo genero
%       + diferente: indica que se busca amistad entre
%         personas de genero distinto
%       + indiferente: el filtro nulo, se busca amistad
%         sin depender del género
% La función devolverá una lista de pareja de personas
% que pueden ser amigas, que incluye a las parejas
% que verifican el FiltroGenero y que además
%   + Tienen una afición en común, o,
%   + Tienen ambos edades que dentro del segmento [M-10,M+10],
%     con M a la media de las edades en la lista.
%   + Tienen ambos edades fuera del segmento [M-10,M+10]

% Ejemplo:
% p3:amistad([{20,hombre,a}, {35,mujer,[b,c]},
%   {24,hombre,[c,d]}, {36,transgenero,[a,e]},
%   {46,mujer,d}, {34,hombre,[b,c]},
%   {50,mujer,[a,b,c,d,e]}], diferente).
% Salida:
% [{{20,hombre,a},{36,transgenero,[a,e]}},
%  {{35,mujer,[b,c]},{24,hombre,[c,d]}},
%  {{34,hombre,[b,c]},{50,mujer,[a,b,c,d,e]}}]

amistad(L, FiltroGenero) ->
    M = lists:sum([E || {E, _, _} <- L]) / length(L),
    amistad_emparejar(FiltroGenero, M, L).

amistad_emparejar(_, _, []) ->
    [];
amistad_emparejar(F, M, [X|XS]) ->
    {V, XS2} = amistad_encontrar(F, M, X, XS, []),
    case V of
       nadie_te_quiere -> amistad_emparejar(F, M, XS);
       _ -> [{X, V} | amistad_emparejar(F, M, XS2)]
    end.

amistad_encontrar(_, _, _, [], R) ->
    {nadie_te_quiere, lists:reverse(R)};
amistad_encontrar(F, M, V, [X|XS], R) ->
    case amistad_test(F, M, V, X) of
        true -> {X, lists:reverse(R) ++ XS};
        _ -> amistad_encontrar(F, M, V, XS, [X|R])
    end.

amistad_test(F, M, V1, V2) ->
    {VE1, VG1, VA1} = V1,
    {VE2, VG2, VA2} = V2,
    amistad_genero(F, VG1, VG2) andalso
    (amistad_aficiones(VA1, VA2) orelse
    ((abs(M - VE1) =< 10) andalso (abs(M - VE2) =< 10)) orelse
    ((abs(M - VE1) >= 10) andalso (abs(M - VE2) >= 10))).

amistad_genero(mismo, G1, G2) -> G1 == G2;
amistad_genero(diferente, G1, G2) -> G1 /= G2;
amistad_genero(indiferente, _, _) -> true.

amistad_aficiones(A1, A2) when is_list(A1), is_list(A2) ->
    [0 || X <- A1, Y <- A2, X == Y] /= [];
amistad_aficiones(A1, A2) when is_list(A1) ->
    [0 || X <- A1, X == A2] /= [];
amistad_aficiones(A1, A2) when is_list(A2) ->
    [0 || Y <- A2, A1 == Y] /= [];
amistad_aficiones(A1, A2) ->
    A1 == A2.
