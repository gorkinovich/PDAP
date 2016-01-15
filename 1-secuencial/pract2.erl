%=============================%
% PDAP:   Práctica 2          %
% Alumno: Gorka Suárez García %
%=============================%

-module(pract2).
-export([impares/1, ack/2, mismoConjunto/2,
         normal/1, interseccion/2, esta/2,
         nNodos/1, mapTree/2, sonMultiplos/2,
         h/1]).

%--------------------------------------------------
% Apartado 1
%--------------------------------------------------

% p2:impares([a,b,c,d,e,f]). => [a,c,e]
% p2:impares([a,b]).         => [a]
% p2:impares([]).            => []

impares(XS) -> impares_imp(XS, 1).

impares_imp([], _) -> [];
impares_imp([X|XS], N) when N rem 2 /= 0 -> [X|impares_imp(XS, N + 1)];
impares_imp([_|XS], N) when N rem 2 == 0 -> impares_imp(XS, N + 1).

%--------------------------------------------------
% Apartado 2
%--------------------------------------------------

% p2:ack(3,4). -> 125

ack(M, N) when M == 0 -> N + 1;
ack(M, N) when M >= 0, N == 0 -> ack(M - 1, 1);
ack(M, N) when M >= 0, N >= 0 -> ack(M - 1, ack(M, N - 1)).

% NOTA: En el when usar la coma equivale a andalso y el
% punto y coma equivale a orelse.

%--------------------------------------------------
% Apartado 3
%--------------------------------------------------

% p2:mismoConjunto([1,3,4,4],[4,3,3,1,1]). -> true
% p2:mismoConjunto([1,3,4,4],[4,3,3]).     -> false

mismoConjunto(XS, YS) -> XS2 = normal(XS), YS2 = normal(YS),
    lists:all(fun(X) -> lists:member(X, YS2) end, XS2).

normal(XS) -> normal_imp(XS, []).

normal_imp([], YS) -> lists:reverse(YS);
normal_imp([X|XS], YS) -> 
    case lists:member(X, YS) of
        true  -> normal_imp(XS, YS);
        false -> normal_imp(XS, [X|YS])
    end.

interseccion(XS, YS) ->  XS2 = normal(XS), YS2 = normal(YS),
    lists:filter(fun(X) -> lists:member(X, YS2) end, XS2).

%--------------------------------------------------
% Apartado 4
%--------------------------------------------------

%isEmtpyNode({}) -> true;
%isEmtpyNode(_) -> false.

%isDataNode({_, _, _}) -> true;
%isDataNode(_) -> false.

%getNodeValue({Victim, _, _}) -> Victim.
%getLeftNode({_, Victim, _})  -> Victim.
%getRightNode({_, _, Victim}) -> Victim.

% p2:esta(3,{5,{3,{},{}},{6,{},{}}}).  -> true
% p2:esta(3,{6,{13,{},{}},{6,{},{}}}). -> false

esta(_, {}) -> false;
esta(Victim, {V, L, R}) -> Victim == V orelse
    esta(Victim, L) orelse esta(Victim, R);
esta(_, _) -> false.

% p2:nNodos({6,{13,{42, {}, {}},{}},{6,{},{0, {}, {}}}}).

nNodos({}) -> 0;
nNodos({_, L, R}) -> 1 + nNodos(L) + nNodos(R).

% p2:mapTree((fun(E)->E+1 end), {6, {5, {3,{},{}},{}},{}}).
% -> {7,{6,{4,{},{}},{}},{}}
% F = fun(E) when E rem 2 ==0 -> par; (E) when E rem 2==1 -> impar end.
% p2:mapTree(F, {6, {5, {3,{},{}}, {4,{},{}} }, {6, {}, {8, {}, {}}}}).
% -> {par,{impar,{impar,{},{}},{par,{},{}}},{par,{},{par,{},{}}}}

mapTree(_, {}) -> {};
mapTree(F, {Victim, L, R}) -> {F(Victim), mapTree(F, L), mapTree(F, R)}.

%--------------------------------------------------
% Apartado 5
%--------------------------------------------------

% p2:sonMultiplos(0,0). -> true
% p2:sonMultiplos(0,4). -> true
% p2:sonMultiplos(5,4). -> false
% p2:sonMultiplos(8,4). -> true
% p2:sonMultiplos(4,8). -> true

sonMultiplos(X, Y) when X == 0; Y == 0 -> true;
sonMultiplos(X, Y) -> ((X rem Y) == 0) orelse ((Y rem X) == 0).

%--------------------------------------------------
% Apartado 6
%--------------------------------------------------

% f(), A = p2:h(8), A(16). -> true
% f(), A = p2:h(8), A(15). -> false
% f(), A = p2:h(8), A(0).  -> true
% f(), (p2:h(8))(4).       -> true

h(X) -> fun(Y) -> sonMultiplos(X, Y) end.
