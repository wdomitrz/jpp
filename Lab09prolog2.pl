% a) suma(L, S) wtw, gdy S = suma elementów listy L
suma(L, S) :- suma(L, 0, S).
suma([], A, A).
suma([X | L], A, S) :- AX is A + X, suma(L, AX, S).
% b) dlugosc(L, K) wtw, gdy K = liczba elementów listy L (length/2)
dlugosc(L, K) :- dlugosc(L, 0, K).
dlugosc([], A, A).
dlugosc([_ | L], A, K) :- A1 is A + 1, dlugosc(L, A1, K).
% c) min(L, M) wtw, gdy M jest minimalnym elementem L (L = lista np. liczb całkowitych)
min1([X | L], M) :- min1(L, X, M).
min1([], A, A).
min1([X | L], A, M) :- AX is min(A, X), min1(L, AX, M).
% d) odwroc(L, R) wtw, gdy R jest odwróconą listą L (np. odwroc([1,2,3,4], [4,3,2,1]) - sukces)
odwroc(L, R) :- odwroc(L, [], R).
odwroc([], A, A).
odwroc([X | L], A, R) :- odwroc(L, [X | A], R).
% e) palindrom(Slowo) wtw, gdy (lista) Slowo jest palindromem (np. palindrom([k,a,j,a,k]), palindrom([1,b,2,b,1]) - sukcesy)
palindrom(S) :- odwroc(S, S). % no need to rewrite odwroc
% f) slowo(Slowo) == Slowo = a^n b^n
% (Uwaga: bez arytmetyki!) dwa warianty: (*) n > 0 (**) n >= 0 (np. slowo([a,a,b,b]) - sukces)
slowo_v1(L) :- slowo_v1(L, []).
slowo_v1([a | L], A) :- !, slowo_v1(L, [b | A]).
slowo_v1([b | A], [b | A]).
slowo_v2(L) :- slowo_v2(L, []).
slowo_v2([a | L], A) :- !, slowo_v2(L, [b | A]).
slowo_v2(A, A).
% g) slowo(Zdanie, Reszta) == Zdanie = Slowo * Reszta, Slowo - jw. (np. slowo([a,a,b,b,c,d], [c,d]) - sukces)
slowo_g([a | Z], R) :- slowo_g(Z, [b | R]).
slowo_g(R, R).
% Slower, but shorter version
% slowo_g_slow(Z, R) :- append(A, R, Z), slowo_v2(A).
% h) flagaPolska(Lista, Flaga) wtw, gdy Flaga jest posortowaną listą Lista, złożoną ze stałych b,c
% (np. flagaPolska([b,c,b,c], [b,b,c,c]) - sukces)
flagaPolska(L, F) :- flagaPolska(L, [], F).
flagaPolska([b | L], A, [b | F]) :- flagaPolska(L, A, F).
flagaPolska([c | L], A, F) :- flagaPolska(L, [c | A], F).
flagaPolska([], A, A).
% i) ew. flagaHolenderska(ListaRWB, RWB) (flaga: red-white-blue)
flagaHolenderska(L, RWB) :- flagaHolenderska(L, A, A, RWB).
flagaHolenderska([w | L], AW, AB, RWB) :- flagaHolenderska(L, [w | AW], AB, RWB).
flagaHolenderska([b | L], AW, [b | AB], RWB) :- flagaHolenderska(L, AW, AB, RWB).
flagaHolenderska([r | L], AW, AB, [r | RWB]) :- flagaHolenderska(L, AW, AB, RWB).
flagaHolenderska([], A, [], A).
% j) quickSort(L, S) wtw, gdy S jest wynikiem sortowania L (algorytm QuickSort)
split([], _, [], []).
split([Y | L], X, [Y | LTX], RTX) :- X >= Y, !, split(L, X, LTX, RTX).
split([Y | L], X, LTX, [Y | RTX]) :- Y > X, split(L, X, LTX, RTX).
%     wersja bez akumulatora
quickSort_noacc([], []).
quickSort_noacc([X | L], S) :-
    split(L, X, LTX, GTX),
    quickSort_noacc(LTX, LTXS),
    quickSort_noacc(GTX, GTXS),
    append(LTXS, [X | GTXS], S).
%     wersja z akumulatorem (czyli bez append)
quickSort(L, S) :- quickSort(L, [], S).
quickSort([], A, A).
quickSort([X | L], A, S) :-
    split(L, X, LTX, GTX),
    quickSort(GTX, A, GTXS),
    quickSort(LTX, [X | GTXS], S).
% k) flatten(L, F) wtw, gdy L jest zagnieżdżoną listą list, których elementami są liczby całkowite, a F jest spłaszczoną listą L (np. flatten([1,[[[[2,[3]]], 4], 5]], [1,2,3,4,5]) - sukces)
flatten(L, F) :- flatten(L, [], F).
flatten([], A, A) :- !.
flatten([X | L], A, F) :- !,
    flatten(L, A, LA),
    flatten(X, LA, F).
flatten(X, A, [X | A]) :- integer(X).
