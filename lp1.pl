% dziecko(Dziecko, Matka, Ojciec).
dziecko(jasio, ewa, jan).
dziecko(stasio, ewa, jan).
dziecko(basia, anna, piotr).
dziecko(jan, ela, jakub).

% 1.
% a.
% Is Jasio a child of Eva and Jan?.
% ?- dziecko(jasio, ewa, jan).
% Find children of Eva and Jan.
% ?- dziecko(Child, ewa, jan).
% Find children of Eva.
% ?- dziecko(Child, ewa, _).
% Find pairs child-father.
% ?- dziecko(Child, _, Father).

% b.
% ojciec(Ojciec, Dziecko).
% matka(Matka, Dziecko).
% rodzic(Rodzic, Dziecko).
% babcia(Babcia, Dziecko) (ew. dziadek).
% wnuk(Wnuk/Wnuczka, Babcia/Dziadek).
% przodek(Przodek, Potomek).
ojciec(Ojciec, Dziecko) :- dziecko(Dziecko, _, Ojciec).
matka(Matka, Dziecko) :- dziecko(Dziecko, Matka, _).
rodzic(Ojciec, Dziecko) :- ojciec(Ojciec, Dziecko).
rodzic(Matka, Dziecko) :- matka(Matka, Dziecko).
babcia(Babcia, Wnuk) :- matka(Babcia, Rodzic), rodzic(Rodzic, Wnuk).
dziadek(Dziadek, Wnuk) :- ojciec(Dziadek, Rodzic), rodzic(Rodzic, Wnuk).
wnuk(Wnuk, Dziadek) :- rodzic(Dziadek, Rodzic), rodzic(Rodzic, Wnuk).
przodek(Rodzic, Dziecko) :- rodzic(Rodzic, Dziecko).
przodek(Przodek, Potomek) :- rodzic(Rodzic, Potomek), przodek(Przodek, Rodzic).
% slower version.
% przodek(Przodek, Potomek) :- przodek(Dziecko, Potomek),
%     rodzic(Przodek, Dziecko).

% 2.
% dziecko(Dziecko, Matka, Ojciec, płeć).
dziecko(jasio, ewa, jan, ch).
dziecko(stasio, ewa, jan, ch).
dziecko(basia, anna, piotr, dz).
dziecko(jan, ela, jakub, ch).
% syn(Dziecko, Matka, Ojciec).
% corka(Dziecko, Matka, Ojciec).
% dziecko(Dziecko, Matka, Ojciec).
% wnuczka(Dziecko, Babcia/Dziadek) lub wnuk(Dziecko, Babcia/Dziadek).
syn(Dziecko, Matka, Ojciec) :- dziecko(Dziecko, Matka, Ojciec, ch).
corka(Dziecko, Matka, Ojciec) :- dziecko(Dziecko, Matka, Ojciec, dz).
dziecko_v2(Dziecko, Matka, Ojciec) :- dziecko(Dziecko, Matka, Ojciec, _).
wnuczka(Dziecko, Dziadek) :- corka(Dziecko, Matka, _),
    dziecko_v2(Matka, _, Dziadek).
wnuczka(Dziecko, Dziadek) :- corka(Dziecko, _, Ojciec),
    dziecko_v2(Ojciec, _, Dziadek).
wnuczka(Dziecko, Babcia) :- corka(Dziecko, Matka, _),
    dziecko_v2(Matka, Babcia, _).
wnuczka(Dziecko, Babcia) :- corka(Dziecko, _, Ojciec),
    dziecko_v2(Ojciec, Babcia, _).

% 3.
% Natural numbers - z and s/1.
% a) nat(x) wtw, gdy x jest liczbą naturalną
nat(z).
nat(s(N)) :- nat(N).
% b) plus(x, y, z) wtw, gdy x + y = z
plus1(z, X, X).
plus1(s(X), Y, Z) :- plus1(X, s(Y), Z).
% c) minus(x, y, z) wtw, gdy x - y = z
minus1(X, Y, Z) :- plus1(Y, Z, X).
% d) fib(k, n) wtw, gdy n = k-ta liczba Fibonacciego
% fib_acc(k, f_k, f_{k+1})
fib_acc(z, z, s(z)).
fib_acc(s(K), Fk1, Fk2) :- fib_acc(K, Fk0, Fk1), plus1(Fk1, Fk0, Fk2).
fib(K, Fk) :- fib_acc(K, Fk, _).
% slower version
% fib(z, z).
% fib(s(z), s(z)).
% fib(s(s(K)), Fk2) :- fib(s(K), Fk1), fib(K, Fk0), plus1(Fk1, Fk0, Fk2).

% 4.
% a) lista(L) wtw, gdy L jest (prologową) listą
lista([]).
lista([_ | L]) :- lista(L).
% b) pierwszy(E, L) wtw, gdy E jest pierwszym elementem L
pierwszy(X, [X | _]).
% c) ostatni(E, L) wtw, gdy E jest ostatnim elementem L
ostatni(X, [X]).
ostatni(X, [_ | L]) :- ostatni(X, L).
% d) element(E, L) wtw, gdy E jest (dowolnym) elementem L
%    (czyli member/2)
element(E, [E | _]).
element(E, [_ | L]) :- element(E, L).
% e) scal(L1, L2, L3) wtw, gdy L3 = konkatenacja listy L1 z L2
%    (czyli append/3);
%    porównać z definicją funkcji ++ w Haskellu,
%    podać (wiele) zastosowań procedury scal/3
scal([], L1, L1).
scal([X | L1], L2, [X | L3]) :- scal(L1, L2, L3).
% e') intersect(Z1,Z2) wtw, gdy zbiory (listy) Z1 i Z2 mają niepuste przecięcie
intersect(L1, L2) :- element(X, L1), element(X, L2).
% f) podziel(Lista, NieParz, Parz) == podział danej listy na dwie
%    podlisty zawierające kolejne elementy (odpowiednio) z parzystych
%    (nieparzystych) pozycji
%    (np. podziel([1,3,5,7,9], [1,5,9], [3,7]) - sukces)
podziel([], [], []).
podziel([X | L1], [X | L2], L3) :- podziel(L1, L3, L2).
% g) podlista(P, L) wtw, gdy P jest spójną podlistą L
prefix([], _).
prefix([X | L1], [X | L2]) :- prefix(L1, L2).
nonepty_sublist([X | L1], L2) :- prefix([X | L1], L2).
nonepty_sublist(L1, [_ | L2]) :- nonepty_sublist(L1, L2).
podlista([], _).
podlista(L1, L2) :- nonepty_sublist(L1, L2).
% h) podciag(P, L)  wtw, gdy P jest podciągiem L
%    (czyli niekoniecznie spójną podlistą)
%    (preferowane rozwiązanie: każdy podciąg wygenerowany jeden raz)
podciag([], _).
podciag([X | L1], [X | L2]) :- podciag(L1, L2).
podciag([X | L1], [_ | L2]) :- podciag([X | L1], L2). % We need to be sure that
    % first list is not empty to not generate subsequences multiple times.
% i) wypisz(L) == czytelne wypisanie elementów listy L, z zaznaczeniem
%    jeśli lista pusta (np. elementy oddzielane przecinkami, po
%    ostatnim elemencie kropka)
wypisz(L) :- write('['), show_list_elems(L), write(']').
show_list_elems([]).
show_list_elems([X | []]) :- !, write(X).
show_list_elems([X | L]) :- write(X), write(', '), show_list_elems(L).
% Version without !
% show_list_elems([]).
% show_list_elems([X | L]) :- write(X), show_list_elems_with_commas(L).
% show_list_elems_with_commas([]).
% show_list_elems_with_commas([X | L]) :-
%     write(', '),
%     write(X),
%     show_list_elems_with_commas(L).
% j) sortowanie przez wstawianie:
%      insertionSort(Lista, Posortowana),
%      insert(Lista, Elem, NowaLista)
insertionSort(L, Res) :- insertionSortHelper(L, [], Res).
insertionSortHelper([], Acc, Acc).
insertionSortHelper([X | L], Acc, Res) :- insert(Acc, X, NewAcc),
    insertionSortHelper(L, NewAcc, Res).
insert([], E, [E]).
insert([X | L1], E, [E, X | L1]) :- X >= E.
insert([X | L1], E, [X | L2]) :- X < E, insert(L1, E, L2).
% k) zadanie domowe:
%       srodek(E, L) wtw, gdy E jest środkowym elementem L
%       (lista nieparzystej długości; np. srodek(3,[1,2,3,4,5]))
%    Uwagi:
%      - w tym zadaniu nie używamy jeszcze arytmetyki (nie trzeba)
%      - możliwe rozwiązania zarówno deklaratywne, jak i imperatywne (tylko jako
%        ćwiczenie) o dużym koszcie
%      - poszukiwane rozwiązanie o koszcie liniowym.
srodek(E, L) :- mid_helper(E, L, L).
mid_helper(X, [X | _], [_]).
mid_helper(X, [_ | L1], [_ | [_ | L2]]) :- mid_helper(X, L1, L2).
