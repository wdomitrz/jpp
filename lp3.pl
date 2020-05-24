% 1.
% a) drzewo(D) wtw, gdy D jest drzewem binarnym
drzewo(nil).
drzewo(wezel(L, _, R)) :-
    drzewo(L),
    drzewo(R).
% b) insertBST(DrzewoBST, Elem, NoweDrzewoBST)
insertBST(nil, X, wezel(nil, X, nil)).
insertBST(wezel(L, Y, R), X, wezel(NL, Y, R)) :-
    Y>=X, !,
    insertBST(L, X, NL).
insertBST(wezel(L, Y, R), X, wezel(L, Y, NR)) :-
    X>Y,
    insertBST(R, X, NR).
% d) wypiszBST(D, L) wtw, gdy L=lista wszystkich wierzchołków D (porządek infiksowy)
wypiszBST(T, S) :-
    wypiszBST(T, [], S).
wypiszBST(nil, A, A).
wypiszBST(wezel(L, X, R), A, S) :-
    wypiszBST(R, A, SR),
    wypiszBST(L, [X|SR], S).
% e) stworzBST(L, D) wtw, gdy D jest drzewem BST zawierającym wszystkie elementy listy L (akumulator, ew. bez)
stworzBST(S, T) :-
    stworzBST(S, nil, T).
stworzBST([], A, A).
stworzBST([X|S], A, T) :-
    insertBST(A, X, AT),
    stworzBST(S, AT, T).
% f) liscie(D, L) wtw, gdy L = lista wszystkich liści, od lewej do prawej
liscie(T, S) :-
    liscie(T, [], S).
liscie(nil, A, A).
liscie(wezel(nil, X, nil), A, [X|A]) :- !.
liscie(wezel(L, _, R), A, S) :-
    (L, R)\=(nil, nil),
    liscie(R, A, AR),
    liscie(L, AR, S).
% g) sortBST(L, S) wtw, gdy S = lista posortowana, przy użyciu drzew BST
sortBST(L, S) :-
    stworzBST(L, T),
    wypiszBST(T, S).

% 2.
% Expamples of graphs
graf_1([kr(a, b), kr(a, c), kr(a, d), kr(b, e), kr(c, e)]).
graf_2([kr(a, b), kr(b, c), kr(c, a), kr(c, d), kr(d, a)]).

edge_1(a, b).
edge_1(a, c).
edge_1(a, d).
edge_1(b, e).
edge_1(c, e).

edge_2(a, b).
edge_2(b, c).
edge_2(c, a).
edge_2(c, d).
edge_2(d, a).

% Choosen graph
edge(A, B) :-
    edge_2(A, B).

% a) connect(A,B), connect(Graf,A,B) wtw, gdy istnieje ścieżka z A do B.
% Uwaga: ścieżka = niepusty (!) ciąg krawędzi
% Here we consider DAGs
connect(A, B) :-
    edge(A, B).
connect(A, B) :-
    edge(A, C),
    connect(C, B).

connect(G, A, B) :-
    member(kr(A, B), G).
connect(G, A, B) :-
    member(kr(A, C), G),
    connect(G, C, B).
% b) path(A,B,P) wtw, gdy P = opis ścieżki z A do B, tzn. P = [A, ..., B]
% Here we consider DAGs
path(A, B, [A, B]) :-
    edge(A, B).
path(A, B, [A|P]) :-
    edge(A, C),
    path(C, B, P).

path(A, B, [A, B], G) :-
    member(kr(A, B), G).
path(A, B, [A|P], G) :-
    member(kr(A, C), G),
    path(C, B, P, G).
% c) pathC(A,B,P) w dowolnym grafie skierowanym (cyklicznym)
pathC(A, B, P) :-
    pathC(A, B, P, []).
pathCVis(A, B, [A, B], V) :-
    \+ member(A, V), % we only accept the paths without repeating vertices
                     % (including the first and the last vertices).
    \+ member(B, V),
    edge(A, B).
pathCVis(A, B, [A|P], V) :-
    edge(A, C),
    \+ member(C, V),
    pathCVis(C, B, P, [A|V]).

pathC(A, B, P, G) :-
    pathCVis(A, B, P, [], G).
pathCVis(A, B, [A, B], V, G) :-
    \+ member(A, V), % we only accept the paths without repeating vertices
                     % (including the first and the last vertices).
    \+ member(B, V),
    member(kr(A, B), G).
pathCVis(A, B, [A|P], V, G) :-
    member(kr(A, C), G),
    \+ member(C, V),
    pathCVis(C, B, P, [A|V], G).

% Inconsistency in the following solution:
path_c(G, A, B, P) :-
     path_c(G, [], A, B, P).
path_c(G, _, A, B, [A, B]) :-
     member(kr(A, B), G).
path_c(G, V, A, B, [A | P]) :-
     member(kr(A, C), G),
     \+member(C, V),
     path_c(G, [A | V], C, B, P).

graf_3a([kr(a, b), kr(b, c), kr(c, d), kr(d, b)]).
graf_3b([kr(b, a), kr(c, b), kr(d, c), kr(b, d)]).
% graf_3a(G), path_c(G, a, b, [a, b, c, d, b]). - success
% graf_3b(G), path_c(G, b, a, [b, d, c, b, a]). - failure

% d) euler/? - czy dany graf jest grafem Eulera, czyli znalezienie (sprawdzenie) ścieżki Eulera (wprost z definicji):
% ścieżka, która przechodzi przez każdą krawędź grafu dokładnie raz
euler([kr(A, B)], [A, B]).
euler(G, [A, B|P]) :-
    select(kr(A, B), G, GB), % repleced remove_euler with select
    euler(GB, [B|P]).
