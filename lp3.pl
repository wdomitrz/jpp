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
liscie(wezel(nil, X, nil), A, [X|A]).
liscie(wezel(L, _, R), A, S) :-
    \+ ( L=nil,
         R=nil
       ),
    liscie(R, A, AR),
    liscie(L, AR, S).
% g) sortBST(L, S) wtw, gdy S = lista posortowana, przy użyciu drzew BST
sortBST(L, S) :-
    stworzBST(L, T),
    wypiszBST(T, S).
