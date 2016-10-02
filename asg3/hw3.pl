/** 
* Authors: Yunyi Ding and Brian Lin
*/

father(al, bud).
father(al, kelly).
mother(peggy, kelly).
mother(peggy, bud).
mother(martha, peggy).

grandma(X, Y) :- mother(Z, Y), mother(X, Z).

descendants(X, Y) :- mother(X, Y); father(X, Y); grandma(X, Y).

siblings(X, Y) :- mother(M, X), mother(M, Y), father(F, X), father(F, Y), X \= Y.

