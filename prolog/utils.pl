:- module(utils, [product/2]).
acc_product([], Acc, Acc).
acc_product([Head|Tail], Acc, Result) :- X is Acc * Head, acc_product(Tail, X, Result).

product(L, Result) :- acc_product(L, 1, Result).
