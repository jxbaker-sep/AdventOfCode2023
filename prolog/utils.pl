:- module(utils, [product/2, lcm_list/2]).
acc_product([], Acc, Acc).
acc_product([Head|Tail], Acc, Result) :- X is Acc * Head, acc_product(Tail, X, Result).

product(L, Result) :- acc_product(L, 1, Result).

lcm_list_accum([], Accum, Accum).
lcm_list_accum([A|T], Accum, Result) :- 
  X is lcm(A,Accum),
  lcm_list_accum(T, X, Result).

lcm_list(L, Result) :- lcm_list_accum(L, 1, Result).