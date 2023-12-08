:- module(utils, [product/2, lcm_list/2]).

product(L, Result) :- foldl([A,B,C]>>(C is A * B), L, 1, Result).

lcm_list(L, Result) :- foldl([A,B,C]>>(C is lcm(A,B)), L, 1, Result).