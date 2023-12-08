:- module(utils, [product/2, lcm_list/2, zip/3]).

product(L, Result) :- foldl([A,B,C]>>(C is A * B), L, 1, Result).

lcm_list(L, Result) :- foldl([A,B,C]>>(C is lcm(A,B)), L, 1, Result).

zip([], [], []).
zip([A|T], [A2|T2], [[A,A2]|Tail]) :-
  zip(T, T2, Tail).