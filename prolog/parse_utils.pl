:- set_prolog_flag(double_quotes, chars).
is_digit(X) => member(X, ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']).

remainder(List, List, []).

word(W) --> letter(W).
word(W) --> letter(C), word(W2), {atomic_list_concat([C,W2], W)}.
letter(C) --> [C], { 
  member(C, "ABCDEFGHIJKLMNOPQRSZTUVWXYZabcdefghijklmnopqrstuvwxyz") 
}.

number(X) --> -number(Y), { atomic_list_concat(Y, Atom), atom_number(Atom, X) }.

-number([X]) --> -digit(X).
-number([D|Z]) --> -digit(D), -number(Z).
-digit(C) --> [C], { is_digit(C) }.
digit(X) --> -digit(C), { atom_number(C, X) }.

nondigit --> [N], { \+ is_digit(N) }.

nondigits --> nondigit, nondigits.
nondigits --> [].

lazy_nondigits --> [].
lazy_nondigits --> nondigit, lazy_nondigits.

anything --> [].
anything --> [_], anything.

greedy_anything --> [_], greedy_anything.
greedy_anything --> [].

ws --> [' '].
ws --> [' '], ws.

everything([]) --> [].
everything([S|T]) --> [S], everything(T).