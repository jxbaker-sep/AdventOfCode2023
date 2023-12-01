:- set_prolog_flag(double_quotes, chars).

is_digit(X) => member(X, ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']).

number(X) --> digit(X).
number(X) --> number(Z), digit(D), {
  X is Z * 10 + D
}.
digit(X) --> [N], { is_digit(N), atom_number(N, X) }.

nondigit --> [N], { \+ is_digit(N) }.

nondigits --> nondigit, nondigits.
nondigits --> [].

lazy_nondigits --> [].
lazy_nondigits --> nondigit, lazy_nondigits.

anything --> [].
anything --> [_], anything.

greedy_anything --> [_], greedy_anything.
greedy_anything --> [].
