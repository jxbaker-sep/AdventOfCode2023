:- module(parse_utils, [word/3, letter/3, number/3, digit/3, nondigit/2, nondigits/2, lazy_nondigits/2, anything/2, greedy_anything/2, ws/2, everything/3, read_datafile_to_lines/2]).
:- set_prolog_flag(double_quotes, chars).
is_digit(X) => member(X, ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']).

remainder(List, List, []).

word(W) --> letter(W).
word(W) --> letter(C), word(W2), {atomic_list_concat([C,W2], W)}.
letter(C) --> [C], { 
  member(C, "ABCDEFGHIJKLMNOPQRSZTUVWXYZabcdefghijklmnopqrstuvwxyz") 
}.

number(X) --> number_(Y), { atomic_list_concat(Y, Atom), atom_number(Atom, X) }.

number_([X]) --> digit_(X).
number_([D|Z]) --> digit_(D), number_(Z).
digit_(C) --> [C], { is_digit(C) }.
digit(X) --> digit_(C), { atom_number(C, X) }.

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

read_file_to_lines(Spec, Lines) :-
  read_file_to_string(Spec, String, []), 
  split_string(String, '\n', "", Lines).

read_datafile_to_lines(Spec, Lines) :- 
  atomic_list_concat(['../../../AdventOfCode2023.Data/', Spec, '.txt'], X),
  read_file_to_lines(X, Lines2),
  maplist(atom_chars, Lines2, Lines).