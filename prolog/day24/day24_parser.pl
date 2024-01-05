% :- module(day8_parser, [use_sample/0, use_data/0, use_sample2/0, right/2, left/2, instructions/1]).
:- set_prolog_flag(double_quotes, chars).
% :- ['../parse_utils'].

s(Head, Left, Right) --> everything(Head2), " = (", everything(Left2), ", ", everything(Right2), ")",
  {atom_chars(Head, Head2), atom_chars(Left, Left2), atom_chars(Right, Right2)}.

inject_rows([]).
inject_rows([A|B]) :-
  phrase(s(Head, Left, Right), A), !,
  assert(( left(Head, Left) )),
  assert(( right(Head, Right) )),
  inject_rows(B).


assert_instructions(I) :-
  assert(( instructions(I) )).

retract =>
  retractall( instructions(_) ),
  retractall( right(_,_) ),
  retractall( left(_,_) ).


use([I,[]|Tail]) =>
  retract,
  assert_instructions(I),
  inject_rows(Tail).

sample(Data) :- read_datafile_to_lines('day08_sample', Data).
sample2(Data) :- read_datafile_to_lines('day08_sample2', Data).
data(Data) :- read_datafile_to_lines('day08_data', Data).

use_sample =>
  sample(Data),
  use(Data).

use_sample2 =>
  sample2(Data),
  use(Data).

use_data =>
  data(Data),
  use(Data).

right(_,_) :- false.
left(_,_) :- false.
instructions(_) :- false.
