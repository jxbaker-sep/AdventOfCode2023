:- module(day3_parser, [use_sample/0, use_data/0, digit/3, gear/2, symbol/2]).
:- [day3_sample].
:- [day3_data].
:- dynamic digit/3.
:- dynamic gear/2.
:- dynamic symbol/2.

is_digit(C) :- member(C, ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']).

inject(_, _, []) => true.
inject(Row, Col, [Char|Tail]) =>
  insert(Row, Col, Char),
  Col2 is Col + 1,
  inject(Row, Col2, Tail).

insert(_, _, '.') => true.

insert(Row, Col, Char), is_digit(Char) =>
  once(nth0(N, ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'], Char)),
  assert(( digit(Col, Row, N) )).

insert(Row, Col, *) =>
  assert(( gear(Col, Row) )),
  assert(( symbol(Col, Row) )).

insert(Row, Col, _) =>
  assert(( symbol(Col, Row) )).

-retract =>
  retractall( symbol(_, _) ),
  retractall( gear(_, _) ),
  retractall( digit(_, _, _) ).

inject_rows(_, []) => true.
inject_rows(Row, [Head|Tail]) =>
  inject(Row, 0, Head),
  Row2 is Row + 1,
  inject_rows(Row2, Tail).

use_sample =>
  -retract,
  !,
  sample(Data),
  inject_rows(0, Data).

use_data =>
  -retract,
  !,
  data(Data),
  inject_rows(0, Data).

digit(_,_,_) :- false.
gear(_,_) :- false.
symbol(_,_) :- false.