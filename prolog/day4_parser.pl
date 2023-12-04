:- module(day4_parser, [use_sample/0, use_data/0, wn/2, nyh/2]).
:- [day4_sample].
:- [day4_data].
:- [parse_utils].
:- dynamic wn/2, nyh/2.

s(N, Wins, Haves) --> "Card", ws, number(N), ":", nlist(Wins), " |", nlist(Haves).
nlist([]) --> [].
nlist([Head|Tail]) --> ws, number(Head), nlist(Tail).

insert_wins(_, []).
insert_wins(N, [Head|Tail]) :-
  assert(( wn(N, Head) )),
  insert_wins(N, Tail).

insert_haves(_, []).
insert_haves(N, [Head|Tail]) :-
  assert(( nyh(N, Head) )),
  insert_haves(N, Tail).

inject(Row) =>
  phrase(s(N, Wins, Haves), Row),
  insert_wins(N, Wins),
  insert_haves(N, Haves).

inject_rows([]) => true.
inject_rows([Head|Tail]) =>
  inject(Head),
  inject_rows(Tail).

-retract =>
  retractall( wn(_, _) ),
  retractall( nyh(_, _) ).

use(Data) =>
  -retract,
  inject_rows(Data), !.

use_sample =>
  sample(Data),
  use(Data).

use_data =>
  data(Data),
  use(Data).

wn(_,_) :- false.
nyh(_,_) :- false.