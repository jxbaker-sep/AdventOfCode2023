:- module(day5_parser, [use_sample/0, use_data/0, seed/1, map/4, seed_pair/2]).
:- [day5_sample].
:- [day5_data].
:- ['../parse_utils'].
:- dynamic seed/1, map/4, seed_pair/2.

seeds(NList) --> "seeds: ", nlist(NList).
kind_map(Kind) --> word_with_dashes(Kind2), ws, "map:", { atomic_list_concat(Kind2, Kind) }.
word_with_dashes([K]) --> lord(K).
word_with_dashes([K|Tail]) --> lord(K), word_with_dashes(Tail).
nlist([N]) --> number(N).
nlist([N|T]) --> number(N), ws, nlist(T).
n3(A, B, C) --> number(A), ws, number(B), ws, number(C).
lord(K) --> letter(K).
lord('-') --> "-".

assertseed(Item) :- assert(( seed(Item) )).

assert_seed_pairs([]).
assert_seed_pairs([A, B|Tail]) :-
  assert(( seed_pair(A, B) )),
  assert_seed_pairs(Tail).

assert_map(Kind, A, B, C) :- 
  assert(( map(Kind, A, B, C) )).

inject_rows([], _).

inject_rows([Row|Tail], Kind) :-
  phrase(seeds(NList), Row), !,
  maplist(assertseed, NList),
  assert_seed_pairs(NList),
  inject_rows(Tail, Kind).

inject_rows([Row|Tail], _) :-
  phrase(kind_map(Kind), Row), !,
  inject_rows(Tail, Kind).

inject_rows([Row|Tail], _) :-
  phrase(kind_map(Kind), Row), !,
  inject_rows(Tail, Kind).

inject_rows([Row|Tail], Kind) :-
  phrase(n3(A, B, C), Row), !,
  assert_map(Kind, A, B, C),
  inject_rows(Tail, Kind).


inject_rows([[]|Tail], Kind) :-
  inject_rows(Tail, Kind).

-retract =>
  retractall( seed(_) ),
  retractall( map(_,_,_,_) ),
  retractall( seed_pair(_,_) ).

add_zero_rule1(Kind, Start, Range) :-
  Range >= 1, !,
  assert(( map(Kind, Start, Start, Range) )).

add_zero_rule1(_,_,_).

add_zero_rule2(_, []).
add_zero_rule2(Kind, [[BStart,BRange]]) :-
  X2 is BStart + BRange,
  add_zero_rule1(Kind, X2, 9_999_999_999).
add_zero_rule2(Kind, [[AStart,ARange],[BStart,BRange]|Tail]) :-
  X1 is AStart + ARange,
  L is BStart - X1,
  add_zero_rule1(Kind, X1, L),
  L2 = [[BStart,BRange]|Tail],
  add_zero_rule2(Kind, L2).

add_zero_rule(Kind) :-
  findall([X, Y], map(Kind, X, _, Y), Result),
  sort(Result, Result2),
  [[H1,_]|_] = Result2,
  add_zero_rule1(Kind, 0, H1),
  add_zero_rule2(Kind, Result2), !.

add_zero_rules([]).
add_zero_rules([Head|Tail]) :-
  add_zero_rule(Head), add_zero_rules(Tail).

add_zero_rules :-
  Sequence = [
    'seed-to-soil',
    'soil-to-fertilizer',
    'fertilizer-to-water',
    'water-to-light',
    'light-to-temperature',
    'temperature-to-humidity',
    'humidity-to-location'
  ],
  add_zero_rules(Sequence).

use(Data) =>
  -retract,
  inject_rows(Data, ''), !,
  add_zero_rules.

use_sample =>
  sample(Data),
  use(Data).

use_data =>
  data(Data),
  use(Data).

seed(_) :- false.
seed_pair(_,_) :- false.
map(_,_,_,_) :- false.