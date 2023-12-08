:- [day08_parser].
:- ['../utils'].
:- use_module(library(clpfd)).

walk_(Current, Goal, _, Accum, Accum) :- call(Goal, Current).

walk_(Current, Goal, [], Accum, Result) :-
  instructions(I),
  walk_(Current, Goal, I, Accum, Result).

walk_(Current, Goal, [I|Tail], Accum, Result) :-
  next(Current, I, Next),
  Accum2 #= Accum + 1,
  walk_(Next, Goal, Tail, Accum2, Result).

walk(Start, Goal, Result) :- walk_(Start, Goal, [], 0, Result).

next(Current, 'L', Next) :- left(Current, Next).
next(Current, 'R', Next) :- right(Current, Next).

ends_with(C, Atom) :- atom_concat(_, C, Atom).

do_part1(Result) :-
  walk('AAA', ==('ZZZ'), Result), !.

do_part2(Result) :-
  findall(Node, (left(Node, _), ends_with('A', Node)), Nodes),
  findall(Step, (member(N, Nodes), once(walk(N, ends_with('Z'), Step))), Steps),
  lcm_list(Steps, Result).

%============= TESTS ====================

:- begin_tests(day8).

test(part_1_sample, [true(Result =:= 6)]) :- use_sample, do_part1(Result).
test(part_1_data, [true(Result =:= 14681)]) :- use_data, do_part1(Result).
test(part_2_sample, [true(Result =:= 6)]) :- use_sample2, do_part2(Result).
test(part_2_sample, [true(Result =:= 14_321_394_058_031)]) :- use_data, do_part2(Result).

:- end_tests(day8).