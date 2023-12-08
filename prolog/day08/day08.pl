:- [day08_parser].
:- ['../utils'].
:- use_module(library(clpfd)).

walk(Current, Current, _, _, Accum, Accum).

walk(Current, Destination, [], Instructions, Accum, Result) :-
  walk(Current, Destination, Instructions, Instructions, Accum, Result).

walk(Current, Destination, [I|Tail], Instructions, Accum, Result) :-
  next(Current, I, Next),
  Accum2 #= Accum + 1,
  walk(Next, Destination, Tail, Instructions, Accum2, Result).

next(Current, 'L', Next) :- left(Current, Next).
next(Current, 'R', Next) :- right(Current, Next).

do_part1(Result) :-
  instructions(I),
  walk('AAA', 'ZZZ', I, I, 0, Result), !.

%========          PART 2 ===============

endsWith(C, Atom) :- atom_concat(_, C, Atom).

walk2(Current, _, _, Accum, Accum) :- endsWith('Z', Current).

walk2(Current, [], Instructions, Accum, Result) :-
  walk2(Current, Instructions, Instructions, Accum, Result).

walk2(Current, [I|Tail], Instructions, Accum, Result) :-
  next(Current, I, Next),
  Accum2 #= Accum + 1,
  walk2(Next, Tail, Instructions, Accum2, Result).

% walk_n(Current, 0, _, _, Current) :- !.
% walk_n(Current, N, [], I, Destination) :- walk_n(Current, N, I, I, Destination).
% walk_n(Current, N, [I|Tail], Instructions, Destination) :-
%   next(Current, I, Next),
%   N2 #= N - 1,
%   walk_n(Next, N2, Tail, Instructions, Destination).

do_part2(Result) :-
  instructions(I),
  findall(Node, (left(Node, _), endsWith('A', Node)), Nodes),
  findall(Step, (member(N, Nodes), once(walk2(N, I, I, 0, Step))), Steps),
  maplist(roots, Steps, Roots),
  flatten(Roots, FlatRoots),
  sort(FlatRoots, SortedRoots),
  product(SortedRoots, Result).

roots_accum(N, X, []) :-
  X #> N, !.

roots_accum(N, X, [X|Tail]) :- 
  N mod X #= 0,
  N2 #= N // X,
  roots_accum(N2, X, Tail), !.

roots_accum(N, X, Result) :-
  X2 #= X + 1,
  roots_accum(N, X2, Result).

roots(N, Result) :-
  roots_accum(N, 2, Result).

%============= TESTS ====================

:- begin_tests(day8).

test(part_1_sample, [true(Result =:= 6)]) :- use_sample, do_part1(Result).
test(part_1_data, [true(Result =:= 14681)]) :- use_data, do_part1(Result).
test(part_2_sample, [true(Result =:= 6)]) :- use_sample2, do_part2(Result).
test(part_2_sample, [true(Result =:= 14321394058031)]) :- use_data, do_part2(Result).

:- end_tests(day8).