% :- [day08_parser].
% :- ['../utils'].
:- use_module(library(clpfd)).

sample([
  [19, 13, 30, -2,  1, -2],
  [18, 19, 22, -1, -1, -2],
  [20, 25, 34, -2, -2, -4],
  [12, 31, 28, -1, -2, -1],
  [20, 19, 15,  1, -5, -3]
  ]) :- !.

solve(X, DX, A, DA, T) :-
  T #> 0,
  X + DX * T #= A + DA * T.

solve_all([], _, _, _, _, _, _, []) :- true, !.

solve_all([[X,Y,Z,DX,DY,DZ]|Tail], A, DA, B, DB, C, DC, [T|Ts]) :-
  solve(X, DX, A, DA, T),
  solve(Y, DY, B, DB, T),
  solve(Z, DZ, C, DC, T),
  solve_all(Tail, A, DA, B, DB, C, DC, Ts).

%============= TESTS ====================

:- begin_tests(day24).

% test(part_1_sample, [true(Result =:= 6)]) :- use_sample, do_part1(Result).
% test(part_1_data, [true(Result =:= 14681)]) :- use_data, do_part1(Result).
% test(part_2_sample, [true(Result =:= 6)]) :- use_sample2, do_part2(Result).
% test(part_2_data, [true(Result =:= 14_321_394_058_031)]) :- use_data, do_part2(Result).

:- end_tests(day24).