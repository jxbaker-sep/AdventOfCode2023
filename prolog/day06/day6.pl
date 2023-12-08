:- set_prolog_flag(double_quotes, chars).
:- use_module(library(clpfd)).
:- use_module('../parse_utils', [read_datafile_to_lines/2, number/3, ws/2, optional_ws/2]).
:- ['../utils'].

solve(Time, Distance, Solution) :-
    Solution in 0..Time,
    Solution * (Time - Solution) #> Distance.

count_solutions(Time, Distance, Result) :-
  solve(Time, Distance, Solution),
  fd_size(Solution, Result).

do_part1(Races, Result) :-
  findall(Count,(
    member([Time, Distance], Races),
    count_solutions(Time, Distance, Count)
  ), Counts),
  product(Counts, Result).

times(T) --> "Time:", optional_ws, numbers(T).
distance(T) --> "Distance:", optional_ws, numbers(T).
numbers([]) --> [].
numbers([H]) --> number(H).
numbers([H|T]) --> number(H), ws, numbers(T).

sample(Data) :- read_datafile_to_lines('day06_sample', [A,B]),
    phrase(times(Times), A), !,
    phrase(distance(Distances), B), !,
    zip(Times, Distances, Data).

data(Data) :- read_datafile_to_lines('day06_data', [A,B]),
  phrase(times(Times), A), !,
  phrase(distance(Distances), B), !,
  zip(Times, Distances, Data).

sample2(Data) :- read_datafile_to_lines('day06_sample', [A,B]),
  exclude(==(' '), A, A2),
  exclude(==(' '), B, B2),
  phrase(times(Times), A2), !,
  phrase(distance(Distances), B2), !,
  zip(Times, Distances, Data).

data2(Data) :- read_datafile_to_lines('day06_data', [A,B]),
  exclude(==(' '), A, A2),
  exclude(==(' '), B, B2),
  phrase(times(Times), A2), !,
  phrase(distance(Distances), B2), !,
  zip(Times, Distances, Data).

:- begin_tests(day6).

test(part_1_sample, [true(Result =:= 288)]) :- sample(Data), do_part1(Data, Result).
test(part_1_data, [true(Result =:= 1084752)]) :- data(Data), do_part1(Data, Result).
test(part_2_sample, [true(Result =:= 71503)]) :- sample2(Data), do_part1(Data, Result).
test(part_2_data, [true(Result =:= 28228952)]) :- data2(Data), do_part1(Data, Result).

:- end_tests(day6).