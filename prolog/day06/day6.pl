:- set_prolog_flag(double_quotes, chars).
:- use_module(library(clpfd)).
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

  :- begin_tests(day6).

  test(part_1_sample, [true(Result =:= 288)]) :- sample(Data), do_part1(Data, Result).
  test(part_1_data, [true(Result =:= 1084752)]) :- data(Data), do_part1(Data, Result).
  test(part_2_sample, [true(Result =:= 71503)]) :- sample2(Data), do_part1(Data, Result).
  test(part_2_data, [true(Result =:= 28228952)]) :- data2(Data), do_part1(Data, Result).
  
  :- end_tests(day6).