:- [day3_parser].

number_head(X, Y) :-
  digit(X, Y, _),
  X2 is X -1,
  \+ digit(X2, Y, _).

get_number(X, Y, Number) :-
  number_head(X, Y),
  get_number_acc(X, Y, 0, Number).

get_number_acc(X, Y, Accum, Accum) :-
  \+ digit(X, Y, _), !.

get_number_acc(X, Y, Accum, Result) :-
  digit(X, Y, N),
  Accum2 is Accum * 10 + N,
  X2 is X+1,
  get_number_acc(X2, Y, Accum2, Result).

number_adjacent(X, Y, X2, Y2) :- X2 is X-1, Y2 is Y - 1.
number_adjacent(X, Y, X2, Y2) :- X2 is X-1, Y2 is Y + 0.
number_adjacent(X, Y, X2, Y2) :- X2 is X-1, Y2 is Y + 1.
number_adjacent(X, Y, X2, Y2) :- -subnumber_adjacent(X, Y, X2, Y2).

-subnumber_adjacent(X, Y, X, Y2) :- Y2 is Y - 1.
-subnumber_adjacent(X, Y, X, Y2) :- Y2 is Y + 1.
-subnumber_adjacent(X, Y, X2, Y2) :- 
  X3 is X + 1,
  ( digit(X3, Y, _) -> -subnumber_adjacent(X3, Y, X2, Y2) ; -tail_adjacent(X3, Y, X2, Y2)).
-tail_adjacent(X, Y, X, Y2) :- Y2 is Y - 1.
-tail_adjacent(X, Y, X, Y2) :- Y2 is Y + 0.
-tail_adjacent(X, Y, X, Y2) :- Y2 is Y + 1.


is_number_adjacent_to_symbol(X, Y) :-
  number_adjacent(X, Y, X2, Y2),
  symbol(X2, Y2), !.

do_part1(Result) :- 
  findall(Z, (
    number_head(X, Y), 
    is_number_adjacent_to_symbol(X, Y), 
    get_number(X, Y, Z)
  ), Result2), 
  sumlist(Result2, Result).

do_part2(Result) :-
  findall(GearRatio, (
    gear(X, Y), 
    findall(PartNumber, ( 
      number_head(X2, Y2), 
      number_adjacent(X2, Y2, X, Y),
      get_number(X2, Y2, PartNumber)
    ), PartNumbers), 
    [N1,N2] = PartNumbers,
    GearRatio is N1 * N2
  ), Ratios),
  sumlist(Ratios, Result).

:- begin_tests(day3).

test(part_1_sample, [true(Result =:= 4361)]) :- use_sample, do_part1(Result).
test(part_1_data, [true(Result =:= 543867)]) :- use_data, do_part1(Result).
test(part_2_sample, [true(Result =:= 467835)]) :- use_sample, do_part2(Result).
test(part_2_data, [true(Result =:= 79613331)]) :- use_data, do_part2(Result).

:- end_tests(day3).