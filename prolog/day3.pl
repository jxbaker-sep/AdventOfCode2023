:- [day3_parser].

number_head(X, Y) :-
  digit(X, Y, _),
  X2 is X -1,
  \+ digit(X2, Y, _).

get_number_head(X, Y, X) :-
  digit(X, Y, _),
  X2 is X-1,
  \+ digit(X2, Y, _),
  !.

get_number_head(X, Y, X2) :-
  digit(X, Y, _),
  X3 is X-1,
  digit(X3, Y, _),
  get_number_head(X3, Y, X2).

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

adjacent(X, Y, X2, Y2) :- X2 is X - 1, Y2 is Y - 1.
adjacent(X, Y, X2, Y2) :- X2 is X - 1, Y2 is Y + 0.
adjacent(X, Y, X2, Y2) :- X2 is X - 1, Y2 is Y + 1.
adjacent(X, Y, X2, Y2) :- X2 is X + 0, Y2 is Y - 1.
adjacent(X, Y, X2, Y2) :- X2 is X + 0, Y2 is Y + 1.
adjacent(X, Y, X2, Y2) :- X2 is X + 1, Y2 is Y - 1.
adjacent(X, Y, X2, Y2) :- X2 is X + 1, Y2 is Y + 0.
adjacent(X, Y, X2, Y2) :- X2 is X + 1, Y2 is Y + 1.

is_number_adjacent_to_symbol(X, Y) :-
  symbol(X2, Y2),
  abs(Y2 - Y) =< 1,
  is_number_adjacent(X, Y, X2, Y2), !.

is_number_adjacent(X, Y, _, _) :-
  \+ digit(X, Y, _), !, false.

is_number_adjacent(_, Y, _, Y2) :-
  abs(Y - Y2) > 1, !, false.

is_number_adjacent(X, Y, X2, Y2) :-
  adjacent(X, Y, X2, Y2), !.

is_number_adjacent(X, Y, X2, Y2) :-
  X3 is X + 1,
  is_number_adjacent(X3, Y, X2, Y2), !.

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
      is_number_adjacent(X2, Y2, X, Y),
      get_number(X2, Y2, PartNumber)
    ), PartNumbers), 
    [N1,N2] = PartNumbers,
    GearRatio is N1 * N2
  ), Ratios),
  sumlist(Ratios, Result).