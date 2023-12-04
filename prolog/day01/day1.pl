:- [day1_data].
:- [parse_utils].

s(D, D) --> nondigits, digit(D), nondigits.
s(D1, D2) --> nondigits, digit(D1), anything, digit(D2), nondigits.

s2(D1, D2) --> lazy_nondigits, wordy_digit(D1), greedy_anything, wordy_digit(D2), lazy_nondigits.
s2(D, D) --> lazy_nondigits, wordy_digit(D), lazy_nondigits.
wordy_digit(D) --> digit(D).
wordy_digit(1) --> "one".
wordy_digit(2) --> "two".
wordy_digit(3) --> "three".
wordy_digit(4) --> "four".
wordy_digit(5) --> "five".
wordy_digit(6) --> "six".
wordy_digit(7) --> "seven".
wordy_digit(8) --> "eight".
wordy_digit(9) --> "nine".


part1([], Acc, Acc).
part1([Head|Tail], Acc, Result) :-
  s(D1, D2, Head, []), !,
  Acc2 is Acc + 10 * D1 + D2,
  part1(Tail, Acc2, Result).

part2([], Acc, Acc).
part2([Head|Tail], Acc, Result) :-
  s2(D1, D2, Head, []), !,
  Acc2 is Acc + 10 * D1 + D2,
  atomic_list_concat(Head, Word), 
  part2(Tail, Acc2, Result).

do_part1(Data, Result) :-
  part1(Data, 0, Result).

do_part2(Data, Result) :-
  part2(Data, 0, Result).

part2_sample(["two1nine",
  "eightwothree",
  "abcone2threexyz",
  "xtwone3four",
  "4nineeightseven2",
  "zoneight234",
  "7pqrstsixteen"]).

% :- initialization(main).

% main :- data(Data), do_part2(Data, Result).