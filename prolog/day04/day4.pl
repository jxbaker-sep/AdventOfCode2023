:- [day4_parser].
:- table score_card/2, win_card/2.

max_card(Result) :- findall(X, wn(X, _), Ns), max_list(Ns, Result).

count_wins(Card, Result) :-
  findall(
    X, (
      wn(Card, X),
      nyh(Card, X)
    ),
    Wins
  ),
  length(Wins, Result).

do_part1(Result) :-
  findall(
    Score, (
      max_card(MaxCard),
      between(1, MaxCard, Card),
      count_wins(Card, L),
      (L == 0 -> Score is 0 ; Score is 2 ** (L-1))
    ),
    Scores
  ),
  sumlist(Scores, Result).

win_cards([], Accum, Result) => Result = Accum.

win_cards([Head|Tail], Accum, Result) =>
  win_card(Head, Temp),
  Temp2 is Temp + 1 + Accum,
  win_cards(Tail, Temp2, Result).

win_card(N, Result) :-
  score_card(N, Score),
  win_cards(Score, 0, Result).

score_card(N, Result) =>
  count_wins(N, L),
  N2 is N + 1,
  N3 is N + L, 
  (findall(X, between(N2, N3, X), Result) -> true ; Result = []).

do_part2(Result) :-
  max_card(MaxCard),
  findall(X, between(1, MaxCard, X), Start),
  win_cards(Start, 0, Result).

:- begin_tests(day4).
% swipl -g 'time(run_tests)' -t halt day4.pl

test(part_1_sample, [true(Result =:= 13)]) :- abolish_all_tables, use_sample, do_part1(Result).
test(part_1_data, [true(Result =:= 32_001)]) :- abolish_all_tables, use_data, do_part1(Result).
test(part_2_sample, [true(Result =:= 30)]) :- abolish_all_tables, use_sample, do_part2(Result).
test(part_2_data, [true(Result =:= 5_037_841)]) :- abolish_all_tables, use_data, do_part2(Result).

:- end_tests(day4).