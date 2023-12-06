:- ['../parse_utils'].
:- [day2_data].
:- use_module('../utils').


s([G|D]) --> "Game ", number(G), ": ", draws(D).
draws([D]) --> samples(D).
draws([D|Ds]) --> samples(D), "; ", draws(Ds).
samples([S]) --> sample(S).
samples([S|T]) --> sample(S), ", ", samples(T).
sample([N, C]) --> number(N), " ", word(C).

legal_draw([]).
legal_draw([[Count, red]|_]) :- Count > 12, !, fail.
legal_draw([[Count, green]|_]) :- Count > 13, !, fail.
legal_draw([[Count, blue]|_]) :- Count > 14, !, fail.
legal_draw([_|Tail]) :- legal_draw(Tail).

legal_draws([]).
legal_draws([Draw|Tail]) :-
  legal_draw(Draw),
  legal_draws(Tail).

legal_game([_|Draws]) :- 
  legal_draws(Draws).

do_part1(Data, Result) :-
  findall(Game, (member(X, Data), phrase(s(Game), X)), Games),
  include(legal_game, Games, LegalGames),
  findall(GameNumber, member([GameNumber|_], LegalGames), LegalGameNumbers),
  sumlist(LegalGameNumbers, Result).

sample(["Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green",
"Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue",
"Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red",
"Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red",
"Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
]).

sample_power([Count, red], [R, G, B], [R2, G, B]) :- !, R2 is max(Count, R).
sample_power([Count, green], [R, G, B], [R, G2, B]) :- !, G2 is max(Count, G).
sample_power([Count, blue], [R, G, B], [R, G, B2]) :- !, B2 is max(Count, B).

samples_power(Samples, Accum, Out) :-
  foldl(sample_power, Samples, Accum, Out).

draws_power(Draws, Out) :-
  foldl(samples_power, Draws, [0,0,0], Out).

game_power([_|Draws], Out) :- draws_power(Draws, Out).

do_part2(Data, Result) :-
  findall(Game, (member(X, Data), phrase(s(Game), X)), Games),
  maplist(game_power, Games, GamePowers),
  maplist(product, GamePowers, Powers),
  sumlist(Powers, Result).

:- begin_tests(day1).
% swipl -g 'time(run_tests)' -t halt day1.pl

test(part_1_sample, [true(Result =:= 8)]) :- sample(Data), do_part1(Data, Result).
test(part_1_data, [true(Result =:= 2913)]) :- data(Data), do_part1(Data, Result).
test(part_2_sample, [true(Result =:= 2286)]) :- sample(Data), do_part2(Data, Result).
test(part_2_data, [true(Result =:= 55593)]) :- data(Data), do_part2(Data, Result).

:- end_tests(day1).