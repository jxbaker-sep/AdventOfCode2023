:- [day5_parser].

mapsequence([
  'seed-to-soil',
  'soil-to-fertilizer',
  'fertilizer-to-water',
  'water-to-light',
  'light-to-temperature',
  'temperature-to-humidity',
  'humidity-to-location'
]).

do_map(Source, Kind, Destination) :-
  map(Kind, DestinationRangeStart, SourceRangeStart, Length),
  Source >= SourceRangeStart,
  Source < SourceRangeStart + Length,
  !,
  Destination is Source - SourceRangeStart + DestinationRangeStart.

do_map(_, Kind, _) :-
  \+ once(map(Kind, _, _, _)),
  throw(error(domain_error(kind, Kind), 'invalid kind')).

do_map(Source, _, Source). 

-s2l([], Result, Result).
-s2l([Head|Tail], Accum, Result) :-
  do_map(Accum, Head, Accum2),
  -s2l(Tail, Accum2, Result).

seed_to_location(Seed, Location) :-
  mapsequence(Sequence),
  -s2l(Sequence, Seed, Location).

do_part1(Result) :-
  findall(Location, (
    seed(X),
    seed_to_location(X, Location)
  ), Locations),
  min_list(Locations, Result).

% Rule is a list with values DestinationRangeStart, SourceRangeStart, RangeLength
create_new_range_rule(OldDestinationRangeStart, OldSourceStart, SourceRange, Kind, Rules) :-
  SourceStart is OldDestinationRangeStart,
  findall([NewDRS, NewSRS, NewRL], (
    map(Kind, DRS, SRS, RL),
    SourceStart + SourceRange -1 >= SRS,
    SourceStart < SRS + RL,
    NewStart is max(SourceStart, SRS),
    NewRL is min(SourceStart + SourceRange, SRS + RL) - NewStart,
    NewSRS is OldSourceStart + NewStart - SourceStart,
    NewDRS is  DRS + NewStart - SRS
  ), Rules).

create_new_range_rules(_, [], []).
create_new_range_rules(Kind, [[ODRS,SS,SR]|Tail], Result) :-
  create_new_range_rule(ODRS, SS, SR, Kind, Result2), !,
  writeln([[ODRS,SS,SR], Kind, Result2]),
  append(Result2, Result3, Result),
  create_new_range_rules(Kind, Tail, Result3), !.

create_all_range_rules(Result) :-
  findall([X, X, Y], seed_pair(X, Y), StartingRangeRules),
  mapsequence(Sequence),
  foldl(create_new_range_rules, Sequence, StartingRangeRules, Result).

% -6284386055 - wrong
do_part2(Result) :-
  create_all_range_rules(Range),
  findall(X, (
    member([A, _, _], Range),
    X is A
  ), Xs),
  min_list(Xs, Result).

:- begin_tests(day4).

test(part_1_sample, [true(Result =:= 35)]) :- use_sample, do_part1(Result).
test(part_1_data, [true(Result =:= 265_018_614)]) :- use_data, do_part1(Result).
test(part_2_sample, [true(Result =:= 46)]) :- use_sample, do_part2(Result).
test(part_2_data, [true(Result =:= 63_179_500)]) :- use_data, do_part2(Result).

:- end_tests(day4).