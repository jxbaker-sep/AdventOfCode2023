:- [day5_parser].

subsequent('seed-to-soil', 'soil-to-fertilizer').
subsequent('soil-to-fertilizer', 'fertilizer-to-water').
subsequent('fertilizer-to-water', 'water-to-light').
subsequent('water-to-light', 'light-to-temperature').
subsequent('light-to-temperature', 'temperature-to-humidity').
subsequent('temperature-to-humidity', 'humidity-to-location').


walk(SourceStart, SourceRange, Kind, Result) :-
  map(Kind, DRS, SRS, RL),
  SourceStart + SourceRange -1 >= SRS,
  SourceStart < SRS + RL,
  NewStart is max(SourceStart, SRS),
  Offset is NewStart - SRS,
  NewRL is min(SourceStart + SourceRange, SRS + RL) - NewStart,
  DestinationStart is DRS + Offset,
  (subsequent(Kind, Kind2) -> walk(DestinationStart, NewRL, Kind2, Result) ; Result=DestinationStart).

do_part1(Result) :-
  findall(Location, (
    seed(X),
    walk(X, 1, 'seed-to-soil', Location)
  ), Locations),
  min_list(Locations, Result).

do_part2(Result) :-
  findall(Location, (
    seed_pair(X, Y),
    walk(X, Y, 'seed-to-soil', Location)
  ), Locations),
  min_list(Locations, Result).

:- begin_tests(day5).

test(part_1_sample, [true(Result =:= 35)]) :- use_sample, do_part1(Result).
test(part_1_data, [true(Result =:= 265_018_614)]) :- use_data, do_part1(Result).
test(part_2_sample, [true(Result =:= 46)]) :- use_sample, do_part2(Result).
test(part_2_data, [true(Result =:= 63_179_500)]) :- use_data, do_part2(Result).

:- end_tests(day5).