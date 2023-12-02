using AdventOfCode2023.Utils;
using JetBrains.Annotations;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;
using TypeParser;

namespace AdventOfCode2023.Days.Day02;

[UsedImplicitly]
public class Day02 : AdventOfCode<long,List<Game>>
{
    public override List<Game> Parse(string input) => TypeCompiler.ParseLines<Game>(input);

    [TestCase(Input.Example, 8)]
    [TestCase(Input.File, 2913)]
    public override long Part1(List<Game> games)
    {
        return games.Where(Possible).Select(game => game.Number).Sum();
    }

    private bool Possible(Game game) => game.Draws.SelectMany(d => d.Samples).All(Possible);

    private bool Possible(Sample sample) => sample.Color switch
    {
        "red" => sample.Count <= 12,
        "green" => sample.Count <= 13,
        "blue" => sample.Count <= 14,
        _ => throw new ApplicationException()
    };

    [TestCase(Input.Example, 2286)]
    [TestCase(Input.File, 55593)]
    public override long Part2(List<Game> input)
    {
        return input.Select(MinimumCubesPower).Sum();
    }

    private long MinimumCubesPower(Game game)
    {
        return game.Draws.SelectMany(d => d.Samples).GroupToDictionary(s => s.Color, s => s.Count)
            .Values
            .Select(l => l.Max())
            .Product();
    }
}

public record Sample(long Count, string Color);
public record Draw([Format(Separator = ",")]List<Sample> Samples);
public record Game([Format(Before="Game", After = ":")]long Number, [Format(Separator = ";")]List<Draw> Draws);
