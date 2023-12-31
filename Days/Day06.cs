﻿using AdventOfCode2023.Utils;

using System;
using System.Collections.Generic;
using System.Linq;

namespace AdventOfCode2023.Day05;


public class Day06 : AdventOfCode<long,IReadOnlyList<Race>>
{
    public override IReadOnlyList<Race> Parse(string input) {
        var lines = input.Lines();
        var times = lines[0]["Time:".Length..].Parse<IReadOnlyList<long>>();
        var distances = lines[1]["Distance:".Length..].Parse<IReadOnlyList<long>>();
        var x = times.Zip(distances).Select(z => new Race(z.First, z.Second)).ToList();

        var timez = Convert.ToInt64(lines[0]["Time:".Length..].Replace(" ", ""));
        var distancesz = Convert.ToInt64(lines[1]["Distance:".Length..].Replace(" ", ""));
        x.Add(new Race(timez, distancesz));
        return x;
    }

    [TestCase(Input.Sample, 288)]
    [TestCase(Input.Data, 1084752)]
    public override long Part1(IReadOnlyList<Race> races)
    {
        return races.Take(races.Count - 1).Select(Compute).Product();
    }

    [TestCase(Input.Sample, 71503)]
    [TestCase(Input.Data, 28228952)]
    public override long Part2(IReadOnlyList<Race> races)
    {
        return Compute(races.Last());
    }

    private long Compute(Race race)
    {
        var n = 0;
        for(var x = 0; x < race.Time; x++)
        {
            var z = x * (race.Time - x);
            n += (z > race.Distance) ? 1 : 0;
        }
        return n;
    }
}

public record Race(long Time, long Distance);