using AdventOfCode2023.Utils;
using System;
using System.Collections.Generic;
using System.Linq;
using TypeParser;

namespace AdventOfCode2023.Day05;

public class Day05 : AdventOfCode<long,Day05Struct>
{
    public override Day05Struct Parse(string input) {
        var seeds = new List<long>();
        var steps = new List<List<N3>>();
        var current = new List<N3>();
        foreach (var line in input.Lines())
        {
            if (line.StartsWith("seeds:"))
            {
                var temp = line.Parse<Seeds>();
                seeds.AddRange(temp.seeds);
            }
            if (line.EndsWith("map:"))
            {
                current = new List<N3>();
                steps.Add(current);
            }
            if (line.TryParse<N3>(out var n3))
            {
                current.Add(n3);
            }
        }

        steps = steps.Select(step => FillInZeroes(step)).ToList();

        return new Day05Struct(seeds, steps);
    }

    private static List<N3> FillInZeroes(IReadOnlyList<N3> n3s)
    {
        var sorted = n3s.OrderBy(it => it.DestinationRangeStart).ToList();
        var n1 = sorted[0];
        var pre = new N3(0, 0, n1.DestinationRangeStart);
        sorted.Windows(2).SelectMany(n => {
            var dsr = n[0].DestinationRangeStart + n[0].RangeLength;
            var drl = n[1].DestinationRangeStart - dsr;
            if (drl <= 0) { return new List<N3>{ n[0], n[1] }; }
            return new List<N3>{ n[0], new N3(dsr, dsr, drl), n[1] };
        });
        var nlast = sorted.Last();
        var nlaster = nlast.DestinationRangeStart + nlast.RangeLength;
        var post = new N3(nlaster, nlaster, long.MaxValue - nlaster);
        var result = new List<N3> { pre };
        result.AddRange(sorted);
        result.Add(post);
        return result;
    }

    [TestCase(Input.Sample, 35)]
    [TestCase(Input.Data, 265_018_614)]
    public override long Part1(Day05Struct cards)
    {
        return cards.Seeds.SelectMany(seed => Walk(seed, 1, cards.Steps)).Min();
    }

    [TestCase(Input.Sample, 46)]
    [TestCase(Input.Data, 63_179_500)]
    public override long Part2(Day05Struct cards)
    {
        return cards.Seeds.InGroupsOf(2).SelectMany(seed => Walk(seed[0], seed[1], cards.Steps)).Min();
    }

    public IEnumerable<long> Walk(long sourceStart, long sourceRange, IReadOnlyList<IReadOnlyList<N3>> steps)
    {
        var current = steps[0];
        foreach(var step in current)
        {
            if (sourceStart + sourceRange - 1 >= step.SourceRangeStart && sourceStart < step.SourceRangeStart + step.RangeLength)
            {
                var newStart = Math.Max(sourceStart, step.SourceRangeStart);
                var offset = newStart - step.SourceRangeStart;
                var newRangeLength = Math.Min(sourceStart + sourceRange, step.SourceRangeStart + step.RangeLength) - newStart;
                var destinationStart = step.DestinationRangeStart + offset;
                if (steps.Count == 1) yield return destinationStart;
                else 
                {
                    var v = Walk(destinationStart, newRangeLength, steps.Skip(1).ToList());
                    foreach(var x in v) yield return x;
                }
            }
        }
    }
}

public record N3(long DestinationRangeStart, long SourceRangeStart, long RangeLength);

public record Seeds([Format(Before="seeds:")]IReadOnlyList<long> seeds);
public record Day05Struct(IReadOnlyList<long> Seeds, IReadOnlyList<IReadOnlyList<N3>> Steps);