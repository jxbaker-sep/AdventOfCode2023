using AdventOfCode2023.Days.Day04;
using AdventOfCode2023.Utils;
using JetBrains.Annotations;
using System;
using System.Collections.Generic;
using System.Configuration;
using System.Linq;
using System.Text.RegularExpressions;
using TypeParser;

namespace AdventOfCode2023.Days.Day07;

[UsedImplicitly]
public class Day07 : AdventOfCode<long,IReadOnlyList<HandValue>>
{
    public override IReadOnlyList<HandValue> Parse(string input) {
        var lines = input.Lines();
        return lines.Select(line => {
            var t = line.Split(" ");
            var value = Convert.ToInt64(t[1]);

            var longs = ComputeLongs(t[0]);
            var type = ComputeType(longs);
            var weight = ComputeWeight(type, longs);

            var longs2 = ComputeLongs2(t[0]);
            var type2 = ComputeType2(longs2);
            var weight2 = ComputeWeight(type2, longs2);

            return new HandValue(new Hand(type, weight, type2, weight2), value);
        }).ToList();
    }

    private long ComputeWeight(long type, List<long> longs)
    {
        return longs.Aggregate(type, (accum, next) => accum * 15 + next);
    }

    private long ComputeType(List<long> longs)
    {
        var temp = longs.GroupToDictionary();
        var groups = temp.ToDictionary(it => it.Key, it => it.Value.Count);
        var counts_ = groups.GroupToDictionary(it => it.Value);
        var counts = counts_.ToDictionary(it => it.Key, it => it.Value.Count);
        var xs = new[]{5,4,3,2,1}.Select(it => counts.GetValueOrDefault(it, 0)).ToList();
        return xs
            .Aggregate((accum, it) => accum * 15 + it);
    }

    private long ComputeType2(List<long> longs)
    {
        var temp = longs.GroupToDictionary();
        var groups = temp.ToDictionary(it => it.Key, it => it.Value.Count);
        var zeroes = groups.GetValueOrDefault(0, 0);

        foreach(var k in groups.Keys.Where(k => k != 0 ).OrderByDescending(k => groups[k]).Take(1))
        {
            var value = groups[k];
            groups[k] += zeroes;
            zeroes = 0;
        }
        if (zeroes != 0 && zeroes != 5) {
            throw new ApplicationException();
        }
        if (zeroes == 0)
            groups.Remove(0);

        return ComputeType(groups.SelectMany(g => Enumerable.Repeat(g.Key, g.Value)).ToList());
    }

    private List<long> ComputeLongs2(string v)
    {
        return v.Select(c => (long)"J23456789TQKA".IndexOf(c)).ToList();
    }

    private List<long> ComputeLongs(string v)
    {
        return v.Select(c => (long)"23456789TJQKA".IndexOf(c)).ToList();
    }

    [TestCase(Input.Example, 6440)]
    [TestCase(Input.File, 249726565)]
    public override long Part1(IReadOnlyList<HandValue> HandValues)
    {
        return HandValues.OrderBy(hv => hv.Hand.Strength).WithIndices()
            .Sum(hv => hv.Value.Value * (hv.Index+1));
    }

    [TestCase(Input.Example, 5905)]
    [TestCase(Input.File, 251135960)]
    public override long Part2(IReadOnlyList<HandValue> HandValues)
    {
        return HandValues.OrderBy(hv => hv.Hand.Strength2).WithIndices()
            .Sum(hvi => hvi.Value.Value *(hvi.Index+1));
    }

}

public record Hand(long Type, long Strength, long Type2, long Strength2);
public record HandValue(Hand Hand, long Value);