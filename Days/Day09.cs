using AdventOfCode2023.Day04;
using AdventOfCode2023.Utils;
using JetBrains.Annotations;
using System;
using System.Collections.Generic;
using System.Configuration;
using System.Linq;
using System.Text.RegularExpressions;
using TypeParser;

namespace AdventOfCode2023.Day09;

[UsedImplicitly]
public class Day09 : AdventOfCode<long,IReadOnlyList<IReadOnlyList<long>>>
{
    public override IReadOnlyList<IReadOnlyList<long>> Parse(string input) =>
        TypeCompiler.ParseLines<IReadOnlyList<long>>(input);


    [TestCase(Input.Sample, 114)]
    [TestCase(Input.Data, 1702218515)]
    public override long Part1(IReadOnlyList<IReadOnlyList<long>> values)
    {
        return values.Select(Interpolate).Sum();
    }

    public long Interpolate(IReadOnlyList<long> values)
    {
        if (values.All(d => d == 0)) {
            return 0;
        }
        var deltas = values.Windows(2).Select(it => it[1] - it[0]).ToList();
        var next = Interpolate(deltas);
        return values.Last() + next;
    }

    [TestCase(Input.Sample, 2)]
    [TestCase(Input.Data, 0)]
    public override long Part2(IReadOnlyList<IReadOnlyList<long>> values)
    {
        return values.Select(ReverseInterpolate).Sum();
    }

    public long ReverseInterpolate(IReadOnlyList<long> values)
    {
        if (values.All(d => d == 0)) {
            return 0;
        }
        var deltas = values.Windows(2).Select(it => it[1] - it[0]).ToList();
        var next = ReverseInterpolate(deltas);
        return  values.First() - next;
    }
}