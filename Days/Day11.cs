using AdventOfCode2023.Utils;
using JetBrains.Annotations;
using System.Collections.Generic;
using System.Linq;

namespace AdventOfCode2023.Day11;

using Galaxies = IReadOnlySet<Position>;

[UsedImplicitly]
public class Day11 : AdventOfCode<long, Galaxies>
{
    public override Galaxies Parse(string input) 
    {
        return input.Lines()
           .SelectMany((line, row) => 
                line.WithIndices()
                    .Where(it => it.Value == '#')
                   .Select(it => new Position(row, it.Index)))
            .ToHashSet(); 
    }

    [TestCase(Input.Sample, 374, Arg0 = 2)]
    [TestCase(Input.Data, 9522407, Arg0 = 2)]
    [TestCase(Input.Sample, 1030, Arg0 = 10)]
    [TestCase(Input.Sample, 8410, Arg0 = 100)]
    [TestCase(Input.Data, 544723432977, Arg0 = 1000000)]
    public override long Part1(Galaxies data)
    {
        var doubledRows = DetectDoubledRows(data);
        var doubledCols = DetectDoubledCols(data);
        return data.Pairs().Select(pair => Distance(pair.First, pair.Second, doubledRows, doubledCols, TestCase.Arg0)).Sum();
    }

    public override long Part2(Galaxies data)
    {
        return 0;
    }

    private long Distance(Position a, Position b, IReadOnlySet<long> doubledRows, IReadOnlySet<long> doubledCols,
        long expansionFactor)
    {
        var m = a.ManhattanDistance(b);
        var xmin = (int)new[]{a.X, b.X}.Min();
        var xmax = (int)new[]{a.X, b.X}.Max();
        var ymin = (int)new[]{a.Y, b.Y}.Min();
        var ymax = (int)new[]{a.Y, b.Y}.Max();
        var extraX = Enumerable.Range(xmin, xmax - xmin + 1)
           .Count(it => doubledCols.Contains(it));
        var extraY = Enumerable.Range(ymin, ymax - ymin + 1)
           .Count(it => doubledRows.Contains(it));
        var z = ((long)extraX + (long)extraY);
        return m + z * (expansionFactor - 1);
    }

    private IReadOnlySet<long> DetectDoubledRows(Galaxies data)
    {
        var rows = data.Select(it => it.Y).ToHashSet();
        var min = (int)rows.Min();
        var max = (int)rows.Max();
        return Enumerable.Range(min, max - min + 1).Select(it=>(long)it).Where(y => !rows.Contains(y)).ToHashSet();
    }

    private IReadOnlySet<long> DetectDoubledCols(Galaxies data)
    {
        var cols = data.Select(it => it.X).ToHashSet();
        var min = (int)cols.Min();
        var max = (int)cols.Max();
        return Enumerable.Range(min, max - min + 1).Select(it=>(long)it).Where(x => !cols.Contains(x)).ToHashSet();
    }
}


