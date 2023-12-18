using System.Collections.Generic;
using System.Linq;

namespace AdventOfCode2023.Utils;

public static class Helper
{
    public static IEnumerable<long> Between(long min, long max)
    {
        for (var x = min; x <= max; x++)
        {
            yield return x;
        }
    }

    public static T Identity<T>(T t) => t;

    public static long AreaOfPolygon(IReadOnlyList<Position> poly)
    {
        return poly.Windows2().Append((poly[^1], poly[0]))
            .Aggregate(0L, (accum, it) => accum + it.Item1.X * it.Item2.Y - it.Item1.Y * it.Item2.X)
            / 2;
    }
}
