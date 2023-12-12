using System.Collections.Generic;

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
}
