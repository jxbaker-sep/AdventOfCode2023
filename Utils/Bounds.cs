using System.Collections;
using System.Collections.Generic;

namespace AdventOfCode2023.Utils;

public record Bounds(long Top, long Left, long Bottom, long Right);

public static class BoundsExtensions
{
  public static Bounds Extend(this Bounds b, long length)
  {
    return new Bounds(b.Top - length, b.Left - length, b.Bottom + length, b.Right + length);
  }

  public static IEnumerable<Position> Points(this Bounds bounds)
  {
      for(var row = bounds.Top; row <= bounds.Bottom; row++)
      {
        for(var col = bounds.Left; col <= bounds.Right; col++)
        {
          yield return new Position(row, col);
        }
      }

  }
}