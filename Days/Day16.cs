using AdventOfCode2023.Utils;
using JetBrains.Annotations;
using System;
using System.Collections.Generic;
using System.Linq;

namespace AdventOfCode2023.Day16;

using Day16Data = IReadOnlyList<string>;

[UsedImplicitly]
public class Day16 : AdventOfCode<long, Day16Data>
{
    public override Day16Data Parse(string input) => input.Lines();
    
    [TestCase(Input.Sample, 46)]
    [TestCase(Input.Data, 7632)]
    public override long Part1(Day16Data grid)
    {
      return Energize(grid, Position.Zero, Vector.East);
    }


    [TestCase(Input.Sample, 51)]
    [TestCase(Input.Data, 8023)]
    public override long Part2(Day16Data grid)
    {
      var south = Enumerable.Range(0, grid[0].Length).Select(col => (new Position(0, col), Vector.South));
      var north = Enumerable.Range(0, grid[0].Length).Select(col => (new Position(grid.Count - 1, col), Vector.North));
      var east = Enumerable.Range(0, grid.Count).Select(row => (new Position(row, 0), Vector.East));
      var west = Enumerable.Range(0, grid.Count).Select(row => (new Position(row, grid[0].Length-1), Vector.West));
      return south.Concat(north).Concat(east).Concat(west).Select(it => Energize(grid, it.Item1, it.Item2)).Max();
    }

    long Energize(Day16Data grid, Position startPosition, Vector startVector)
    {
      var open = new List<(Position p, Vector v)>{(startPosition, startVector)}.ToQueue();
      var closed = new HashSet<(Position p, Vector v)>{(startPosition, startVector)};

      while (open.TryDequeue(out var current))
      {
        var position = current.p;
        var vector = current.v;
        foreach(var next in Adjacent(grid[(int)position.Y][(int)position.X], vector))
        {
          var p = position + next;
          if (p.X < 0 || p.Y < 0) continue;
          if (p.Y >= grid.Count || p.X >= grid[0].Length) continue;
          if (closed.Add((p, next))) open.Enqueue((p, next));
        }
      }

      return closed.Select(it => it.p).Distinct().LongCount();
    }

    private IEnumerable<Vector> Adjacent(char c, Vector v)
    {
      if (c == '.') yield return  v;
      else if (c == '-')
      {
          if (v == Vector.East || v == Vector.West) yield return v;
          else
          {
            yield return Vector.East;
            yield return Vector.West;
          }
      }
      else if (c == '|')
      {
          if (v == Vector.North || v == Vector.South) yield return v;
          else
          {
            yield return Vector.North;
            yield return Vector.South;
          }
      }
      else if (c == '/')
      {
        if (v == Vector.East) yield return Vector.North;
        else if (v == Vector.South) yield return Vector.West;
        else if (v == Vector.West) yield return Vector.South;
        else if (v == Vector.North) yield return Vector.East;
      }
      else if (c == '\\')
      {
        if (v == Vector.East) yield return Vector.South;
        else if (v == Vector.South) yield return Vector.East;
        else if (v == Vector.West) yield return Vector.North;
        else if (v == Vector.North) yield return Vector.West;
      }
      else throw new ApplicationException();
    }
}
