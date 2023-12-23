using AdventOfCode2023.Utils;
using System;
using System.Collections.Generic;
using System.Data;
using System.Linq;

namespace AdventOfCode2023.Day23;

using Grid = IReadOnlyList<IReadOnlyList<char>>;

public record Brick(Position3d P1, Position3d P2);

public class Day23 : AdventOfCode<long, Grid>
{
    public override Grid Parse(string input) => input.Lines().Select(it=>it.Select(it=>it).ToList()).ToList();

    const char Path = '.';
    const char Forest = '#';
    const char SlopeNorth='^';
    const char SlopeSouth='v';
    const char SlopeEast='>';
    const char SlopeWest='<';

    [TestCase(Input.Sample, 94)]
    // [TestCase(Input.Data, 2386)]
    public override long Part1(Grid grid)
    {
      return Walk(grid, true).Max();
    }

    [TestCase(Input.Sample, 154)]
    [TestCase(Input.Data, 0)]
    public override long Part2(Grid grid)
    {
      return Walk(grid, false).Max();
    }


    IEnumerable<long> Walk(Grid grid, bool slipperySlopes)
    {
      var start = grid.Grid().Where(p => grid.At(p) == Path).Single(p => p.Y == 0);
      var goal = grid.Grid().Where(p => grid.At(p) == Path).Single(p => p.Y == grid.Rows() -1);

      var open = new Queue<(Position P, long Steps, IReadOnlySet<Position> Visited)>();
      open.Enqueue((start, 0, new[]{start}.ToHashSet()));
      while (open.TryDequeue(out var current))
      {
        foreach(var neighbor in OpenNeighbors(current.P, grid, slipperySlopes))
        {
          if (current.Visited.Contains(neighbor)) continue;
          if (neighbor == goal) {yield return current.Steps + 1; continue;}
          open.Enqueue((neighbor, current.Steps + 1, current.Visited.Append(neighbor).ToHashSet()));
        }
      }
    }

    private IEnumerable<Position> OpenNeighbors(Position p, Grid grid, bool slipperySlopes)
    {
      var ns = slipperySlopes ? grid.At(p) switch
      {
        Path => p.OrthoganalNeighbors().ToArray(),
        SlopeEast => new[]{p + Vector.East},
        SlopeWest => new[]{p + Vector.West},
        SlopeNorth => new[]{p + Vector.North},
        SlopeSouth => new[]{p + Vector.South},
        _ => throw new ApplicationException()
      } : p.OrthoganalNeighbors().ToArray();
      foreach(var n in ns)
      {
        if (!grid.TryAt(n, out var c) || c == Forest) continue;
        yield return n;
      }
    }
}