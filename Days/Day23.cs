using AdventOfCode2023.Utils;
using System;
using System.Collections.Generic;
using System.Data;
using System.Linq;
using System.Threading;

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

    // [TestCase(Input.Sample, 94)]
    // [TestCase(Input.Data, 2386)]
    public override long Part1(Grid grid)
    {
      return Walk(grid, true).Max();
    }

    [TestCase(Input.Sample, 154)]
    [TestCase(Input.Data, 0)] // 5970 too low
    public override long Part2(Grid grid)
    {
      var threads = new HashSet<Thread>();
      var start = grid.Grid().Where(p => grid.At(p) == Path).Single(p => p.Y == 0);
      var goal = grid.Grid().Where(p => grid.At(p) == Path).Single(p => p.Y == grid.Rows() -1);
      CreateThreads(start, new HashSet<Position>{start}, grid, threads);
      var xstart = threads.Single(it => it.P1 == start);
      var xgoal = threads.Single(it => it.P2 == goal);
      return ChainThreads(new Chain([xstart.P1, xstart.P2], xstart.Length), threads, xgoal.P1)!.Length
        + xgoal.Length;
    }

    public record Chain(IReadOnlyList<Position> Points, long Length);

    public Dictionary<string, Chain?> Memoise = new();

    Chain? ChainThreads(Chain chain, HashSet<Thread> threads, Position goal)
    {
      var key = CreateKey(chain);
      if (Memoise.TryGetValue(key, out var found)) return found;

      var tails = threads.Where(t => t.P1 == chain.Points[^1] && !chain.Points.Contains(t.P2)).ToList();
      Chain? max = null;
      foreach(var tail in tails)
      {
        var tailChain = new Chain(chain.Points.Append(tail.P2).ToList(), chain.Length + tail.Length);
        if (tail.P2 == goal)
        {
          if (max is null) max = tailChain;
          else if (tailChain.Length > max.Length) max = tailChain;
        }
        else if (ChainThreads(tailChain,threads, goal) is {} x)
        {
          if (max is null) max = x;
          else if (x.Length > max.Length) max = x;
        }
      }
      Memoise[key] = max;
      return max;
    }

    private string CreateKey(Chain chain)
    {
        return chain.Points.OrderBy(p=>p.Y).ThenBy(p=>p.X).Select(p => p.ToString()).Join(";") + chain.Points.Last().ToString();
    }

    private void CreateThreads(Position start, IEnumerable<Position> visited2, Grid grid, HashSet<Thread> threads)
    {
      var visited = visited2.ToHashSet();
      var current = start;
      long count = 0;
        while (true)
        {
          var rawNeighbors = OpenNeighbors(current, grid, false).ToList();
          var ns = rawNeighbors.Except(visited).ToList();
          if (ns.Count == 0)
          {
            var goal = grid.Grid().Where(p => grid.At(p) == Path).Single(p => p.Y == grid.Rows() -1);
            if (current == goal)
            {
              threads.Add(new Thread(start, current, count));
              threads.Add(new Thread(current, start, count));
              return;
            }
          }
          if (ns.Count == 1)
          {
            count += 1;
            visited.Add(ns.First());
            current = ns.First();
            continue;
          }
          var expanded = threads.Any(it => it.P1 == current);
          threads.Add(new Thread(start, current, count));
          threads.Add(new Thread(current, start, count));
          if (!expanded)
          {
            foreach(var n in ns)
            {
              CreateThreads(current, rawNeighbors.Except([n]).Append(current), grid, threads);
            }
          }
          break;
        }
    }

    public record Thread(Position P1, Position P2, long Length);

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

    private IEnumerable<Vector> VectorsOut(Position p, Grid grid)
    {
      var vs = Vector.Cardinal;
      foreach(var v in vs)
      {
        var n = p + v;
        if (!grid.TryAt(n, out var c) || c == Forest) continue;
        yield return v;
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