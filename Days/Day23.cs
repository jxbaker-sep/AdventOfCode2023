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

    // [TestCase(Input.Sample, 94)]
    // [TestCase(Input.Data, 2386)]
    public override long Part1(Grid grid)
    {
      return Walk(grid, true).Max();
    }

    [TestCase(Input.Sample, 154)]
    // [TestCase(Input.Data, 0)] // 5970 too low
    public override long Part2(Grid grid)
    {
      Memoise = new();
      var segments = new HashSet<Segment>();
      var start = grid.Grid().Where(p => grid.At(p) == Path).Single(p => p.Y == 0);
      var goal = grid.Grid().Where(p => grid.At(p) == Path).Single(p => p.Y == grid.Rows() -1);
      CreateSegments(start, OpenNeighbors(start, grid, false).Single(), grid, segments);
      // var rc = grid.GridValues().Where(p => p.Value != Forest)
      //   .Select(p => {
      //     var x = OpenNeighbors(p.Position, grid, false).Count();
      //     if (x > 2) Console.WriteLine($"{p}: {x}");
      //     return x;
      //   })
      //   .Where(ns => ns > 2)
      //   .Sum();
      foreach(var segment in segments.OrderBy(it => it.P1).ThenBy(it => it.P2)) Console.WriteLine($"{segment.P1}, {segment.P2}, {segment.Length}");
      var xstart = segments.Single(it => it.P1 == start);
      var xgoal = segments.Single(it => it.P2 == goal);
      var chain = FindLongestChain(new Chain([xstart.P1, xstart.P2], xstart.Length, [xstart]), segments, goal);

      PrintChain(grid, chain);

      foreach(var pt in chain.Points) Console.WriteLine($"{pt}");
      var temp = chain.Segments.SelectMany(s => s.Positions).ToHashSet().Count();
      Console.WriteLine(temp);
      return chain!.Length;
    }

    private void PrintChain(Grid grid, Chain chain)
    {
        var grid2 = grid.Select(it => it.ToList()).ToList();
        foreach(var p in chain.Segments.SelectMany(it => it.Positions))
        {
          grid2.Set(p, '*');
        }
        grid2.PrintGrid();
    }


    public long Id = 0;
    public record Chain(IReadOnlyList<Position> Points, long Length, IReadOnlyList<Segment> Segments);

    public Dictionary<string, Chain?> Memoise = new();

    Chain? FindLongestChain(Chain chain, HashSet<Segment> segments, Position goal)
    {
      var key = CreateKey(chain);
      if (Memoise.TryGetValue(key, out var found)) return found;

      var tails = segments.Where(segment => segment.P1 == chain.Points[^1] && !chain.Points.Contains(segment.P2)).ToList();
      Chain? max = null;
      foreach(var tail in tails)
      {
        var chainPlusTail = new Chain(chain.Points.Append(tail.P2).ToList(), chain.Length + tail.Length, chain.Segments.Append(tail).ToList());
        if (tail.P2 == goal)
        {
          if (max is null) max = chainPlusTail;
          else if (chainPlusTail.Length > max.Length) max = chainPlusTail;
        }
        else if (FindLongestChain(chainPlusTail,segments, goal) is {} x)
        {
          if (max is null) max = x;
          else if (x.Length > max.Length) max = x;
        }
      }
      Memoise.Add(key, max);
      return max;
    }

    private string CreateKey(Chain chain)
    {
        return chain.Points.OrderBy(p=>p).Select(p => p.ToString()).Join(";") + "=>" + chain.Points.Last().ToString();
    }

    private void CreateSegments(Position intersection, Position current, Grid grid, HashSet<Segment> segments)
    {
      var visited = new[]{intersection, current}.ToHashSet();
      long count = 0 + extra;
      while (true)
      {
        var rawNeighbors = OpenNeighbors(current, grid, false).ToList();
        var ns = rawNeighbors.Except(visited).ToList();
        if (ns.Count == 0)
        {
          var goal = grid.Grid().Where(p => grid.At(p) == Path).Single(p => p.Y == grid.Rows() -1);
          if (current == goal)
          {
            count += 1;
            segments.Add(new Segment(intersection, current, count, visited.ToList()));
            segments.Add(new Segment(current, intersection, count, visited.ToList()));
            return;
          } 
          throw new ApplicationException();
        }
        if (ns.Count == 1)
        {
          count += 1;
          visited.Add(ns.First());
          current = ns.First();
          continue;
        }
        count += 1;
        var expanded = segments.Any(it => it.P1 == current);
        if (!segments.Any(it => it.P1 == current && it.P2 == intersection))
          segments.Add(new Segment(current, intersection, count, visited.ToList()));
        if (!segments.Any(it => it.P1 == intersection && it.P2 == current))
          segments.Add(new Segment(intersection, current, count, visited.ToList()));
        if (!expanded)
        {
          foreach(var n in ns)
          {
            CreateSegments(current, n, grid, segments);
          }
        }
        break;
      }
    }

    public Dictionary<long, IReadOnlySet<Position>> SegmentIdToPositions = new();
    public record Segment(Position P1, Position P2, long Length, IReadOnlyList<Position> Positions);

    IEnumerable<long> Walk(Grid grid, bool slipperySlopes)
    {
      var start = grid.Grid().Single(p => grid.At(p) == Path && p.Y == 0);
      var goal = grid.Grid().Single(p => grid.At(p) == Path && p.Y == grid.Rows() -1);

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