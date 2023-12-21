using AdventOfCode2023.Day04;
using AdventOfCode2023.Utils;
using System.Collections.Generic;
using System.Data;
using System.Linq;

namespace AdventOfCode2023.Day21;

using Day21Data = (Position Start, IReadOnlyList<IReadOnlyList<char>> Grid);

public class Day21 : AdventOfCode<long, Day21Data>
{
  const char Garden = '.';
  const char Rock = '#';
  const char Start = 'S';

    public override Day21Data Parse(string input) {
      var raw = input.Lines().Select(it=>it.ToList()).ToList();
      var start = raw.Grid().Where(p => raw.At(p) == 'S').Single();
      raw.Set(start, Garden);
      return (start, raw);
    }

    [TestCase(Input.Sample, 16, Arg0 = 6)]
    [TestCase(Input.Data, 3658, Arg0 = 64)]
    public override long Part1(Day21Data world)
    {
      var MaxSteps = TestCase.Arg0;
      var grid = world.Grid;
      var open = new[]{(P: world.Start, Steps: 0)}.ToQueue();
      var closed = new[]{open.First()}.ToHashSet();
      while (open.TryDequeue(out var current))
      {
        var (p, steps) = current;
        if (steps == MaxSteps) continue;
        foreach(var p2 in p.OrthoganalNeighbors())
        {
          if (grid.TryAt(p2, out var next) && next == Garden)
          {
            var temp = (p2, steps + 1);
            if (closed.Add(temp)) open.Enqueue(temp);
          }
        }
      }

      return closed.Count(it => it.Steps == MaxSteps);
    }


    // [TestCase(Input.Sample, 16, Arg0 = 6)]
    // [TestCase(Input.Sample, 50, Arg0 = 10)]
    // [TestCase(Input.Sample, 1594, Arg0 = 50)]
    // [TestCase(Input.Sample, 6536, Arg0 = 100)]
    // [TestCase(Input.Sample, 167004, Arg0 = 500)]
    // [TestCase(Input.Sample, 668697, Arg0 = 1000)]
    // [TestCase(Input.Sample, 16733044, Arg0 = 5000)]
    // [TestCase(Input.Data, 0, Arg0 = 26_501_365)]
    public override long Part2(Day21Data world)
    {
      var MaxSteps = TestCase.Arg0;
      var grid = world.Grid;
      var open = new[]{(P: world.Start, Steps: 0)}.ToQueue();
      var closed = new[]{open.First()}.ToHashSet();
      while (open.TryDequeue(out var current))
      {
        var (p, steps) = current;
        if (steps == MaxSteps) continue;
        foreach(var p2 in p.OrthoganalNeighbors())
        {
          var row = LMath.MathMod(p2.Y, grid.Rows());
          var col = LMath.MathMod(p2.X, grid.Cols());
          if (grid.TryAt(new Position(row, col), out var next) && next == Garden)
          {
            var temp = (p2, steps + 1);
            if (closed.Add(temp)) open.Enqueue(temp);
          }
        }
      }

      return closed.Count(it => it.Steps == MaxSteps);
    }

}
