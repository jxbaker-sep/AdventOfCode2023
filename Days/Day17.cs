using AdventOfCode2023.Utils;

using System;
using System.Collections.Generic;
using System.Linq;

namespace AdventOfCode2023.Day17;

using Day17Data = IReadOnlyList<IReadOnlyList<long>>;


public class Day17 : AdventOfCode<long, Day17Data>
{
    public override Day17Data Parse(string input) => input.Lines().Select(it => it.Select(y => Convert.ToInt64($"{y}")).ToList()).ToList();
    
    [TestCase(Input.Sample, 102)]
    [TestCase(Input.Data, 843)]
    public override long Part1(Day17Data grid)
    {
      return CalculateHeatLoss(grid, 0, 3);
    }

    [TestCase(Input.Sample, 94)]
    [TestCase(Input.Raw, 71, Raw = @"111111111111
999999999991
999999999991
999999999991
999999999991")]
    [TestCase(Input.Data, 1017)]
    public override long Part2(Day17Data grid)
    {
      return CalculateHeatLoss(grid, 4, 10);
    }

    private record Step(Position Position, long HeatLoss, Vector Heading, long StepsSinceTurn, IReadOnlyList<Position> Path);

    private long CalculateHeatLoss(Day17Data grid, long minStraight, long maxStraight)
    {
        var goal = new Position(grid.Count - 1, grid[0].Count - 1);
        var open = new PriorityQueue<Step>(r => r.HeatLoss + r.Position.ManhattanDistance(goal));
        open.Enqueue(new(Position.Zero, 0, Vector.East, 0, new List<Position>{Position.Zero}));
        open.Enqueue(new(Position.Zero, 0, Vector.South, 0, new List<Position>{Position.Zero}));

        // var closed = new Dictionary<Position, long>{{Position.Zero, 0}};
        var closed = new Dictionary<(Position, Vector, long), long>();

        while (open.TryDequeue(out var current))
        {
          // Console.WriteLine($"{current.HeatLoss}, {current.Path.Select(p => p.ToString()).Join(",")}");
          var cck = (current.Position, current.Heading, current.StepsSinceTurn);
          if (closed.TryGetValue(cck, out var updatedHeatLoss) && updatedHeatLoss < current.HeatLoss) continue;

          var headings = new List<Vector>();
          if (current.StepsSinceTurn < maxStraight) {
            headings.Add(current.Heading);
          }
          if (current.StepsSinceTurn >= minStraight) {
            headings.Add(current.Heading.RotateLeft());
            headings.Add(current.Heading.RotateRight());
          }

          foreach(var newHeading in headings)
          {
            var n = current.Position + newHeading;
            if (!n.Y.IsInRange(0, grid.Count - 1)) continue;
            if (!n.X.IsInRange(0, grid[0].Count - 1)) continue;
            var newStepsSinceTurn = (newHeading == current.Heading) ? current.StepsSinceTurn + 1 : 1;
            var hl = current.HeatLoss + grid[(int)n.Y][(int)n.X];
            if (n == goal) {
              if (newStepsSinceTurn < minStraight) continue;
              return hl;
            }
            if (current.Path.Contains(n)) continue;

            var ck = (n, newHeading, newStepsSinceTurn);
            if (closed.TryGetValue(ck, out var knownMinHeatLoss) && knownMinHeatLoss <= hl) continue;
            var newPath = current.Path.Append(n).ToList();
            open.Enqueue(new(n, hl, newHeading, newStepsSinceTurn, newPath));
            closed[ck] = hl;
          }
        }

        throw new ApplicationException();
    }


}
