using AdventOfCode2023.Utils;

using System;
using System.Collections.Generic;
using System.Linq;

namespace AdventOfCode2023.Day14;

using Day14Data = IReadOnlyList<IReadOnlyList<char>>;


public class Day14 : AdventOfCode<long, Day14Data>
{
    public override Day14Data Parse(string input) => input.Lines().Select(it=>it.ToList()).ToList();
    
    [TestCase(Input.Sample, 136)]
    [TestCase(Input.Data, 106186)]
    public override long Part1(Day14Data data)
    {
        var height = data.Count;
        return RollNorth(data).Select((line, row) => (long)line.Count(c => c == 'O') * (height - row)).Sum();
    }

    [TestCase(Input.Sample, 64)]
    [TestCase(Input.Data, 106390)]
    public override long Part2(Day14Data data)
    {
      var height = data.Count;
      var stash = new Dictionary<string, long>();
      const long itMax = 1_000_000_000L;
      var found = false;
      for(long x = 0; x < itMax; x++)
      {
          data = RollNorth(data);
          data = RollWest(data);
          data = RollSouth(data);
          data = RollEast(data);
          var key = Key(data);
          if (!found && stash.TryGetValue(key, out var lastX))
          {
            var cycleLength = x - lastX;
            var remainingCycles = (itMax - x) / cycleLength;
            x += remainingCycles * cycleLength;
            found = true;
          }
          if (!found) stash.Add(key, x);
      }
      return data.Select((line, row) => (long)line.Count(c => c == 'O') * (height - row)).Sum();
    }

    private static string Key(Day14Data data) => data.SelectMany(row => row).Join();

    public static Day14Data RollNorth(Day14Data data)
    {
      var colMax = Enumerable.Repeat(0, data[0].Count).ToList();
      var result = Enumerable.Range(0, data.Count).Select(row => Enumerable.Repeat('.', data[0].Count).ToList()).ToList();
      foreach(var (line, row) in data.WithIndices())
      {
        foreach(var (c, col) in line.WithIndices())
        {
          if (c == '#')
          {
            colMax[col] = row + 1;
            result[row][col] = '#';
          }
          else if (c == 'O')
          {
            result[colMax[col]][col] = 'O';
            colMax[col] = colMax[col] + 1;
          }
        }
      }
      return result;
    }

    public Day14Data RollSouth(Day14Data data) => RollNorth(data.Reverse().ToList()).Reverse().ToList();

    public Day14Data RollWest(Day14Data data) => RollNorth(data.RotateRight()).RotateLeft();

    public Day14Data RollEast(Day14Data data) => RollNorth(data.RotateLeft()).RotateRight();
}
