using AdventOfCode2023.Utils;
using JetBrains.Annotations;
using System;
using System.Collections.Generic;
using System.Linq;
using TypeParser;

namespace AdventOfCode2023.Day18;

using Day18Data = IReadOnlyList<DigInstruction>;

public record DigInstruction(Vector Direction, long Length, string Color);

[UsedImplicitly]
public class Day18 : AdventOfCode<long, Day18Data>
{
    public override Day18Data Parse(string input) => input.Lines().Select(line => {
      var x = line.Split(" ");
      return new DigInstruction(d2v(x[0].Single()), Convert.ToInt64(x[1]), x[2]);
    }).ToList();
    
    public enum GroundType {
      Clear,
      Trench,
      Unknown
    }

    [TestCase(Input.Sample, 62)]
    [TestCase(Input.Data, 40761)]
    public override long Part1(Day18Data digInstructions)
    {
      var dig = new Dictionary<Position, GroundType>{{Position.Zero, GroundType.Trench}};
      var p = Position.Zero;
      var l = new List<Position>{Position.Zero};
      foreach(var instruction in digInstructions)
      {
        for(var n = 0; n < instruction.Length; n++)
        {
          p += instruction.Direction;
          
          dig[p] = GroundType.Trench;
        }
      }

      var aop = Helper.AreaOfPolygon(dig.Keys.ToList());

      // var bounds = dig.Bounds().Extend(1);
      return aop + (dig.Keys.Count / 2) + 1;
      // foreach(var point in bounds.Points())
      // {
      //   if (!dig.ContainsKey(point)) dig[point] = GroundType.Unknown;
      // }

      // FloodFill(dig, bounds.Points().First());

      

      // return dig.Values.Count(it => it != GroundType.Clear);
    }


    // [TestCase(Input.Sample, 952_408_144_115)]
    // [TestCase(Input.Data, 0)]
    public override long Part2(Day18Data grid)
    {
      return 0;
    }

    private void FloodFill(Dictionary<Position, GroundType> dig, Position position)
    {
        var open = new[]{position}.ToQueue();
        while (open.TryDequeue(out var value))
        {
          foreach(var n in value.OrthoganalNeighbors())
          {
            if (!dig.ContainsKey(n)) continue;
            if (dig[n] != GroundType.Unknown) continue;
            dig[n] = GroundType.Clear;
            open.Enqueue(n);
          }
        }
    }

    public Vector d2v(char c) => c switch{
      'U' => Vector.North,
      'R' => Vector.East,
      'D' => Vector.South,
      'L' => Vector.West,
      _ => throw new ApplicationException()
    };
}
