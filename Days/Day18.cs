using AdventOfCode2023.Utils;

using System;
using System.Collections.Generic;
using System.Linq;

namespace AdventOfCode2023.Day18;

using Day18Data = IReadOnlyList<DigInstruction>;

public record DigInstruction(Vector Direction, long Length, string Color);


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
        return AreaOfDig(digInstructions);
    }


    [TestCase(Input.Sample, 952_408_144_115)]
    [TestCase(Input.Data, 106_920_098_354_636)]
    public override long Part2(Day18Data badDigInstructions)
    {
      var digInstructions = badDigInstructions.Select(Goodify).ToList();
      return AreaOfDig(digInstructions);
    }

    private static long AreaOfDig(Day18Data digInstructions)
    {
        var p = Position.Zero;
        var l = new List<Position> { Position.Zero };
        foreach (var instruction in digInstructions)
        {
            p += instruction.Direction * instruction.Length;
            l.Add(p);
        }

        var aop = Helper.AreaOfPolygon(l);
        var x = digInstructions.Sum(it => it.Length);

        return aop + (x / 2) + 1;
    }
    
    private DigInstruction Goodify(DigInstruction bad)
    {
      var x = bad.Color[2..^1];
      var d = Convert.ToInt32($"{x[^1]}");
      var l = Convert.ToInt64(x[..^1], 16);

      var ds = new[]{Vector.East, Vector.South,Vector.West,Vector.North};
      return new(ds[d], l, bad.Color);
    }

    public Vector d2v(char c) => c switch{
      'U' => Vector.North,
      'R' => Vector.East,
      'D' => Vector.South,
      'L' => Vector.West,
      _ => throw new ApplicationException()
    };
}
