using AdventOfCode2023.Utils;
using System;
using System.Collections.Generic;
using System.Data;
using System.Linq;

namespace AdventOfCode2023.Day24;

using Points = IReadOnlyList<PV3>;

public record Point3(decimal X, decimal Y, decimal Z);
public record Vector3(decimal X, decimal Y, decimal Z);
public record PV3(Point3 Point, Vector3 Vector, Normalized N);

public record Normalized(decimal Slope, decimal YIntersection);

public class Day24 : AdventOfCode<long, Points>
{
    public override Points Parse(string input) => input.Lines().Select(line=>{
      var x = line.Split("@", StringSplitOptions.TrimEntries);
      var xyz = x[0].Split(",", StringSplitOptions.TrimEntries).Select(Convert.ToDecimal).ToList();
      var dxyz = x[1].Split(",", StringSplitOptions.TrimEntries).Select(Convert.ToDecimal).ToList();
      return new PV3(new(xyz[0], xyz[1], xyz[2]), new(dxyz[0], dxyz[1], dxyz[2]), Normalize(xyz[0], xyz[1], dxyz[0], dxyz[1]));
    }).ToList();

    [TestCase(Input.Sample, 2, Arg0 = 7, Arg1 = 27)]
    [TestCase(Input.Data, 21679, Arg0 = 200000000000000, Arg1 = 400000000000000)]
    public override long Part1(Points grid)
    {
      return grid.Pairs().LongCount(p => Intersect(p.First, p.Second, TestCase.Arg0, TestCase.Arg1));
    }

    // [TestCase(Input.Sample, 154)]
    // [TestCase(Input.Data, 6246)] 6246 correct, takes 101.6s to generate
    public override long Part2(Points grid)
    {
      return 0;
    }

    public bool Intersect(PV3 p1, PV3 p2, decimal min, decimal max)
    {
      var n1 = p1.N;
      var n2 = p2.N;
      if (n1.Slope == n2.Slope) return false;
      var x = (n2.YIntersection - n1.YIntersection) / (n1.Slope - n2.Slope);
      if (!IsInFuture(p1, x) || !IsInFuture(p2, x)) return false;
      var y = n1.Slope * x + n1.YIntersection;
      return min <= x && x <= max && min <= y && y <= max;
    }

    public bool IsInFuture(PV3 p, decimal x)
    {
      if (p.Vector.X > 0) return x > p.Point.X;
      return x < p.Point.X;
    }


    public Normalized Normalize(decimal x, decimal y, decimal dx, decimal dy)
    {
      return new Normalized(dy / dx, y - (x / dx) * dy);
    }

}