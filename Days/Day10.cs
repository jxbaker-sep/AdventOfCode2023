using AdventOfCode2023.Utils;
using JetBrains.Annotations;
using System;
using System.Collections.Generic;
using System.Linq;

namespace AdventOfCode2023.Day10;

using Pipes = IReadOnlyDictionary<Position, char>;
public record Day10Data(Position Start, Pipes Pipes);

[UsedImplicitly]
public class Day10 : AdventOfCode<long, Day10Data>
{
    public override Day10Data Parse(string input) 
    {
        var d = new Dictionary<Position, char>();
        var result = new Day10Data(Position.Zero, d);
        foreach(var (line, row) in input.Lines().WithIndices())
        {
            foreach(var (c, col) in line.WithIndices())
            {
                var position = new Position(row, col);
                if (c == Ground) continue;
                if (c == Start)
                {
                    result = result with {Start = position};
                }
                else
                {
                    d[position] = c;
                }
            }
        }

        var n = d.GetValueOrDefault(result.Start + Vector.North);
        var s = d.GetValueOrDefault(result.Start + Vector.South);
        var e = d.GetValueOrDefault(result.Start + Vector.East);
        var w = d.GetValueOrDefault(result.Start + Vector.West);

        if (LeadsSouth(n) && LeadsNorth(s)) d[result.Start] = Vertical;
        else if (LeadsEast(w) && LeadsWest(e)) d[result.Start] = Horizontal;
        else if (LeadsWest(e) && LeadsSouth(n)) d[result.Start] = NE;
        else if (LeadsWest(e) && LeadsNorth(s)) d[result.Start] = SE;
        else if (LeadsEast(w) && LeadsSouth(n)) d[result.Start] = NW;
        else if (LeadsEast(w) && LeadsNorth(s)) d[result.Start] = SW;
        else throw new ApplicationException();

        return result;
    }

    private bool LeadsNorth(char c) => VectorsOut(c).Contains(Vector.North);
    private bool LeadsSouth(char c) => VectorsOut(c).Contains(Vector.South);
    private bool LeadsEast(char c) => VectorsOut(c).Contains(Vector.East);
    private bool LeadsWest(char c) => VectorsOut(c).Contains(Vector.West);


    public const char Vertical = '|';
    public const char Horizontal = '-';
    public const char NE = 'L';
    public const char NW = 'J';
    public const char SW = '7';
    public const char SE = 'F';
    public const char Ground = '.';
    public const char Start = 'S';


    [TestCase(Input.Sample, 8)]
    [TestCase(Input.Data, 7012)]
    public override long Part1(Day10Data data)
    {
        var temp = FindLoop(data.Start, data.Pipes);
        return temp.Count / 2;
    }

    public IReadOnlyList<Position> FindLoop(Position start, Pipes pipes)
    {
        var result = new List<Position>{start};
        var dom = VectorsOut(pipes[start]).First();
        var current = start + dom;
        while (current != start)
        {
            result.Add(current);
            var vo = VectorsOut(pipes[current]);
            dom = new[]{dom, dom.RotateLeft(), dom.RotateRight()}.First(it => vo.Contains(it));
            current += dom;
        }
        return result;
    }

    private static IEnumerable<Vector> VectorsOut(char pipe)
    {
        if (pipe == Vertical || pipe == NE || pipe == NW)
            yield return Vector.North; 
        if (pipe == Vertical || pipe == SE || pipe == SW)
            yield return Vector.South;
        if (pipe == Horizontal || pipe == SE || pipe == NE)
            yield return Vector.East;
        if (pipe == Horizontal || pipe == SW || pipe == NW)
            yield return Vector.West;
    }

    [TestCase(Input.Sample2, 10)]
    [TestCase(Input.Data, 395)]
    public override long Part2(Day10Data data)
    {
        var x = FindLoop(data.Start, data.Pipes);
        var area = AreaOfPolygon(x);
        return area - x.Count / 2  + 1;
    }

    public static long AreaOfPolygon(IReadOnlyList<Position> poly)
    {
        return poly.Windows2().Append((poly.Last(), poly.First()))
            .Aggregate(0L, (accum, it) => accum + it.Item1.X * it.Item2.Y - it.Item1.Y * it.Item2.X)
            / 2;
    }
}


