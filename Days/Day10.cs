using AdventOfCode2023.Utils;
using JetBrains.Annotations;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection.Metadata.Ecma335;
using System.Runtime.ConstrainedExecution;
using TypeParser;

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
    public const char Closed = ' ';
    public const char Start = 'S';


    [TestCase(Input.Sample, 8)]
    [TestCase(Input.Data, 7012)]
    public override long Part1(Day10Data data)
    {
        var count = CountDistances(data.Start, data.Pipes);
        return count.Values.Max();
    }

    private IReadOnlyDictionary<Position, long> CountDistances(Position start, Pipes pipes)
    {
        var result = new Dictionary<Position, long>{{start, 0}};
        var open = new []{(position: start, distance: 0)}.ToQueue();
        while (open.TryDequeue(out var current))
        {
            foreach(var next in Adjacents(current.position, pipes[current.position]))
            {
                if (!pipes.ContainsKey(next)) throw new ApplicationException();
                if (result.ContainsKey(next)) continue;
                result[next] = current.distance + 1;
                open.Enqueue((next, current.distance + 1));
            }
        }
        return result;
    }

    private IEnumerable<Vector> VectorsOut(char pipe)
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

    private IEnumerable<Position> Adjacents(Position p, char pipe)
    {
        foreach(var v in VectorsOut(pipe)) yield return p + v;
    }

    [TestCase(Input.Sample2, 10)]
    [TestCase(Input.Data, 395)]
    public override long Part2(Day10Data data)
    {
        var loop = CountDistances(data.Start, data.Pipes).Keys.ToDictionary(it => it, it => data.Pipes[it]);
        return FindOpens(loop).Count(kv => kv.Value == Closed);
        
    }

    public Pipes FindOpens(Pipes loop)
    {
        var map = GenerateGrounds(loop);
        var upperLeft = loop.Keys.OrderBy(k => k.Y).ThenBy(k => k.X).First();
        if (loop[upperLeft] != SE) throw new ApplicationException();
        Paint(map, upperLeft + Vector.East, upperLeft, Vector.East);
        FloodFill(map);
        return map;
    }

    private void FloodFill(Dictionary<Position, char> map)
    {
        var open = map.Where(it => it.Value == Closed).Select(it => it.Key).ToQueue();
        while (open.TryDequeue(out var p))
        {
            foreach(var adjacent in p.OrthoganalNeighbors())
            {
                if (map.GetValueOrDefault(adjacent) == Ground)
                {
                    open.Enqueue(adjacent);
                    map[adjacent] = Closed;
                }
            }
        }
    }

    public void Paint(Dictionary<Position, char> map, Position current, Position terminal, Vector dom)
    {
        while (current != terminal)
        {
            var brush = dom.RotateRight();
            if (map[current + brush] == Ground)
            {
                map[current + brush] = Closed;
            }
            var vo = VectorsOut(map[current]).ToList();
            if (vo.Contains(dom))
            {
                // dom remains the same
            }
            else if (dom == Vector.East || dom == Vector.West)
            {
                if (vo.Contains(Vector.North)) dom = Vector.North;
                else if (vo.Contains(Vector.South)) dom = Vector.South;
                else throw new ApplicationException();
            }
            else if (dom == Vector.North || dom == Vector.South)
            {
                if (vo.Contains(Vector.East)) dom = Vector.East;
                else if (vo.Contains(Vector.West)) dom = Vector.West;
                else throw new ApplicationException();
            }
            else throw new ApplicationException();
            var newBrush = dom.RotateRight();
            if (newBrush != brush)
            {
                var x = current + newBrush;
                if (map[x] == Ground) map[x] = Closed;
            }
            current += dom;
        }
    }

    private Dictionary<Position, char> GenerateGrounds(Pipes loop)
    {
        return loop.Grid().ToDictionary(it => it, p => loop.ContainsKey(p) ? loop[p] : Ground);
    }
}


