using AdventOfCode2023.Utils;
using JetBrains.Annotations;
using System;
using System.Collections.Generic;
using System.Linq;

namespace AdventOfCode2023.Day03;

[UsedImplicitly]
public class Day03 : AdventOfCode<long, Data>
{
    public override Data Parse(string input) 
    {
        var indexToPartNumber = new Dictionary<long, long>();
        var positionToIndex = new Dictionary<Position, long>();
        var symbols = new Dictionary<Position, char>();

        var lines = input.Lines().WithIndices();
        var currentIndex = 0;
        var currentPartNumber = 0;
        foreach(var (line, y) in lines)
        {
            if (currentPartNumber != 0)
            {
                currentIndex += 1;
                currentPartNumber = 0;
            }
            foreach(var (c, x) in line.WithIndices())
            {
                if (new[]{'0', '1', '2', '3', '4', '5', '6', '7', '8', '9'}.Contains(c))
                {
                    currentPartNumber = currentPartNumber * 10 + c - '0';
                    positionToIndex[new Position(y, x)] = currentIndex;
                    indexToPartNumber[currentIndex] = currentPartNumber;
                    continue;
                }
                if (currentPartNumber != 0)
                {
                    currentIndex += 1;
                    currentPartNumber = 0;
                }
                if (c == '.') continue;
                if (new[]{'*', '#', '+', '$', '%', '/', '=', '&', '@', '-'}.Contains(c))
                {
                    symbols.Add(new Position(y, x), c);
                    continue;
                }
                throw new ApplicationException($"Unknown symbol '{c}'");
            }
        }
        return new(indexToPartNumber, positionToIndex, symbols);
    }

    [TestCase(Input.Sample, 4361)]
    [TestCase(Input.Data, 543867)]
    public override long Part1(Data data)
    {
        return data.Symbols.Keys.SelectMany(p => p.DiagonalAndOrthoganalNeighbors())
            .Select(p => data.PositionToIndex.TryGetValue(p, out var number) ? (long?)number : (long?)null)
            .OfType<long>()
            .Distinct()
            .Select(index => data.IndexToPartNumber[(int)index])
            .Sum();
    }

    [TestCase(Input.Sample, 467835)]
    [TestCase(Input.Data, 79613331)]
    public override long Part2(Data data)
    {
        return data.Symbols.Where(kv => kv.Value == '*')
            .Select(kv => 
                kv.Key.DiagonalAndOrthoganalNeighbors()
                .Select(nb => data.PositionToIndex.TryGetValue(nb, out var number) ? (long?)number : (long?)null)
                .OfType<long>()
                .Distinct()
                .Select(index => data.IndexToPartNumber[(int)index])
                .ToList())
            .Where(l => l.Count == 2)
            .Select(l => l[0] *l[1])
            .Sum();
    }
}

public record Data(IReadOnlyDictionary<long, long> IndexToPartNumber, IReadOnlyDictionary<Position, long> PositionToIndex, IReadOnlyDictionary<Position, char> Symbols);
