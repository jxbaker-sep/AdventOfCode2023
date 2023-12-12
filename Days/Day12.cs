using AdventOfCode2023.Utils;
using JetBrains.Annotations;
using Microsoft.VisualBasic;
using System;
using System.Collections.Generic;
using System.IO.Compression;
using System.Linq;

namespace AdventOfCode2023.Day12;

public record Day12Record(string Template, List<long> Runs);

[UsedImplicitly]
public class Day12 : AdventOfCode<long, IReadOnlyList<Day12Record>>
{
    public override IReadOnlyList<Day12Record> Parse(string input) =>
      input.Lines().Select(line => {
        var x = line.Split(" ");
        return new Day12Record(x[0], x[1].Split(",").Select(y => Convert.ToInt64(y)).ToList());
      }).ToList();
    
    [TestCase(Input.Sample, 21)]
    [TestCase(Input.Data, 7633)]
    public override long Part1(IReadOnlyList<Day12Record> data)
    {
      return data.Sum(d => CreateMatches(d.Template, d.Runs).Count());
    }

    [TestCase(Input.Sample, 525152)]
    [TestCase(Input.Data, 0)]
    public override long Part2(IReadOnlyList<Day12Record> data)
    {
      return data.Sum(d => {
        var d2 = new Day12Record(Enumerable.Repeat(d.Template, 5).Join("?"),
          Enumerable.Repeat(d.Runs, 5).SelectMany(it=>it).ToList());
        var x = CreateMatches(d2.Template, d2.Runs).Count();
        return x;
      });
    }


    public IEnumerable<string> CreateMatches(string template, List<long> runs)
    {
      var t = runs[0];
      var myPattern = Enumerable.Repeat('#', (int)t).Join();
      var isLast = runs.Count == 1;
      if (!isLast) myPattern += '.';
      var minNext = isLast ? 0 : runs.Skip(1).Sum() + runs.Skip(1).Count() - 1;
      var available = template.Length - minNext;
      for(var spaces = 0; spaces + myPattern.Length <= available; spaces++)
      {
        var prefix = Enumerable.Repeat('.', spaces).Join();
        if (Matches(template, prefix + myPattern))
        {
          var remainder = template[(prefix+myPattern).Length..];
          if (!isLast)
            foreach(var item in CreateMatches(remainder, runs.Skip(1).ToList())) yield return prefix + myPattern + item;
          else if (Matches(remainder, Enumerable.Repeat('.', remainder.Length).Join()))
            yield return prefix + myPattern;
        }
      }
    }

    public bool Matches(string template, string pattern)
    {
      foreach(var (a,b) in template.Take(pattern.Length).Zip(pattern))
      {
        if (a == '?') continue;
        if (a != b) return false;
      }
      return true;
    }
}


