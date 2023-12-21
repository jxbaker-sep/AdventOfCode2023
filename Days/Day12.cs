using AdventOfCode2023.Utils;
using System;
using System.Collections.Generic;
using System.Linq;

namespace AdventOfCode2023.Day12;

public record Day12Record(string Template, List<int> Runs);


public class Day12 : AdventOfCode<long, IReadOnlyList<Day12Record>>
{
    public override IReadOnlyList<Day12Record> Parse(string input) =>
      input.Lines().Select(line => {
        var x = line.Split(" ");
        return new Day12Record(x[0], x[1].Split(",").Select(y => Convert.ToInt32(y)).ToList());
      }).ToList();
    
    [TestCase(Input.Sample, 21)]
    [TestCase(Input.Data, 7633)]
    public override long Part1(IReadOnlyList<Day12Record> data)
    {
      return data.Sum(d => CountMatches(d.Template, d.Runs));
    }

    [TestCase(Input.Sample, 525152)]
    [TestCase(Input.Data, 23_903_579_139_437)]
    public override long Part2(IReadOnlyList<Day12Record> data)
    {
      return data.Sum(d => {
        var template = Enumerable.Repeat(d.Template, 5).Join("?");
        var runs = Enumerable.Repeat(d.Runs, 5).SelectMany(Helper.Identity).ToList();
        var x = CountMatches(template, runs);
        return x;
      });
    }

    public Dictionary<(string, string), long> MyMemoise = new();

    public long CountMatches(string template, List<int> runs)
    {
      var key = (template, runs.Select(it => $"{it}").Join(","));
      if (MyMemoise.TryGetValue(key, out var value)) return value;
      var x = CountMatchesImpl(template, runs);
      MyMemoise[key] = x;
      return x;
    }

    public long CountMatchesImpl(string template, List<int> runs)
    {
      if (template == "" && runs.Count == 0) return 1;

      if (template == "") return 0;

      if (template[0] == '.') return CountMatches(template[1..], runs);

      if (template[0] == '?') return CountMatches("." + template[1..], runs) + CountMatches("#" + template[1..], runs);

      // We know it's a # here.
      if (runs.Count == 0) return 0;

      var isLast = runs.Count == 1;
      
      if (template.Length < runs[0] + (isLast ? 0 : 1)) return 0;

      if (template.Take(runs[0]).TakeWhile(MatchesHash).Count() == runs[0]) 
      {
        if (isLast)
          return CountMatches(template[runs[0]..], runs.Skip(1).ToList());
        else if ( MatchesDot(template[runs[0]]))
          return CountMatches(template[(runs[0] + 1)..], runs.Skip(1).ToList());
      }

      return 0;
    }

    public static bool MatchesDot(char c) => c == '.' || c == '?';
    public static bool MatchesHash(char c) => c == '#' || c == '?';
}


