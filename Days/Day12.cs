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
      return data.Sum(d => CountMatchingTemplate(d));
    }

    // [TestCase(Input.Sample, 525152)]
    // [TestCase(Input.Data, 0)]
    public override long Part2(IReadOnlyList<Day12Record> data)
    {
      return data.Sum(d => {
        var d2 = new Day12Record(Enumerable.Repeat(d.Template, 5).Join(),
          Enumerable.Repeat(d.Runs, 5).SelectMany(it=>it).ToList());
        return CountMatchingTemplate(d2);
      });
    }


    public long CountMatchingTemplate(Day12Record data)
    {
      return Templates(data).Where(template => Matches(template, data.Template)).Count();
    }

    public bool Matches(string template, string test)
    {
      if (template.Length != test.Length) throw new ApplicationException();
      foreach(var (a,b) in template.Zip(test))
      {
        if (b == '?') continue;
        if (a != b) return false;
      }
      return true;
    }

    private IEnumerable<string> Templates(Day12Record data)
    {
      var goalLength = data.Template.Length;

      var runLength = (int)data.Runs.Sum();
      var numberOfRuns = data.Runs.Count;
      var spacesLength = goalLength - runLength ;

      foreach(var spaces in CreateSpaces( numberOfRuns + 1, spacesLength, new List<int>(), true))
      {
        yield return CreateTemplate(spaces, data.Runs);
      }
    }

    public IEnumerable<List<int>> CreateSpaces(int count, int sum, List<int> prefix, bool isFirst)
    {
      if (count < 0) throw new ApplicationException();
      if (count == 0) {
        yield return prefix;
        yield break;
      }
      if (count == 1)
      {
        prefix.Add(sum);
        yield return prefix;
        yield break;
      }
      if (count > 1) {
        if (sum == 0) throw new ApplicationException();
        var remainingMandatorySpaces = count - 2;
        for(var x = sum - remainingMandatorySpaces; x >= (isFirst ? 0 : 1); x--)
        {
          var l = prefix.ToList();
          l.Add(x);
          foreach(var sub in CreateSpaces(count-1, sum - x, l, false)) yield return sub;
        }
      }
    }

    public string CreateTemplate(List<int> spaces, List<long> runs)
    {
      var result = "";
      result += Enumerable.Repeat(".", spaces[0]).Join();
      foreach (var (r,s) in runs.Zip(spaces.Skip(1), (a,b)=>(a,b)))
      {
        result += Enumerable.Repeat("#", (int)r).Join();
        result += Enumerable.Repeat(".", s).Join();
      }
      return result;
    }
}


