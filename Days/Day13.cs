using AdventOfCode2023.Utils;
using JetBrains.Annotations;
using System;
using System.Collections.Generic;
using System.Linq;

namespace AdventOfCode2023.Day13;

using Day13Data = IReadOnlyList<IReadOnlyList<string>>;

[UsedImplicitly]
public class Day13 : AdventOfCode<long, Day13Data>
{
    public override Day13Data Parse(string input) =>input.Paragraphs().ToList();
    
    [TestCase(Input.Sample, 405)]
    [TestCase(Input.Data, 30802)]
    public override long Part1(Day13Data data)
    {
      return data.Select(d => FindReflections(d, false).First()).Sum();
    }

    [TestCase(Input.Sample, 400)]
    [TestCase(Input.Data, 37876)]
    public override long Part2(Day13Data data)
    {
      return data.Select(d => FindSmudgedReflection(d)).Sum();
    }

    public long FindSmudgedReflection(IReadOnlyList<string> rows)
    {
      var originalReflection = FindReflections(rows, false).First();
      foreach(var x in FindReflections(rows, true))
      {
        if (x != originalReflection) return x;
      }
      throw new ApplicationException();
    }

    public IEnumerable<long> FindReflections(IReadOnlyList<string> rows, bool allowOneSmudge)
    {
      foreach(var x in FindReflectionsImpl(rows, allowOneSmudge)) yield return x * 100;
      foreach(var x in FindReflectionsImpl(Invert(rows), allowOneSmudge)) yield return x;
    }

    public static IEnumerable<long> FindReflectionsImpl(IReadOnlyList<string> rows, bool allowOneSmudge)
    {
      for(var i = 1; i < rows.Count; i++)
      {
        var found = true;
        var smudges = 0;
        for (int n = 0; n + i < rows.Count && i - 1 - n >= 0; n++)
        {
          var diffCount = rows[i + n].Zip(rows[i - 1 - n]).Where(it => it.First != it.Second).Count();
          if (diffCount == 0) continue;
          smudges += diffCount;
          if (allowOneSmudge && smudges == 1) continue;
          found = false;
          break;
        }
        if (found) yield return i;
      }
    }

    public static IReadOnlyList<string> Invert(IReadOnlyList<string> rows)
    {
      var result = new List<string>();
      for (var col = 0; col < rows[0].Length; col++)
      {
        result.Add(rows.Select(row => row[col]).Join());
      }
      return result;
    }
}


