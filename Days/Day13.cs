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
      return data.Select(d => FindReflections(d).Single()).Sum();
    }

    [TestCase(Input.Sample, 400)]
    [TestCase(Input.Data, 37876)]
    public override long Part2(Day13Data data)
    {
      return data.Select(d => FindSmudgedReflection(d)).Sum();
    }

    public long FindSmudgedReflection(IReadOnlyList<string> rows)
    {
      var originalReflection = FindReflections(rows).Single();
      for(var row = 0; row < rows.Count; row += 1)
      {
        for (var col = 0; col < rows[0].Length; col += 1)
        {
          var smudged = rows.ToList();
          smudged[row] = smudged[row][..col] + Smudge(smudged[row][col]) + smudged[row][(col+1)..];
          try
          {
            foreach(var x in FindReflections(smudged))
              if (x != originalReflection)
                return x;
          }
          catch(ApplicationException) {}
        }
      }
      throw new ApplicationException();
    }

    private string Smudge(char v)
    {
        return v switch
        { 
          '.' => "#", 
          '#' => ".", 
          _ => throw new ApplicationException() 
        };
    }

    public IEnumerable<long> FindReflections(IReadOnlyList<string> rows, bool isFlipped = false)
    {
      for(var i = 1; i < rows.Count; i++)
      {
        var found = true;
        for (int n = 0; n + i < rows.Count && i - 1 - n >= 0; n++)
        {
          if (rows[i + n] != rows[i - 1 - n]) { found = false; break; } 
        }
        if (found) yield return i * 100;
      }

      if (isFlipped) yield break;
      var inverted = Invert(rows);
      foreach(var x in FindReflections(inverted, true)) yield return x / 100;
    }

    public IReadOnlyList<string> Invert(IReadOnlyList<string> rows)
    {
      var result = new List<string>();
      for (var col = 0; col < rows[0].Length; col++)
      {
        result.Add(rows.Select(row => row[col]).Join());
      }
      return result;
    }
}


