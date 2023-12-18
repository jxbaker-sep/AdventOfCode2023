using AdventOfCode2023.Utils;
using JetBrains.Annotations;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading;

namespace AdventOfCode2023.Day15;

using Day15Data = IReadOnlyList<string>;

[UsedImplicitly]
public class Day15 : AdventOfCode<long, Day15Data>
{
    public override Day15Data Parse(string input) => input.Lines().Single().Split(",");
    
    [TestCase(Input.Sample, 1320)]
    [TestCase(Input.Data, 506269)]
    public override long Part1(Day15Data data)
    {
      return data.Sum(it => Hash(it));
    }


    [TestCase(Input.Sample, 145)]
    [TestCase(Input.Data, 264021)]
    public override long Part2(Day15Data data)
    {
      var boxes = Enumerable.Range(0, 256).Select(it => new List<(string Label, long FocusLength)>()).ToList();
      foreach(var instruction in data)
      {
        if (instruction.EndsWith("-")) {
          var label = instruction[..^1];
          Remove(boxes, label);
        }
        else {
          var x = instruction.Split('=');
          var label = x[0];
          var focusLength = Convert.ToInt64(x[1]);
          Add(boxes, label, focusLength);
        }
      }
      return boxes.SelectMany((box, boxIndex) => box.Select((lens, lensIndex) => (boxIndex + 1) * (lensIndex + 1) * lens.FocusLength))
        .Sum();
    }

    private void Remove(List<List<(string Label, long FocusLength)>> boxes, string label)
    {
        var id = Hash(label);
        boxes[id] = boxes[id].Where(it => it.Label != label).ToList();
    }

    private void Add(List<List<(string Label, long FocusLength)>> boxes, string label, long focusLength)
    {
        var box = boxes[Hash(label)];
        var optional = box.WithIndices().Where(it => it.Value.Label == label).ToList();
        if (optional.Count > 0) {
          var index = optional.Single().Index;
          box[index] = (label, focusLength);
        } else {
          box.Add((label, focusLength));
        }
    }

    private int Hash(string it) => it.Aggregate(0, (accum, c) => (accum + c) * 17 % 256);

}
