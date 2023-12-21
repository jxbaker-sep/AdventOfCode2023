using AdventOfCode2023.Utils;
using System;
using System.Collections.Generic;
using System.Data;
using System.Linq;
using System.Text.RegularExpressions;

namespace AdventOfCode2023.Day20;

using Day20Data = IReadOnlyDictionary<string, Module>;

public record Module(string Type, string Name, IReadOnlyList<string> Outputs, string State);


public class Day20 : AdventOfCode<long, Day20Data>
{
  const string FlipFlop = "%";
  const string Conjunction = "&";
  const string Low = "low";
  const string High = "high";
  const string Off = "off";
  const string On = "on";

    public override Day20Data Parse(string input) {
      var world = input.Lines().Select(line => {
        var rx = Regex.Match(line, @"^(?<type>(%|&)?)(?<name>\w+) -> (?<follows>.*)$");
        if (!rx.Success) throw new ApplicationException();
        var type = rx.OptionalStringGroup("type") ?? "";
        var name = rx.StringGroup("name");
        var follows = rx.StringGroup("follows").Split(",").Select(it=>it.Trim()).ToList();

        var state = "";
        if (type == FlipFlop) state = Off;

        return new Module(type, name, follows, state);
      }).ToDictionary(it => it.Name, it => it);

      foreach(var name in world.Values.Where(it => it.Type == Conjunction).Select(it => it.Name))
      {
        var inputs = world.Values.Where(it => it.Outputs.Contains(name)).Select(it => it.Name).ToList();
        var state = inputs.Select(it => $"{it}:{Low}").Join(",");
        world[name] = world[name] with {State = state};
      }

      return world;
    }

    [TestCase(Input.Sample, 32000000, N = 1)]
    [TestCase(Input.Sample, 11687500, N = 2)]
    [TestCase(Input.Data, 807069600)]
    public override long Part1(Day20Data world)
    {
      var countLow = 0L;
      var countHigh = 0L;
      for(var i = 0; i < 1000; i++)
      {
        var (n, n2, world2, _) = PushButton(world);
        countLow += n;
        countHigh += n2;
        world = world2;
      }
      return countLow * countHigh;
    }


    // [TestCase(Input.Sample, 167409079868000)]
    // [TestCase(Input.Data, 0)]
    public override long Part2(Day20Data world)
    {
      var count = 0L;
      while (true)
      {
        count += 1;
        if (count % 10000 == 0) Console.WriteLine(count);
        var (_, _, world2, found) = PushButton(world);
        if (found) return count;
        world = world2;
      }
      throw new ApplicationException();
    }

    (long, long, Day20Data, bool) PushButton(Day20Data oldWorld)
    {
      var world = oldWorld.ToDictionary(it => it.Key, it => it.Value);
      var open = new[]{(Destination: "broadcaster", Signal: Low, Origin: "button")}.ToQueue();
      var countLow = 0L;
      var countHigh = 0L;
      var rxWasLow = false;
      while (open.TryDequeue(out var current))
      {
        var signal = current.Signal;
        if (signal == Low) countLow += 1; else countHigh += 1;
        if (current.Destination == "rx" && signal == Low) rxWasLow = true;
        if (!world.ContainsKey(current.Destination)) continue; 
        var module = world[current.Destination];
        if (current.Destination == "broadcaster")
        {
          Send(open, module, signal);
        }
        else if (module.Type == FlipFlop)
        {
          if (signal == High) continue;
          if (module.State == Off)
          {
            Send(open, module, High);
            world[current.Destination] = module with {State = On};
            continue;
          }
          Send(open, module, Low);
          world[current.Destination] = module with {State = Off};
        }
        else if (module.Type == Conjunction)
        {
          var inputs = module.State.Split(",").Select(it => it.Split(":")).ToDictionary(it => it[0], it => it[1]);
          if (!inputs.ContainsKey(current.Origin)) throw new ApplicationException();
          inputs[current.Origin] = signal;
          if (inputs.Values.All(it => it == High)) Send(open, module, Low);
          else Send(open, module, High);
          world[current.Destination] = module with {State = inputs.Select(it => $"{it.Key}:{it.Value}").Join(",")};
        }
        else throw new ApplicationException();
      }

      return (countLow, countHigh, world, rxWasLow);
    }

    private void Send(Queue<(string Destination, string Signal, string Origin)> open, Module module, string signal)
    {
        foreach(var destination in module.Outputs)
        {
          open.Enqueue((destination, signal, module.Name));
        }
    }
}
