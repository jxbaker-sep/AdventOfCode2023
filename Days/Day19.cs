using AdventOfCode2023.Utils;
using JetBrains.Annotations;
using Microsoft.VisualBasic;
using System;
using System.Collections.Generic;
using System.Data;
using System.Formats.Asn1;
using System.Linq;
using System.Text.RegularExpressions;

namespace AdventOfCode2023.Day19;

using Day19Data = World;
using Workflows = IReadOnlyDictionary<string, Workflow>;

public record World(Workflows Workflows, IReadOnlyList<Part> Parts);
public record Condition(char Field, char Operator, long Value);
public record Rule(Condition? Condition, string Destination);
public record Workflow(string Name, IReadOnlyList<Rule> Rules);
public record Part(long X, long M, long A, long S);

public record DigInstruction(Vector Direction, long Length, string Color);

[UsedImplicitly]
public class Day19 : AdventOfCode<long, Day19Data>
{
    public override Day19Data Parse(string input)
    {
      var ps = input.Paragraphs().ToList();
      var workflows = ps[0].Select(ParseWorkflow).ToDictionary(it => it.Name, it => it);
      var parts = ps[1].Select(ParsePart).ToList();
      return new World(workflows, parts);
    }

    private Part ParsePart(string line)
    {
        var m = Regex.Match(line, @"\D*(?<x>\d+)\D*(?<m>\d+)\D*(?<a>\d+)\D*(?<s>\d+)");
        return new Part(m.LongGroup("x"), m.LongGroup("m"), m.LongGroup("a"), m.LongGroup("s"));
    }

    private Workflow ParseWorkflow(string line)
    {
      line = line[..^1];
      var p = line.Split('{');
      var name = p[0];
      var rules = new List<Rule>();
      var r = p[1].Split(',');
      foreach(var rule in r)
      {
        var m = Regex.Match(rule, @"(?<Field>[axms])(?<Operator>[<>])(?<Value>\d+):(?<Dest>.*)");
        if (m.Success)
        {
          var condition = new Condition(m.StringGroup("Field")[0], m.StringGroup("Operator")[0], m.LongGroup("Value"));
          rules.Add(new Rule(condition, m.StringGroup("Dest")));
        }
        else
        {
          rules.Add(new Rule(null, rule));
        }
      }
      return new Workflow(name, rules);
    }

    [TestCase(Input.Sample, 19114)]
    [TestCase(Input.Data, 352052)]
    public override long Part1(Day19Data world)
    {
        var workflows = world.Workflows;
        var parts = world.Parts;

        return parts.Where(p => Accept(workflows, p)).Sum(p => p.X + p.M + p.S + p.A);
    }


    [TestCase(Input.Sample, 167409079868000)]
    [TestCase(Input.Data, 116606738659695)]
    public override long Part2(Day19Data world)
    {
      var tps = new HashSet<TheoreticalPart>();
      tps = MatchTheoreticalPart(world.Workflows, "in", new TheoreticalPart(new Part(1,1,1,1), new Part(4000, 4000, 4000, 4000))).ToHashSet();

      var workflows = tps.Pairs()
        .Select(p => Overlap(p.First, p.Second))
        .OfType<TheoreticalPart>()
        .Select(CreateExclusionWorkflow)
        .ToList();

      foreach(var workflow in workflows)
      {
        tps = tps.SelectMany(tp => MatchTheoreticalPart(workflow, "z1", tp)).ToHashSet();
      }

      return tps.Sum(it => (it.Max.A - it.Min.A + 1)*(it.Max.X - it.Min.X + 1)*(it.Max.M - it.Min.M + 1)*(it.Max.S - it.Min.S + 1));
    }

    public Workflows CreateExclusionWorkflow(TheoreticalPart p)
    {
      var rules = new List<Rule>
      {
          new(new('x', '<', p.Min.X), "A"),
          new(new('x', '>', p.Max.X), "A"),
          new(new('m', '<', p.Min.M), "A"),
          new(new('m', '>', p.Max.M), "A"),
          new(new('a', '<', p.Min.A), "A"),
          new(new('a', '>', p.Max.A), "A"),
          new(new('s', '<', p.Min.S), "A"),
          new(new('s', '>', p.Max.S), "A"),
          new(null, "R")
      };

      var wf = new Workflow("z1", rules);
      return new Dictionary<string, Workflow>{{wf.Name, wf}};
    }

    public TheoreticalPart? Overlap(TheoreticalPart p1, TheoreticalPart p2)
    {
      var result = new TheoreticalPart(
        new Part(
          Math.Max(p1.Min.X, p2.Min.X),
          Math.Max(p1.Min.M, p2.Min.M),
          Math.Max(p1.Min.A, p2.Min.A),
          Math.Max(p1.Min.S, p2.Min.S)
        ),
        new Part(
          Math.Min(p1.Max.X, p2.Max.X),
          Math.Min(p1.Max.M, p2.Max.M),
          Math.Min(p1.Max.A, p2.Max.A),
          Math.Min(p1.Max.S, p2.Max.S)
        )
      );
      if (result.Min.X <= result.Max.X &&
          result.Min.M <= result.Max.M &&
          result.Min.A <= result.Max.A &&
          result.Min.S <= result.Max.S)
        return result;
      return null;
    }

    IEnumerable<TheoreticalPart> SplitOverlaps(TheoreticalPart p1, TheoreticalPart p2)
    {
      var splitCount = 0;
      foreach(var field in new[]{'a', 'x', 'm', 's'})
      {
        var (left, right) = GetMin(p1, field) < GetMin(p1, field) ? (p1, p2) : (p2, p1);

        var minLeft = GetMin(left, field);
        var minRight = GetMin(right, field);
        var maxLeft = GetMax(left, field);
        var maxRight = GetMax(right, field);

        if (maxLeft < minRight)
        {
          continue;
        }
        splitCount += 1;

        if (minLeft < minRight) // area of left before right
        {
          yield return SetMax(left, field, minRight-1);
        }

        if (maxLeft > maxRight) // area of left after right
        {
          yield return SetMin(left, field, maxRight +1);
        }

        if (maxRight > maxLeft) // area of right after left
        {
          yield return SetMin(right, field, maxLeft +1);
        }

        // area of overlap
        var x = minRight;
        var y = new[]{maxRight, maxLeft}.Min();
        var otherFields = new[]{'a', 'x', 'm', 's'}.Where(it => it != field).ToList();
        var result = new TheoreticalPart(new Part(1,1,1,1), new Part(4000,4000,4000,4000));
        result = SetMin(result, field, x);
        result = SetMax(result, field, y);
        foreach(var f in otherFields)
        {
          result = SetMin(result, f, new[]{GetMin(left, f), GetMin(right, f)}.Min());
          result = SetMax(result, f, new[]{GetMax(left, f), GetMax(right, f)}.Min());
        }
        yield return result;
      }

      if (splitCount == 0)
      {
        yield return p1;
        yield return p2;
      }
    }

    public record TheoreticalPart(Part Min, Part Max);

    public IEnumerable<TheoreticalPart> MatchTheoreticalPart(Workflows workflows, string current, TheoreticalPart part)
    {
      var workflow = workflows[current];
      foreach(var rule in workflow.Rules)
      {
        var results = NarrowAndApply(workflows, rule, part);
        foreach(var result in results) yield return result;
        part = ReverseRule(rule.Condition, part);
      }
    }

    public TheoreticalPart ReverseRule(Condition? c, TheoreticalPart part)
    {
      if (c == null) return part; // shouldnt matter because non-conditional rule is last
      if (c.Operator == '<')
      {
        var newRule = new Condition(c.Field, '>', c.Value - 1);
        return Narrow(newRule, part) ?? throw new ApplicationException();
      }
      else if (c.Operator == '>')
      {
        var newRule = new Condition(c.Field, '<', c.Value + 1);
        return Narrow(newRule, part) ?? throw new ApplicationException();
      }
      throw new ApplicationException();
    }

    public IEnumerable<TheoreticalPart> NarrowAndApply(Workflows workflows, Rule rule, TheoreticalPart part)
    {
      var c = rule.Condition;
      var p = Narrow(c, part);
      if (p != null)
      {
        if (rule.Destination == "A") yield return p;
        else if (rule.Destination == "R") yield break;
        else foreach(var sub in MatchTheoreticalPart(workflows, rule.Destination, p)) yield return sub;
      }
    }

    private TheoreticalPart? Narrow(Condition? c, TheoreticalPart part)
    {
        if (c == null) return part;
        var max = GetMax(part, c.Field);
        var min = GetMin(part, c.Field);
        if (c.Operator == '<')
        {
          if (min >= c.Value) return null;
          max = new[]{max, c.Value-1}.Min();
          if (min > max) return null;
          return SetMax(part, c.Field, max);
        }
        else if (c.Operator == '>')
        {
          if (max <= c.Value) return null;
          min = new[]{min, c.Value+1}.Max();
          if (min > max) return null;
          return SetMin(part, c.Field, min);
        }
        throw new ApplicationException();
    }

    public bool Accept(Workflows workflows, Part p)
    {
      var current = "in";
      while (current != "A" && current != "R")
      {
        var workflow = workflows[current];
        foreach(var rule in workflow.Rules)
        {
          if (Match(rule.Condition, p)) {
            current = rule.Destination;
            break;
          }
        }
      }
      return current == "A";
    }

    private bool Match(Condition? condition, Part p)
    {
      if (condition is null) return true;
      if (condition.Operator == '<')
      {
        return GetField(p, condition.Field) < condition.Value;
      }
      else if (condition.Operator == '>')
      {
        return GetField(p, condition.Field) > condition.Value;
      }
      throw new ApplicationException();
    }

    private long GetField(Part p, char field)
    {
        return field switch
        {
          'x' => p.X,
          'a' => p.A,
          's' => p.S,
          'm' => p.M,
          _ => throw new ApplicationException()
        };
    }

    private Part SetField(Part p, char field, long value)
    {
        return field switch
        {
          'x' => p with {X = value},
          'a' => p with {A = value},
          's' => p with {S = value},
          'm' => p with {M = value},
          _ => throw new ApplicationException()
        };
    }

    private long GetMin(TheoreticalPart p, char field) => GetField(p.Min, field);
    private long GetMax(TheoreticalPart p, char field) => GetField(p.Max, field);

    private TheoreticalPart SetMin(TheoreticalPart p, char field, long value) => p with {Min = SetField(p.Min, field, value)};
    private TheoreticalPart SetMax(TheoreticalPart p, char field, long value) => p with {Max = SetField(p.Max, field, value)};
}
