using AdventOfCode2023.Utils;
using System;
using System.Collections.Generic;
using System.Data;
using System.IO;
using System.Linq;

namespace AdventOfCode2023.Day25;

using Connections = IReadOnlySet<Connection>;
using ConnectionMap = IReadOnlyDictionary<string, IReadOnlyList<string>>;

public class Day25 : AdventOfCode<long, Connections>
{
    public override Connections Parse(string input) {
      var result = new HashSet<Connection>();

      foreach(var line in input.Lines())
      {
        var x = line.Split(":", StringSplitOptions.TrimEntries);
        var ys = x[1].Split(" ", StringSplitOptions.TrimEntries);
        foreach(var y in ys) result.Add(NormalizedConnection(x[0], y));
      }
      return result;
    }

    [TestCase(Input.Sample, 54)]
    [TestCase(Input.Data, 525264)]
    public override long Part1(Connections connections)
    {
      var map = CreateConnectionMap(connections);
      Dictionary<Connection, long> counts = new();
      foreach(var node in map.Keys)
      {
        FindShortestPaths(map, node, counts);
      }
      var x = counts.OrderByDescending(it => it.Value).ToList();

      var connectionMap = CreateConnectionMap(connections.Except(x.Take(3).Select(it=>it.Key)));
      var groups = FindConnectedGroups(connectionMap);
      if (groups.Count != 2) throw new ApplicationException();
      return groups[0].LongCount() * groups[1].LongCount();
    }

    // [TestCase(Input.Sample, 154)]
    // [TestCase(Input.Data, 6246)] 6246 correct, takes 101.6s to generate
    public override long Part2(Connections connections)
    {
      return 0;
    }

    public void FindShortestPaths(ConnectionMap connections, string start, Dictionary<Connection, long> counts)
    {
      var open = connections[start].Select(it => (End: it, Path: new[]{NormalizedConnection(start, it)}.ToList() as IReadOnlyList<Connection>)).ToQueue();
      var closed = new[]{start}.ToHashSet();
      while (open.TryDequeue(out var current))
      {
        foreach(var step in current.Path)
        {
          counts[step] = counts.GetValueOrDefault(step) + 1;
        }
        foreach(var next in connections[current.End])
        {
          if (closed.Add(next)) open.Enqueue((next, current.Path.Append(NormalizedConnection(current.End, next)).ToList()));
        }
      }
    }

    private IReadOnlyList<IReadOnlySet<string>> FindConnectedGroups(ConnectionMap connectionMap)
    {
        var ungrouped = connectionMap.Keys.ToHashSet();
        var result = new List<IReadOnlySet<string>>();

        while (ungrouped.Any())
        {
          if (result.Count == 2) return [];
          var first = ungrouped.First();
          var l = new HashSet<string>();
          var open = new[]{first}.ToQueue();
          while (open.TryDequeue(out var item))
          {
            ungrouped.Remove(item);
            if (l.Add(item))
            {
              foreach(var next in connectionMap[item]) open.Enqueue(next);
            }
          }
          result.Add(l);
        }

        return result;
    }


    public static Connection NormalizedConnection(string e1, string e2)
    {
      if (e1.CompareTo(e2) <= 0) return new Connection(e1, e2);
      return new Connection(e2, e1);
    }

    public static ConnectionMap CreateConnectionMap(IEnumerable<Connection> connections)
    {
      var result = new Dictionary<string, List<string>>();

      foreach(var connection in connections)
      {
        result.AddToList(connection.First, connection.Second);
        result.AddToList(connection.Second, connection.First);
      }

      return result.ToDictionary(it => it.Key, it => it.Value as IReadOnlyList<string>);
    }
}
public record Connection(string First, string Second);