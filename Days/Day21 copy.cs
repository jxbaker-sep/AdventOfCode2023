using AdventOfCode2023.Utils;
using System;
using System.Collections.Generic;
using System.Data;
using System.Linq;

namespace AdventOfCode2023.Day22;

using Grid3d = Dictionary<Position3d, Brick>;

public record Brick(Position3d P1, Position3d P2);

public class Day22 : AdventOfCode<long, Grid3d>
{
    public override Grid3d Parse(string input) {
      var bricks = input.Lines().Select(line => {
        var x = line.Split("~").SelectMany(it => it.Split(",")).Select(it => Convert.ToInt64(it)).ToList();
        return new Brick(new Position3d(x[0], x[1], x[2]), new Position3d(x[3], x[4], x[5]));
      }).ToList();

      var grid = new Grid3d();
      foreach(var brick in bricks) Emplace(grid, brick);
      return grid;
    }

    [TestCase(Input.Sample, 5)]
    [TestCase(Input.Data, 501)]
    public override long Part1(Grid3d grid)
    {
      Settle(grid);
      var (Network, ReverseNetwork) = CreateSupportNetwork(grid);
      var bricks = grid.Values.Distinct();
      return bricks.Count(brick => CanBeDeleted(brick, Network, ReverseNetwork));
    }

    [TestCase(Input.Sample, 7)]
    [TestCase(Input.Data, 80948)]
    public override long Part2(Grid3d grid)
    {
      Settle(grid);
      var (Network, ReverseNetwork) = CreateSupportNetwork(grid);
      var bricks = grid.Values.Distinct().ToList();
      return bricks.Sum(brick => {
        var hs = new HashSet<Brick>();
        BricksFall(brick, Network, ReverseNetwork, hs);
        return hs.Count() - 1;
      });
    }

    private void BricksFall(Brick brick, IReadOnlyDictionary<Brick, IReadOnlyList<Brick>> network, IReadOnlyDictionary<Brick, IReadOnlyList<Brick>> reverseNetwork,
      HashSet<Brick> falling)
    {
      falling.Add(brick);
      var supported = network[brick];

      var canFall = supported.Where(b => !reverseNetwork[b].Except(falling).Any()).ToList();
      foreach(var b in canFall) falling.Add(b);
      foreach(var b in canFall)
      {
        BricksFall(b, network, reverseNetwork, falling);
      }
    }

    private bool CanBeDeleted(Brick brick, IReadOnlyDictionary<Brick, IReadOnlyList<Brick>> network,
      IReadOnlyDictionary<Brick, IReadOnlyList<Brick>> reverseNetwork)
    {
      var supported = network[brick];
      if (supported.Count == 0) return true;
      foreach(var b in supported)
      {
        if (reverseNetwork[b].Count == 1 && !OnGround(b)) return false;
      }
      return true;
    }

    // return map: brick B to bricks directly resting on B 
    public (IReadOnlyDictionary<Brick, IReadOnlyList<Brick>> Network,
      IReadOnlyDictionary<Brick, IReadOnlyList<Brick>> ReverseNetwork) CreateSupportNetwork(Grid3d grid)
    {
      var result = new Dictionary<Brick, IReadOnlyList<Brick>>();
      var reverseNetwork = new Dictionary<Brick, List<Brick>>();

      var bricks = grid.Values.Distinct();
      foreach(var brick in bricks)
      {
        var l = brick.TopPoints().Select(p => {
            return p with {Z = p.Z + 1};
          })
          .Select(p => grid.TryGetValue(p, out var brick2) ? brick2 : null)
          .OfType<Brick>()
          .Distinct()
          .ToList();
        result[brick] = l;
        foreach(var b in l) reverseNetwork.InsertIntoList(b, brick);
      }

      return (result, reverseNetwork.ToDictionary(it => it.Key, it => (IReadOnlyList<Brick>)it.Value));
    }

    private void Settle(Grid3d grid)
    {
        var fell = true;
        while (fell)
        {
          fell = false;
          var bricks = grid.Values.Distinct().OrderBy(b => b.P1.Z);
          foreach(var brick in bricks)
          {
            var dZ = -1;
            while (CanFall(grid, brick, dZ))
            {
              dZ += -1;
            }
            dZ += 1;
            if (dZ != 0)
            {
              fell = true;
              foreach(var p in brick.Points())
              {
                grid.Remove(p);
              }
              Emplace(grid, brick with {P1 = brick.P1 with {Z = brick.P1.Z + dZ}, P2 = brick.P2 with {Z = brick.P2.Z + dZ}});
            }
          }
        }
    }

    public bool CanFall(Grid3d grid, Brick brick, long dZ)
    {
      if (brick.Bottom() + dZ < 0) return false;
      foreach(var p in brick.BottomPoints())
      {
        if (grid.ContainsKey(new Position3d(p.X, p.Y, p.Z + dZ))) return false;
      }
      return true;
    }

    public bool OnGround(Brick brick) => brick.Bottom() == 0;

    public void Emplace(Grid3d grid, Brick brick)
    {
      foreach(var p in brick.Points()) grid[p] = brick;
    }
}

public static class BrickExtensions
{
  public static IEnumerable<Position3d> Points(this Brick brick)
  {
    if (brick.P1.X > brick.P2.X) throw new ApplicationException();
    if (brick.P1.Y > brick.P2.Y) throw new ApplicationException();
    if (brick.P1.Z > brick.P2.Z) throw new ApplicationException();
    for(var x = brick.P1.X; x <= brick.P2.X; x++ )
    {
      for(var y = brick.P1.Y; y <= brick.P2.Y; y++ )
      {
        for(var z = brick.P1.Z; z <= brick.P2.Z; z++ )
        {
          yield return new Position3d(x,y,z);
        }
      }
    }
  }

  public static IEnumerable<Position3d> BottomPoints(this Brick brick)
  {
    if (brick.P1.X > brick.P2.X) throw new ApplicationException();
    if (brick.P1.Y > brick.P2.Y) throw new ApplicationException();
    if (brick.P1.Z > brick.P2.Z) throw new ApplicationException();
    for(var x = brick.P1.X; x <= brick.P2.X; x++ )
    {
      for(var y = brick.P1.Y; y <= brick.P2.Y; y++ )
      {
        yield return new Position3d(x,y,brick.Bottom());
      }
    }
  }

  public static IEnumerable<Position3d> TopPoints(this Brick brick)
  {
    if (brick.P1.X > brick.P2.X) throw new ApplicationException();
    if (brick.P1.Y > brick.P2.Y) throw new ApplicationException();
    if (brick.P1.Z > brick.P2.Z) throw new ApplicationException();
    for(var x = brick.P1.X; x <= brick.P2.X; x++ )
    {
      for(var y = brick.P1.Y; y <= brick.P2.Y; y++ )
      {
        yield return new Position3d(x,y,brick.Top());
      }
    }
  }
  public static long Bottom(this Brick b) => b.P1.Z;
  public static long Top(this Brick b) => b.P2.Z;
}