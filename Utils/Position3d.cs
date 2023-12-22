using System;
using System.Collections.Generic;
using System.Linq;

namespace AdventOfCode2023.Utils
{
    public record Position3d(long X, long Y, long Z)
    {
        public static Position3d operator -(Position3d p, Position3d p2)
        {
            return new Position3d(p.Y - p2.Y, p.X - p2.X, p.Z - p2.Z);
        }

        public static Position3d operator -(Position3d p)
        {
            return new Position3d(-p.Y, -p.X, -p.Z);
        }

        public static Position3d operator +(Position3d p, Position3d p2)
        {
            return new Position3d(p.Y + p2.Y, p.X + p2.X, p.Z + p2.Z);
        }

        public long ManhattanDistance()
        {
            return Math.Abs(X) + Math.Abs(Y) + Math.Abs(Z);
        }

        public long ManhattanDistance(Position3d other)
        {
            return (this - other).ManhattanDistance();
        }

        public override string ToString()
        {
            return $"({Y},{X},{Z})";
        }

        public Position3d RollUp() => new Position3d(-Z, X, Y);

        public Position3d RollLeft() => new Position3d(X, -Y, Z);

        public Position3d Rotate() => new Position3d(Y, Z, -X);

        public IEnumerable<Position3d> Orthoganals()
        {
            yield return new Position3d(Y + 1, X, Z);
            yield return new Position3d(Y - 1, X, Z);
            yield return new Position3d(Y, X + 1, Z);
            yield return new Position3d(Y, X - 1, Z);
            yield return new Position3d(Y, X, Z + 1);
            yield return new Position3d(Y, X, Z - 1);
        }
    }
}