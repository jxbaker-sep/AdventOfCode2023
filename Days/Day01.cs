using AdventOfCode2023.Day04;
using AdventOfCode2023.Utils;
using JetBrains.Annotations;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;

namespace AdventOfCode2023.Day01;

using Board = IReadOnlyList<IReadOnlyList<char>>;

[UsedImplicitly]
public class Day01 : AdventOfCode<long, GameState>
{
    public readonly Position Goal = new(0, 8);
    public readonly Position Start = new(9, 8);

    public override GameState Parse(string input) {
        var snakes = new List<Position>();
        var board = input.Lines().Select((line, row) => line.WithIndices().Aggregate(new List<char>(), (accum, c) =>
            {
                if (c.Value == '~') {snakes.Add(new Position(row, c.Index)); accum.Add(Empty);}
                else if (c.Value == 'P' || c.Value == 'G') accum.Add(Empty);
                else accum.Add(c.Value);
                return accum;
            })).ToList();
        return new GameState(Start, 0, snakes, board);
    }

    [TestCase(Input.Sample, 0, N = 2)]
    public override long Part1(GameState input)
    {
        FindPath(input);
        return 0;
    }

    public readonly char Empty = '.';
    public readonly char Wall = '#';
    public readonly char Sliding = 's';
    public readonly char Exploding = 'e';
    public readonly char Movable = 'm';
    public readonly char RockBomb = '*';
    public readonly char BrownWall = 'b';
    public readonly char BrownButton = 'B';
    public readonly char Snake = '~';


    private void FindPath(GameState start)
    {
        var open = new[]{new Move(start, "Start")}.ToQueue();
        var closed = new HashSet<string>{CreateKey(start)};
        while (open.TryDequeue(out var current))
        {
            foreach(var next in Open(current.GameState))
            {
                if (next.GameState.Player == Goal)
                {
                    PrintGameState(current.Description + "\n" + next.Description);
                    return;
                }
                var key = CreateKey(next.GameState);
                if (closed.Add(key))
                {
                    open.Enqueue(new Move(next.GameState, current.Description + "\n" + next.Description));
                }
            }
        }
        throw new ApplicationException();
    }

    private void PrintGameState(string description)
    {
        var ds = description.Split("\n");
        var tail = "";
        var count = 0;
        Console.WriteLine();
        foreach(var d in ds)
        {
            if (d != tail && count > 0)
            {
                Console.WriteLine($"{tail} x{count}");
                count = 0;
            }
            count += 1;
            tail = d;
        }
        if (count > 0)
        {
            Console.WriteLine($"{tail} x{count}");
        }
    }

    private IEnumerable<Move> Open(GameState gameState)
    {
        foreach(var next in Open2(gameState))
            foreach(var movedSnake in MoveSnakes(next))
                yield return movedSnake;
    }

    private IEnumerable<Move> MoveSnakes(Move move)
    {
        var player = move.GameState.Player;
        var board = move.GameState.Board;
        var newSnakes = new List<Position>();
        foreach(var snake_ in move.GameState.Snakes)
        {
            var snake = snake_;
            var moved = true;
            while (moved)
            {
                moved = false;
                var dx = (snake.X < player.X) ? Vector.East : (snake.X > player.X ? Vector.West : Vector.Zero);
                if (snake + dx == player) yield break;
                if (board.At(snake + dx) == Empty && dx != Vector.Zero)
                {
                    snake += dx;
                    moved = true;
                }
                var dy = (snake.Y < player.Y) ? Vector.South : (snake.Y > player.Y ? Vector.North : Vector.Zero);
                if (snake + dy == player) yield break;
                if (board.At(snake + dy) == Empty && dy != Vector.Zero)
                {
                    snake += dy;
                    moved = true;
                }
            }
            newSnakes.Add(snake);
        }
        yield return move with {GameState = move.GameState with {Snakes = newSnakes}};
    }

    private IEnumerable<Move> Open2(GameState gameState)
    {
        var p = gameState.Player;
        foreach(var vector in new []{Vector.North, Vector.East, Vector.West, Vector.South})
        {
            var next = p + vector;
            if (next.Y < 0 || next.Y >= gameState.Board.Count) continue;
            if (next.X < 0 || next.X >= gameState.Board[0].Count) continue;
            var c = gameState.Board[(int)next.Y][(int)next.X];
            if (c == Empty)
            {
                yield return new Move(gameState with {Player = next}, VectorWord(vector));
            }
            else if (c == Wall) continue;
            else if (c == BrownWall) continue;
            else if (c == BrownButton)
            {
                var z = Copy(gameState.Board);
                for(var row = 0; row < gameState.Board.Count; row++)
                    for(var col = 0; col < gameState.Board[0].Count; col++)
                        if (z[row][col] == BrownWall || z[row][col] == BrownButton)
                            z[row][col] = Empty;
                yield return new Move(gameState with{Board = z}, $"Explode All Browns");
            }
            else if (c == Movable || c == Sliding) {
                var next2 = next + vector;
                if (!gameState.Board.TryAt(next2, out var c2)) continue;
                if (c2 != Empty) continue;
                if (c == Sliding) while (gameState.Board.TryAt(next2 + vector, out var temp) && temp == Empty) next2 += vector;
                var z = Copy(gameState.Board);
                z.Set(next2, c);
                z.Set(next, Empty);
                yield return new Move(gameState with{Board = z}, $"Push {VectorWord(vector)}");
            }
            else if (c == Exploding)
            {
                if (gameState.RockBombs == 0) continue;
                var z = Copy(gameState.Board);
                z.Set(next, Empty);
                yield return new Move(gameState with {Board=z, RockBombs = gameState.RockBombs-1}, $"Explode {VectorWord(vector)}");
            }
            else if (c == '*')
            {
                var z = Copy(gameState.Board);
                z.Set(next, Empty);
                yield return new Move(gameState with {Board = z, RockBombs = gameState.RockBombs + 1}, "Pick up rock bomb.");
            }
            else throw new ApplicationException($"Unkown c == {c}");
        }
    }

    private string VectorWord(Vector vector)
    {
        if (vector == Vector.East) return "Right";
        if (vector == Vector.West) return "Left";
        if (vector == Vector.North) return "Up";
        if (vector == Vector.South) return "Down";
        throw new ApplicationException();
    }

    public override long Part2(GameState input) => 0;

    public List<List<char>> Copy(Board board) => board.Select(row => row.Select(c => c).ToList()).ToList();

    public string CreateKey(GameState board)
    {
        var z = Copy(board.Board); 
        z.Set(board.Player, 'P');
        foreach(var snake in board.Snakes)
        {
            z.Set(snake, '~');
        }
        return z.Select(it => it.Join()).Join("\n") + board.RockBombs;
    }
}

public record Move(GameState GameState, string Description);

public record GameState(Position Player, long RockBombs, IReadOnlyList<Position> Snakes, Board Board);
