using AdventOfCode2023.Utils;
using System;
using System.Collections.Generic;
using System.Linq;
using TypeParser;

namespace AdventOfCode2023.Day04;

public class Day04 : AdventOfCode<long,List<Card>>
{
    public override List<Card> Parse(string input) => TypeCompiler.ParseLines<Card>(input);

    [TestCase(Input.Sample, 13)]
    [TestCase(Input.Data, 32_001)]
    public override long Part1(List<Card> cards)
    {
        return cards.Sum(card => {
            var score = card.Score();
            return score == 0 ? 0 : LMath.Pow(2, score-1);
        });
    }

    [TestCase(Input.Sample, 30)]
    [TestCase(Input.Data, 5_037_841)]
    public override long Part2(List<Card> cards)
    {
        return ScoreCards(cards, cards.Select(card => card.Number).ToList());
    }

    private static long ScoreCards(IReadOnlyList<Card> cards, IReadOnlyList<long> pile)
    {
        var open = pile.ToQueue();
        long count = 0;
        while (open.TryDequeue(out var next))
        {
            count += ScoreCard(cards, next);
        }
        return count;
    }

    private static Func<IReadOnlyList<Card>, long, long> ScoreCard {get;} = Memoise.Create<IReadOnlyList<Card>, long, long>(ScoreCardImpl);

    private static long ScoreCardImpl(IReadOnlyList<Card> cards, long number)
    {
        var score = cards[(int)number - 1].Score();
        if (score == 0) return 1;
        return 1 + ScoreCards(cards, cards.Skip((int)number).Take((int)score).Select(card => card.Number).ToList());
    }
}

public record Card(
    [Format(Before="Card", After =":")]long Number, 
    [Format(Terminator = "|")]IReadOnlyList<long> WinningNumbers, 
    IReadOnlyList<long> NumbersYouHave);

public static class CardExtensions {
    public static long Score(this Card card)
    {
        return card.WinningNumbers.Intersect(card.NumbersYouHave).Count();
    }
}