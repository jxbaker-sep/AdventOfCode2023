﻿using AdventOfCode2023.Utils;
using JetBrains.Annotations;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;

namespace AdventOfCode2023.Days.Day04;

[UsedImplicitly]
public class Day04 : AdventOfCode<long,List<Card>>
{
    public override List<Card> Parse(string input) => input.Lines().Select(line => {
        var m = Regex.Match(line, @"Card\s+(?<CardNumber>\d+): (?<Wins>(\s|\d+)+) \| (?<Haves>(\s|\d+)+)");
        if (!m.Success) throw new ApplicationException();
        var wins = m.StringGroup("Wins").Split(" ").Select(it => it.Trim()).Where(it => it != "").Select(it => Convert.ToInt64(it)).ToList();
        var haves = m.StringGroup("Haves").Split(" ").Select(it => it.Trim()).Where(it => it != "").Select(it => Convert.ToInt64(it)).ToList();
        return new Card(m.LongGroup("CardNumber"), wins, haves);
    }).ToList();

    [TestCase(Input.Example, 13)]
    [TestCase(Input.File, 32001)]
    public override long Part1(List<Card> cards)
    {
        return cards.Sum(card => {
            var score = card.Score();
            return score == 0 ? 0 : LMath.Pow(2, score-1);
        });
    }

    [TestCase(Input.Example, 30)]
    [TestCase(Input.File, 5037841)]
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

    private static long ScoreCard(IReadOnlyList<Card> cards, long number)
    {
        var score = cards[(int)number - 1].Score();
        if (score == 0) return 1;
        return 1 + ScoreCards(cards, cards.Skip((int)number).Take((int)score).Select(card => card.Number).ToList());
    }
}

public record Card(long Number, IReadOnlyList<long> WinningNumbers, IReadOnlyList<long> NumbersYouHave);

public static class CardExtensions {
    public static long Score(this Card card)
    {
        return card.WinningNumbers.Intersect(card.NumbersYouHave).Count();
    }
}