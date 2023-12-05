﻿using System;
using System.Collections.Generic;
using System.Linq;
using TypeParser;

namespace AdventOfCode2023.Utils;
public static class EnumerableExtensions
{
    public static IEnumerable<List<T>> InGroupsOf<T>(this IEnumerable<T> self, int n)
    {
        var l = new List<T>();
        foreach (var item in self)
        {
            l.Add(item);
            if (l.Count == n)
            {
                yield return l;
                l = new List<T>();
            }
        }
        if (l.Any()) throw new Exception();
    }

    public static long Pin(this long self, int min, int max)
    {
        if (self < min) return min;
        if (self > max) return max;
        return self;
    }

    public static IEnumerable<long> Range2(long min, long max)
    {
        for (var x = min; x <= max; x++) yield return x;
    }

    public static IEnumerable<T> Flatten<T>(this IEnumerable<IEnumerable<T>> self)
    {
        return self.SelectMany(it => it);
    }

    public static IEnumerable<List<T>> Choose<T>(this IEnumerable<T> self, int k)
    {
        if (k <= 0) yield break;
        var l = self.ToList();
        if (k >= l.Count)
        {
            yield return l;
            yield break;
        }

        if (k == 1)
        {
            foreach (var item in l) yield return new List<T> { item };
            yield break;
        }

        var first = l[0];
        foreach (var item in l.Skip(1).Choose(k - 1))
        {
            var result = new List<T> { first };
            result.AddRange(item);
            yield return result;
        }

        foreach (var item in l.Skip(1).Choose(k)) yield return item;
    }

    public static long Product(this IEnumerable<long> self)
    {
        return self.Aggregate((current, value) => current * value);
    }

    public static int Product(this IEnumerable<int> self)
    {
        return self.Aggregate((current, value) => current * value);
    }

    public static long ToLong(this IEnumerable<bool> self)
    {
        return self.Aggregate(0L, (current, value) => current * 2 + (value ? 1 : 0));
    }

    public static List<T> DequeueList<T>(this Queue<T> self, long count)
    {
        var result = new List<T>();
        for (var i = 0; i < count; i++)
        {
            result.Add(self.Dequeue());
        }

        return result;
    }

    public static Queue<T> ToQueue<T>(this IEnumerable<T> self)
    {
        return new Queue<T>(self);
    }

    public static IEnumerable<List<T>> Windows<T>(this IEnumerable<T> self, int windowSize)
    {
        var queue = new Queue<T>();
        foreach (var item in self)
        {
            queue.Enqueue(item);
            if (queue.Count == windowSize)
            {
                yield return queue.ToList();
                queue.Dequeue();
            }
        }
    }

    public static IEnumerable<(T first, T second)> Pairs<T>(this IEnumerable<T> self)
    {
        var l = self.ToList();
        for (var first = 0; first < l.Count - 1; first++)
        {
            for (var second = first + 1; second < l.Count; second++)
            {
                yield return (l[first], l[second]);
            }
        }
    }

    public static List<T> ListFromItem<T>(T item)
    {
        return new List<T> { item };
    }

    public static Dictionary<TKey, List<TValue>> GroupToDictionary<T, TKey, TValue>(this IEnumerable<T> input, Func<T, TKey> keyFunc,
        Func<T, TValue> valueFunc)
        where TKey : notnull
    {
        return input.GroupBy(keyFunc).ToDictionary(it => it.Key, it => it.Select(valueFunc).ToList());
    }

    public static Dictionary<TKey, List<T>> GroupToDictionary<T, TKey>(this IEnumerable<T> input, Func<T, TKey> keyFunc)
        where TKey : notnull
    {
        return input.GroupBy(keyFunc).ToDictionary(it => it.Key, it => it.ToList());
    }

    public static Dictionary<T, List<T>> GroupToDictionary<T>(this IEnumerable<T> input)
        where T : notnull
    {
        return input.GroupToDictionary(it => it);
    }

    public static T Mode<T>(this IEnumerable<T> self)
        => self.GroupBy(value => value).MaxBy(it => it.Count()) is { } result ? result.Key : throw new Exception();

    public static IEnumerable<T> Modes<T>(this IEnumerable<T> self) where T : notnull
    {
        var groups = self.GroupBy(value => value).ToList();
        var max = groups.MaxBy(it => it.Count()) is { } result ? result.Count() : throw new Exception();
        return groups.Where(it => it.Count() == max).Select(it => it.Key);
    }

    // Flips rows and columns, eg:
    // [ [ 1, alpha, foo], [2, beta, bar] ] => [ [1, 2], [alpha, beta], [foo, bar] ]
    public static IEnumerable<List<T>> ZipMany<T>(this IEnumerable<IEnumerable<T>> self)
    {
        return self.Aggregate(new List<List<T>>(), (accum, current) =>
        {
            if (!accum.Any())
            {
                return current.Select(it => new List<T> { it }).ToList();
            }

            foreach (var z in accum.Zip(current))
            {
                z.First.Add(z.Second);
            }

            return accum;
        });
    }

    public static string Join<T>(this IEnumerable<T> self, string separator = "") =>
        string.Join(separator, self.Select(it => it?.ToString()));

    public static List<string> Lines(this string input) =>
        input.Split("\n")
            .Select(it => it.Trim()).ToList();

    public static bool ContainsAll<T>(this IEnumerable<T> first, IEnumerable<T> other)
    {
        return !other.Except(first).Any();
    }

    public static IEnumerable<List<string>> Paragraphs(this string input)
    {
        var lines = input.Lines();
        var p = new List<string>();
        foreach (var line in lines)
        {
            if (string.IsNullOrWhiteSpace(line))
            {
                if (p.Any()) yield return p;
                p = new List<string>();
            }
            else
            {
                p.Add(line);
            }
        }
        if (p.Any()) yield return p;
    }

    public static IEnumerable<T> Range<T>(T first, int count, Func<T, T> generateNext)
    {
        var current = first;
        while (count-- > 0)
        {
            yield return current;
            if (count > 0)
            {
                current = generateNext(current);
            }
        }
    }

    public static IEnumerable<(T Value, int Index)> WithIndices<T>(this IEnumerable<T> self) =>
        self.Select((it, index) => (it, index));

    public static int FindIndex<T>(this IEnumerable<T> self, Func<T, bool> selector) =>
        self.WithIndices().First(it => selector(it.Value)).Index;

    public static string Csv<T>(this IEnumerable<T> self) => self.Join(",");

    public static T Pop<T>(this List<T> self)
    {
        var result = self.Last();
        self.RemoveAt(self.Count - 1);
        return result;
    }

    public static List<T> PopRange<T>(this List<T> self, int count)
    {
        var result = self.Skip(self.Count - count).ToList();
        self.RemoveRange(self.Count - count, count);
        return result;
    }

    public static T Shift<T>(this LinkedList<T> self)
    {
        if (self.Any())
        {
            var result = self.First();
            self.RemoveFirst();
            return result;
        }
        throw new ApplicationException("Attempt to shift empty list.");
    }

    public static (List<T>, List<T>) SplitInTwo<T>(this IEnumerable<T> self, Func<T, bool> test)
    {
        var l1 = new List<T>();
        var l2 = new List<T>();
        var split = false;
        foreach (var item in self)
        {
            if (!split)
            {
                if (test(item))
                {
                    split = true;
                    continue;
                }
                l1.Add(item);
            }
            else
            {
                l2.Add(item);
            }
        }
        return (l1, l2);
    }

    public static bool TryParse<T>(this string self, out T result, Format? format = null) 
    {
        try
        {
            result = self.Parse<T>(format);
            return true;
        }
        catch
        {
            result = default!;
            return false;
        }
    }

    public static T Parse<T>(this string self, Format? format = null) => TypeCompiler.Parse<T>(self, format);

    public static List<T> Parse<T>(this IEnumerable<string> self) => self.Select(s => s.Parse<T>()).ToList();

    // like TakeWhile but also returns first non-match
    public static IEnumerable<T> TakeWhilePlusOne<T>(this IEnumerable<T> self, Func<T, bool> predicate)
    {
        foreach (var item in self)
        {
            var p = predicate(item);
            yield return item;
            if (!p) break;
        }
    }

    public static void InsertIntoList<TKey, TValue>(this IDictionary<TKey, List<TValue>> self, TKey key, TValue item)
    {
        if (self.TryGetValue(key, out var list))
        {
            list.Add(item);
        }
        else 
        {
            var list2 = new List<TValue> { item };
            self[key] = list2;
        }
    }
}
