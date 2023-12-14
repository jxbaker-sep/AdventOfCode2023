using AdventOfCode2023.Utils;
using JetBrains.Annotations;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;

namespace AdventOfCode2023.Day01;

[UsedImplicitly]
public class Day01 : AdventOfCode<long,List<string>>
{
    public override List<string> Parse(string input) => 
        input.Lines();

    [TestCase(Input.Raw, 142, Raw = @"1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet")]
    [TestCase(Input.Data, 57346)]
    public override long Part1(List<string> input)
    {
        return input.Select(Part1L).Sum();
    }

    private long Part1L(string s)
    {
        var regex1 = new Regex(@"^.*?(\d)");
        var regex2 = new Regex(@".*(\d)");

        var m = regex1.Match(s);
        var m2 = regex2.Match(s);
        return GetDigit(m.Groups[1].Value) * 10 + GetDigit(m2.Groups[1].Value);
    }

    [TestCase(Input.Raw, 281, Raw = @"two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen")]
    [TestCase(Input.Data, 57345)]
    public override long Part2(List<string> input)
    {
        return input.Select(Part2L).Sum();
    }

    private long Part2L(string s)
    {
        var regex1 = new Regex(@"^.*?(\d|one|two|three|four|five|six|seven|eight|nine)");
        var regex2 = new Regex(@".*(\d|one|two|three|four|five|six|seven|eight|nine)");

        var m = regex1.Match(s);
        var m2 = regex2.Match(s);
        return GetDigit(m.Groups[1].Value) * 10 + GetDigit(m2.Groups[1].Value);
    }

    private static long GetDigit(string s)
    {
        var ds = new[]{"0","1","2","3","4","5","6","7","8","9"};
        var index = Array.IndexOf(ds, s);
        if (index > -1) return index;
        var ds2 = new[]{"zero","one","two","three","four","five","six","seven","eight","nine"};
        index = Array.IndexOf(ds2, s);
        if (index > -1) return index;
        throw new ApplicationException();
    }
}