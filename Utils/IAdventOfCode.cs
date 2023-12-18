using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using FluentAssertions;

namespace AdventOfCode2023.Utils
{
    public interface IAdventOfCode
    {
        void Run();
    }

    public static class AdventOfCodeExtensions
    {
        public static string Data(this IAdventOfCode self) => System.IO.File.ReadAllText($"../AdventOfCode2023.Data/{self.GetType().Name.ToLower()}_data.txt");
        public static string Sample(this IAdventOfCode self) => System.IO.File.ReadAllText($"../AdventOfCode2023.Data/{self.GetType().Name.ToLower()}_sample.txt");
        public static string SampleN(this IAdventOfCode self, long N) => System.IO.File.ReadAllText($"../AdventOfCode2023.Data/{self.GetType().Name.ToLower()}_sample{N}.txt");
    }

    public abstract class AdventOfCode<TOut, TIn> : IAdventOfCode
    {
        public void Run()
        {
            var samples = new Dictionary<long, TIn>();
            var file = new List<TIn>();

            var part1TestCases = GetType().GetMethod("Part1")!.GetCustomAttributes<TestCaseAttribute>().ToList();
            var part2TestCases = GetType().GetMethod("Part2")!.GetCustomAttributes<TestCaseAttribute>().ToList();

            foreach (var sample in part1TestCases.Union(part2TestCases).Where(it => it.Input == Input.Sample))
            {
                if (samples.ContainsKey(sample.N)) continue;
                if (sample.N == 0) {
                    samples.Add(0, Parse(this.Sample()));
                }
                else {
                    samples.Add(sample.N, Parse(this.SampleN(sample.N)));

                }
            }

            if (part1TestCases.Union(part2TestCases).Any(it => it.Input == Input.Data))
            {
                file.Add(Parse(this.Data()));
            }

            foreach (var testCase in part1TestCases)
            {
                TestCase = testCase;
                var actual = Part1(testCase.Input switch{
                    Input.Sample => samples[testCase.N],
                    Input.Data => file[0],
                    Input.Raw => Parse(testCase.Raw),
                    _ => throw new ApplicationException()
                });
                if (!actual!.Equals(AdventOfCode<TOut, TIn>.Coerce2nd(actual, testCase.Expected)))
                {
                    Console.WriteLine($"\nERROR! {this.GetType().Name}/Part 1/{testCase.Input} expected {testCase.Expected}, got {actual}");
                }
            }

            foreach (var testCase in part2TestCases)
            {
                TestCase = testCase;
                var actual = Part2(testCase.Input switch{
                    Input.Sample => samples[testCase.N],
                    Input.Data => file[0],
                    Input.Raw => Parse(testCase.Raw),
                    _ => throw new ApplicationException()
                });
                if (!actual!.Equals(AdventOfCode<TOut, TIn>.Coerce2nd(actual, testCase.Expected)))
                {
                    Console.WriteLine($"\nERROR! {this.GetType().Name}/Part 2/{testCase.Input} expected {testCase.Expected}, got {actual}");
                }
            }
        }

        static object? Coerce2nd(object n, object n2)
        {
            if (n2 == null) return n2;
            if (n2.GetType() == typeof(int) && n.GetType() == typeof(long))
            {
                return Convert.ToInt64(n2);
            }
            return n2;
        }

        public abstract TIn Parse(string input);

        public abstract TOut Part1(TIn input);
        public abstract TOut Part2(TIn input);

        public TestCaseAttribute TestCase { get; private set; } = new TestCaseAttribute(Input.Sample, 0);
    }

    [AttributeUsage(AttributeTargets.Method, AllowMultiple = true)]
    public class TestCaseAttribute: Attribute
    {
        public Input Input { get; }
        public object Expected { get; }
        public string Raw {get; set;} = "";
        public long Arg0 {get;set;} = 0;
        public long N { get; set; } = 0;

        public TestCaseAttribute(Input input, object expected)
        {
            Input = input;
            Expected = expected;
        }
    }

    public enum Input
    {
        Sample,
        Data,
        Raw
    }
}