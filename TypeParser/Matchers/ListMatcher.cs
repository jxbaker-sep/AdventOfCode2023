using System.Collections.Generic;
using System.Text.RegularExpressions;

namespace TypeParser.Matchers
{
    internal class ListMatcher<T> : ITypeMatcher
    {
        private readonly ITypeMatcher ElementMatcher;
        private readonly InternalFormat Repeat;

        public ListMatcher(ITypeMatcher elementMatcher, InternalFormat repeat)
        {
            Repeat = repeat;
            ElementMatcher = elementMatcher;
        }

        public ITypeMatcher.Result? Match(string input)
        {
            input = input.TrimStart();
            var instance = new List<T>();

            var m = ElementMatcher.Match(input);
            if (m == null)
            {
                if (Repeat.Min == 0) return new(instance, input);
                return null;
            }

            instance.Add((T)m.Value!);
            input = m.Remainder;

            var separator = Repeat.Separator?.ToString();
            if (separator?.StartsWith("^") == true) {
                separator = separator.Substring(1);
            }

            while (instance.Count < Repeat.Max)
            {
                if (string.IsNullOrWhiteSpace(input)) break;

                if (Repeat.Terminator is Regex re)
                {
                    var temp = input.TrimStart();
                    var afterMatch = re.Match(temp);
                    if (afterMatch.Success)
                    {
                        input = temp[afterMatch.Length..];
                        break;
                    }
                }

                var m1 = Regex.Match(input, @$"^\s*{separator}\s*");
                if (!m1.Success) break;
                input = input[m1.Length..];

                m = ElementMatcher.Match(input);

                if (m == null) return null;

                instance.Add((T)m.Value!);
                input =  m.Remainder;
            }

            if (instance.Count < Repeat.Min)
            {
                return null;
            }

            return new(instance, input);
        }
    }
}