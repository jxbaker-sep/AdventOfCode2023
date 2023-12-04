using System;
using System.Collections.Generic;

namespace AdventOfCode2023.Utils;

public static class Memoise
{
  public static Func<A, B, C> Create<A, B, C>(Func<A, B, C> original)
  {
    var memoise = new Dictionary<(A, B), C>();
    return (A a, B b) => {
      if (memoise.TryGetValue((a, b), out var result))
      {
        return result;
      }
      var temp = original(a, b);
      memoise.Add((a, b), temp);
      return temp;
    };
  }
}