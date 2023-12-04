using System;
using System.Collections.Generic;

namespace AdventOfCode2023.Utils;

public static class Memoise
{
  public static Func<T1, TResult> Create<T1, TResult>(Func<T1, TResult> original)
  {
    var memoise = new Dictionary<Tuple<T1>, TResult>();
    return (T1 a) => {
      if (memoise.TryGetValue(Tuple.Create(a), out var result))
      {
        return result;
      }
      var temp = original(a);
      memoise.Add(Tuple.Create(a), temp);
      return temp;
    };
  }


  public static Func<T1, T2, TResult> Create<T1, T2, TResult>(Func<T1, T2, TResult> original)
  {
    var memoise = new Dictionary<(T1, T2), TResult>();
    return (T1 a, T2 b) => {
      if (memoise.TryGetValue((a, b), out var result))
      {
        return result;
      }
      var temp = original(a, b);
      memoise.Add((a, b), temp);
      return temp;
    };
  }

  public static Func<T1, T2, T3, TResult> Create<T1, T2, T3, TResult>(Func<T1, T2, T3, TResult> original)
  {
    var memoise = new Dictionary<(T1, T2, T3), TResult>();
    return (T1 a, T2 b, T3 c) => {
      if (memoise.TryGetValue((a, b, c), out var result))
      {
        return result;
      }
      var temp = original(a, b, c);
      memoise.Add((a, b, c), temp);
      return temp;
    };
  }
}