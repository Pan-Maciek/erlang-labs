-module(onp).
-author("maciek").

-export([onp/1]).

onp(X) -> eval(X, []).

eval("", [X]) -> X;
eval("", _) -> {error, "invalid expresion"};
eval(" " ++ E, X) -> eval(E, X);
eval("+" ++ E, [ Y, X | T ]) -> eval(E, [ X + Y | T ]);
eval("-" ++ E, [ Y, X | T ]) -> eval(E, [ X - Y | T ]);
eval("*" ++ E, [ Y, X | T ]) -> eval(E, [ X * Y | T ]);
eval("/" ++ _, [ 0 | _ ]) -> {error, "X / 0"};
eval("/" ++ E, [ Y, X | T ]) -> eval(E, [ X / Y | T ]);
eval("sqrt" ++ E,  [ X | T ]) when X > 0 ->  eval(E, [ math:sqrt(X) | T ]);
eval("sqrt" ++ _,  _) ->  {error, "sqrt(X) when X < 0"};
eval("pow" ++ E, [ Y, X | T ]) -> eval(E, [ math:pow(X, Y) | T ]);
eval("sin" ++ E,  [ X | T ]) -> eval(E, [ math:sin(X) | T ]);
eval("cos" ++ E,  [ X | T ]) -> eval(E, [ math:cos(X) | T ]);
eval("tan" ++ E,  [ X | T ]) -> eval(E, [ math:tan(X) | T ]);
eval("%" ++ E, [ Y, X | T ]) -> eval(E, [ math:fmod(X, Y) | T ]);
eval("!" ++ E, [ X | T ]) -> eval(E, [ -X | T ]);
eval(S, T) ->
  case string:to_float(S) of
    {error, _} ->
      case string:to_integer(S) of
        {error, _} -> {error, "invalid input: " ++ S};
        {X, E} -> eval(E, [ X | T ])
      end;
    {X, E} -> eval(E, [ X | T ])
  end.