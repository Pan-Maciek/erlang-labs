-module(pingpong).
-author(maciek).

-export([start/0, stop/0, play/1]).

start() ->
  register(ping, spawn(fun() -> ping(0) end)),
  register(pong, spawn(fun pong/0)).

stop() ->
  ping! stop,
  pong! stop.

play(N) when is_integer(N) ->
  ping! N.

ping(Sum) ->
  receive
    N when is_integer(N), N > 0 -> 
        timer:sleep(500),
        io:format("ping(n: ~p, sum: ~p)~n", [N, Sum + N]),
        pong! N - 1,
        ping(Sum + N);
    stop -> ok
  after
    20000 -> ok
  end.

pong() ->
  receive
    N when is_integer(N), N > 0 -> 
      timer:sleep(500),
      io:format("pong(n: ~p)~n", [N]),
      ping! N - 1,
      pong();
    stop -> ok
  after
    20000 -> ok
  end.
