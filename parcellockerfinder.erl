-module(parcellockerfinder).
-author(maciek).

-export([ findMyParcelLocker/2, sequential/2, parallel/2, multicore/2 ]).

distanceSq({Xa, Ya}, {Xb, Yb}) -> math:pow(Xa - Xb, 2) + math:pow(Ya - Yb, 2).

findMyParcelLocker(_, []) -> {error, no_lockers};
findMyParcelLocker(Position, Lockers) ->
  {_, Locker} = lists:min([{distanceSq(Position, Locker), Locker} || Locker <- Lockers]),
  Locker.

sequential([], _) -> [];
sequential(_, []) -> {error, no_lockers};
sequential(People, Lockers) ->
  [{Person, findMyParcelLocker(Person, Lockers)} || Person <- People].

parallel([], _) -> [];
parallel(_, []) -> {error, no_lockers};
parallel(People, Lockers) ->
  Pid = self(),
  lists:foreach(fun(Person) -> 
                    spawn(fun() -> Pid! {Person, findMyParcelLocker(Person, Lockers)} end)
                end, People),
  [receive X -> X end || _ <- People].

multicore([], _) -> [];
multicore(_, []) -> {error, no_lockers};
multicore(People, Lockers) ->
  {Cores, Pid} = {erlang:system_info(logical_processors_available), self()},
  {Offset, Len} = {length(People) rem Cores, length(People) div Cores},
  lists:foreach(fun(List) -> spawn(fun() -> Pid! sequential(List, Lockers) end) end, [ 
    lists:sublist(People, 1, Offset + Len) | 
    [ lists:sublist(People, 1 + Offset + Len * Core, Len) || Core <- lists:seq(1, Cores - 1) ]
  ]),
  lists:concat([receive X -> X end || _ <- lists:seq(1, Cores)]).
