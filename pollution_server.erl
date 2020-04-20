-module(pollution_server).
-author(maciek).

-define(ReceiveStatus, receive A -> A end).
-define(Expose(Fn, Arg1, Arg2), Fn(Arg1, Arg2) -> server ! {request, self(), Fn, [Arg1, Arg2]}, ?ReceiveStatus).
-define(Expose(Fn, Arg1, Arg2, Arg3), Fn(Arg1, Arg2, Arg3) -> server ! {request, self(), Fn, [Arg1, Arg2, Arg3]}, ?ReceiveStatus).
-define(Expose(Fn, Arg1, Arg2, Arg3, Arg4), Fn(Arg1, Arg2, Arg3, Arg4) -> server ! {request, self(), Fn, [Arg1, Arg2, Arg3, Arg4]}, ?ReceiveStatus).

-import_all(pollution).
-export([ start/0, stop/0, addStation/2, addValue/4, removeValue/3, getOneValue/3, 
          getStationMean/2, getDailyMean/2, getStationQuartiles/2, getStationIQR/2,
          getState/0 ]).

start() -> register(server, spawn(fun init/0)).
stop() -> server ! stop.
init() -> event_loop(pollution:createMonitor()).
getState() -> server ! {getState, self()}, ?ReceiveStatus.

event_loop(State) ->
  receive
    {request, RequestPid, Fn, Args} ->
      event_loop (
        case apply(pollution, Fn, Args ++ [State]) of
          Error = {error, _} -> RequestPid ! Error, State;
          NewState -> RequestPid ! ok, NewState
        end);
    {getState, RequestPid} -> RequestPid ! State, event_loop(State);
    stop -> ok;
    _ -> {error, unknown_operation}
  end.

?Expose(addStation, Name, Coordinates).
?Expose(addValue, Id, Date, Type, Value).
?Expose(removeValue, Id, Date, Type).
?Expose(getOneValue, Id, Date, Type).
?Expose(getStationMean, Id, Type).
?Expose(getDailyMean, Date, Type).
?Expose(getStationQuartiles, Id, Type).
?Expose(getStationIQR, Id, Type).