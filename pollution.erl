-module(pollution).
-author(maciek).

-define(Err, Error = {error, _} -> Error).
-record(monitor, {names = #{}, state = #{}}).

-export([ createMonitor/0, addStation/3, addValue/5, removeValue/4, getOneValue/4, getStationMean/3 ]).

createMonitor() -> #monitor{}.

addStation(Name, _, _) when not is_list(Name) -> {error, name, "expected string"};
addStation(_, Coordinates, _) when not is_tuple(Coordinates) -> {error, coordinates, "expected tuple"};
addStation(Name, Coordinates, Monitor = #monitor{names = Names, state = State}) ->
  case {maps:find(Name, Names), maps:find(Coordinates, State)}  of
    {error, error} -> Monitor#monitor{names = Names#{ Name => Coordinates }, state = State#{ Coordinates => [] }};
    {{ok, _}, _} -> {error, name};
    {_, {ok, _}} -> {error, coordinates}
  end.

getMeasurements(Name, Monitor = #monitor{ names = Names }) when is_list(Name) ->
  case maps:find(Name, Names) of
    error -> {error, name};
    {ok, Coordinates} -> getMeasurements(Coordinates, Monitor)
  end;
getMeasurements(Coordinates, #monitor{ state = State }) when is_tuple(Coordinates) ->
  case maps:find(Coordinates, State) of
    error -> {error, coordinates};
    {ok, Measurements} -> {Coordinates, Measurements}
  end.

addMeasurement(Date, Type, Value, Measurements) ->
  case lists:keyfind({Date, Type}, 1, Measurements) of 
    false -> [{ {Date, Type}, Value } | Measurements];
    _ -> {error, measurement}
  end.

addValue(Id, Date, Type, Value, Monitor = #monitor{state = State}) -> 
  case getMeasurements(Id, Monitor) of ?Err;
    {Coordinates, Measurements} ->
      case addMeasurement(Date, Type, Value, Measurements) of ?Err;
        ExtMeasurements -> Monitor#monitor{state = State#{ Coordinates => ExtMeasurements }}
      end
  end.

removeValue(Id, Date, Type, Monitor = #monitor{state = State}) ->
  case getMeasurements(Id, Monitor) of ?Err;
    {Coordinates, Measurements} -> Monitor#monitor{state = State#{ Coordinates => lists:keydelete({ Date, Type }, 1, Measurements) }}
  end.

getOneValue(Id, Date, Type, Monitor = #monitor{}) ->
  case getMeasurements(Id, Monitor) of ?Err;
    {_, Measurements} ->
      case lists:keytake({ Date, Type }, 1, Measurements) of
        {value, {_, Value}, _} -> Value;
        _ -> {error, not_found}
      end
  end.

mean(List) -> lists:sum(List) / length(List).

getStationMean(Coordinates, Type, #monitor{state = State}) when is_tuple(Coordinates) ->
  case maps:find(Coordinates, State) of
    error -> {error, coordinates};
    {ok, Measurements} -> mean([Value || {_, XType, Value} <- Measurements, XType == Type])
  end.

