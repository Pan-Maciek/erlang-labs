-module(pollution).
-author(maciek).

-define(Err, Error = {error, _} -> Error).
-record(monitor, {names = #{}, state = #{}}).

-export([ createMonitor/0, addStation/3, addValue/5, removeValue/4, 
          getOneValue/4, getStationMean/3, getDailyMean/3,
          getStationQuartiles/3, getStationIQR/3 ]).

createMonitor() -> #monitor{}.

addStation(Name, _, _) when not is_list(Name) -> {error, name};
addStation(_, Coordinates, _) when not is_tuple(Coordinates) -> {error, coordinates};
addStation(Name, Coordinates, #monitor{names = Names, state = State}) ->
  case {maps:is_key(Name, Names), maps:is_key(Coordinates, State)} of
    {false, false} -> #monitor{names = Names#{ Name => Coordinates }, state = State#{ Coordinates => [] }};
    {true, _} -> {error, name};
    {_, true} -> {error, coordinates}
  end.

getMeasurements(Name, Monitor = #monitor{ names = Names }) when is_list(Name) ->
  case maps:find(Name, Names) of
    {ok, Coordinates} -> getMeasurements(Coordinates, Monitor);
    _ -> {error, name}
  end;
getMeasurements(Coordinates, #monitor{ state = State }) when is_tuple(Coordinates) ->
  case maps:find(Coordinates, State) of
    {ok, Measurements} -> {Coordinates, Measurements};
    _ -> {error, coordinates}
  end.

addMeasurement(Key, Value, Measurements) ->
  case lists:keyfind(Key, 1, Measurements) of 
    false -> [{Key, Value} | Measurements];
    _ -> {error, measurement}
  end.

addValue(Id, Date, Type, Value, Monitor = #monitor{state = State}) -> 
  case getMeasurements(Id, Monitor) of ?Err;
    {Coordinates, Measurements} ->
      case addMeasurement({Date, Type}, Value, Measurements) of ?Err;
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
      case lists:keyfind({ Date, Type }, 1, Measurements) of
        false -> {error, not_found};
        {_, Value} -> Value
      end
  end.

mean([]) -> {error, no_values};
mean(List) -> lists:sum(List) / length(List).

getStationMean(Id, Type, Monitor) ->
  case getMeasurements(Id, Monitor) of ?Err;
    {_, Measurements} -> mean([Value || {{_, XType}, Value} <- Measurements, XType == Type])
  end.

getDailyMean({Day, _}, Type, #monitor{state = State}) ->
  mean([Value || Values <- maps:values(State), {{{XDay, _}, XType}, Value} <- Values, {XDay, XType} == {Day, Type} ]).

medianS([], Q2) -> Q2;
medianS([X], _) -> X;
medianS(Sorted, _) -> 
  Len = length(Sorted),
  (lists:nth(Len div 2, Sorted) + lists:nth(Len div 2 + 1, Sorted)) / 2.

quartiles([]) -> {error, no_values};
quartiles(Measurements) ->
  Sorted = lists:sort(Measurements),
  {Min, Max} = {lists:min(Sorted), lists:max(Sorted)},
  Q2 = medianS(Sorted, 0),
  Q1 = medianS([ X || X <- Sorted, X < Q2], Q2),
  Q3 = medianS([ X || X <- Sorted, X > Q2], Q2),
  {Min, Q1, Q2, Q3, Max}.

getStationQuartiles(Id, Type, Monitor) ->
  case getMeasurements(Id, Monitor) of ?Err;
    {_, Measurements} -> quartiles([Value || {{_, XType}, Value} <- Measurements, XType == Type])
  end.

getStationIQR(Id, Type, Monitor) ->
  case getStationQuartiles(Id, Type, Monitor) of ?Err;
    {_, Q1, _, Q3, _} -> Q3 - Q1
  end.
