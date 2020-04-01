-module(pollution).
-author(maciek).

-record(monitor, {names = #{}, state = #{}}).

-export([ createMonitor/0, addStation/3, addValue/5, removeValue/4, getOneValue/4 ]).

createMonitor() -> #monitor{}.

addStation(Name, _, _) when not is_list(Name) -> {error, name, "expected string"};
addStation(_, Coordinates, _) when not is_tuple(Coordinates) -> {error, coordinates, "expected tuple"};
addStation(Name, Coordinates, Monitor = #monitor{names = Names, state = State}) ->
  case {maps:find(Name, Names), maps:find(Coordinates, State)}  of
    {error, error} -> Monitor#monitor{names = Names#{ Name => Coordinates }, state = State#{ Coordinates => [] }};
    {{ok, _}, _} -> {error, name};
    {_, {ok, _}} -> {error, coordinates}
  end.

addMeasurement(Date, Type, Value, Measurements) ->
  case lists:any(fun ({XDate, XType, _}) -> (XType == Type) andalso (XDate == Date) end, Measurements) of 
    true -> error;
    _ -> [{ {Date, Type}, Value } | Measurements]
  end.

addValue(Name, Date, Type, Value, Monitor = #monitor{names = Names}) when is_list(Name) -> 
  case maps:find(Name, Names) of
    error -> {error, name};
    {ok, Coordinates} -> addValue(Coordinates, Date, Type, Value, Monitor)
  end;
addValue(Coordinates, Date, Type, Value, Monitor = #monitor{state = State}) when is_tuple(Coordinates) -> 
  case maps:find(Coordinates, State) of
    {ok, Measurements} -> 
      case addMeasurement(Date, Type, Value, Measurements) of
        error -> {error, measurement};
        ExtMeasurements -> Monitor#monitor{state = State#{ Coordinates => ExtMeasurements }}
      end;
    _ -> {error, coordinates}
  end.

removeValue(Name, Date, Type, Monitor = #monitor{names = Names}) when is_list(Name) ->
  case maps:find(Name, Names) of
    error -> {error, name};
    {ok, Coordinates} -> removeValue(Coordinates, Date, Type, Monitor)
  end;
removeValue(Coordinates, Date, Type, Monitor = #monitor{state = State}) when is_tuple(Coordinates) ->
  case maps:find(Coordinates, State) of
    error -> {error, coordinates};
    {ok, Measurements} -> Monitor#monitor{state = State#{ Coordinates => lists:keydelete({ Date, Type }, 1, Measurements) }}
  end.

getOneValue(Name, Date, Type, Monitor = #monitor{names = Names}) when is_list(Name) ->
  case maps:find(Name, Names) of
    error -> {error, name};
    {ok, Coordinates} -> getOneValue(Coordinates, Date, Type, Monitor)
  end;
getOneValue(Coordinates, Date, Type, #monitor{state = State}) when is_tuple(Coordinates) ->
  case maps:find(Coordinates, State) of
    error -> {error, coordinates};
    {ok, Measurements} ->
      case lists:keytake({ Date, Type }, 1, Measurements) of
        {value, {_, Value}, _} -> Value;
        _ -> {error, not_found}
      end
  end.
