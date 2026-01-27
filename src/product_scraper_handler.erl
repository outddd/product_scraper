-module(product_scraper_handler).
-behaviour(cowboy_handler).

-export([init/2]).
-export([terminate/3]).

-include("product_scraper.hrl").
-include_lib("kernel/include/logger.hrl").

-define(RESPONSE_HEADERS, #{<<"content-type">> => <<"application/json">>}).

init(Req0, State) ->
  Method = cowboy_req:method(Req0),
  Path = cowboy_req:path(Req0),
  ?LOG_DEBUG("~s ~s", [Method, Path]),
  Req = handle(Method, Path, Req0),
  {ok, Req, State}.

handle(<<"GET">>, <<"/assembled-products">>, Req) ->
  QS = cowboy_req:parse_qs(Req),
  case parse_filters(QS) of
    {ok, StartDate, EndDate, Id} ->
      Products = product_scraper_storage:get_products(StartDate, EndDate, Id),
      Reply = group_by_time(Products),
      cowboy_req:reply(200, ?RESPONSE_HEADERS, jiffy:encode(Reply), Req);
    {error, Reason} ->
      Reply = {[{error, Reason}]},
      cowboy_req:reply(400, ?RESPONSE_HEADERS, jiffy:encode(Reply), Req)
  end;


handle(<<"POST">>, <<"/set-time">>, Req) ->
  QS = cowboy_req:parse_qs(Req),
  case parse_interval(proplists:get_value(<<"interval">>, QS)) of
    {ok, Interval} ->
      product_scraper_worker:set_interval(Interval),
      ?LOG_INFO("Interval set to ~B s", [Interval]),
      Reply = {[{status, ok}, {interval, Interval}]},
      cowboy_req:reply(200, ?RESPONSE_HEADERS, jiffy:encode(Reply), Req);
    {error, Reason} ->
      Reply = {[{error, Reason}]},
      cowboy_req:reply(400, ?RESPONSE_HEADERS, jiffy:encode(Reply), Req)
  end;

handle(_, _, Req) ->
  cowboy_req:reply(404, ?RESPONSE_HEADERS, jiffy:encode(#{error => <<"not found">>}), Req).

%% Internal

-define(GREG_TO_UNIX_OFFSET, 62167219200).

parse_filters(QS) ->
  try
    StartDate = parse_date(proplists:get_value(<<"start_date">>, QS)),
    EndDate = parse_date(proplists:get_value(<<"end_date">>, QS)),
    Id = parse_id(proplists:get_value(<<"id">>, QS)),
    {ok, StartDate, EndDate, Id}
  catch
    throw:{invalid, Field} -> {error, <<"invalid ", Field/binary>>}
  end.

parse_date(undefined) -> undefined;
parse_date(DateBin) ->
  try
    [Y, M, D] = [binary_to_integer(X) || X <- string:split(DateBin, <<"-">>, all)],
    % check month and day or get exception
    true = M >= 1 andalso M =< 12,
    true = D >= 1 andalso D =< 31,
    Seconds = calendar:datetime_to_gregorian_seconds({{Y, M, D}, {0, 0, 0}}) - ?GREG_TO_UNIX_OFFSET,
    timer:seconds(Seconds)
  catch
    _:_ ->
      throw({invalid, <<"date">>})
  end.

parse_id(undefined) -> undefined;
parse_id(IdBin) ->
  try
    Id = binary_to_integer(IdBin),
    true = Id > 0,
    Id
  catch
    _:_ ->
      throw({invalid, <<"id">>})
  end.

parse_interval(undefined) -> {error, missing};
parse_interval(Bin) ->
  try
    Interval = binary_to_integer(Bin),
    true = Interval > 0,
    {ok, Interval}
  catch
    _:_ -> {error, invalid}
  end.

group_by_time(Products) ->
  lists:foldl(fun(#product{id = Id, price = Price, scraped_at = ScrapedAt}, Acc) ->
    TimeKey = format_time(ScrapedAt),
    Item = {[{id, integer_to_binary(Id)}, {price, Price}]},
    maps:update_with(TimeKey, fun(List) -> [Item | List] end, [Item], Acc)
  end, #{}, Products).

format_time(Timestamp) ->
  Seconds = Timestamp div 1000,
  Millis = Timestamp rem 1000,
  {{Y, M, D}, {H, Mi, S}} = calendar:system_time_to_universal_time(Seconds, second),
  iolist_to_binary(io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0B.~3..0BZ", [Y, M, D, H, Mi, S, Millis])).

terminate(_Reason, _Req, _State) ->
  ok.
