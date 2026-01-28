-module(product_scraper_worker).
-behaviour(gen_server).

%% API.
-export([start_link/0, set_interval/1]).

%% gen_server.
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
  url 							:: string(),
  interval = 30000 	:: non_neg_integer(),
  timer_ref 				:: reference() | undefined
}).

-include_lib("kernel/include/logger.hrl").
-include("product_scraper.hrl").

-define(DEFAULT_INTERVAL, 60000).
-define(URL, "https://dummyjson.com/products").

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

set_interval(Seconds) when Seconds > 0 ->
  gen_server:cast(?MODULE, {set_interval, timer:seconds(Seconds)}).

%% gen_server.

init([]) ->
  ?LOG_INFO("Start worker"),
  Interval = application:get_env(product_scraper, interval, ?DEFAULT_INTERVAL),
  Url = application:get_env(product_scraper, scrape_url, ?URL),
  self() ! scrape,
  {ok, #state{url = Url, interval = Interval, timer_ref = undefined}}.

handle_call(_Request, _From, State) ->
  {reply, ignored, State}.

handle_cast({set_interval, Ms}, #state{timer_ref = OldRef} = State) ->
  cancel_timer(OldRef),
  Ref = erlang:send_after(Ms, self(), scrape),
  ?LOG_INFO("Interval changed to ~B ms", [Ms]),
  {noreply, State#state{interval = Ms, timer_ref = Ref}}.

handle_info(scrape, #state{url = Url} = State) ->
  do_scrape(Url),
  {noreply, State};
handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% Internal

cancel_timer(undefined) -> ok;
cancel_timer(Ref) ->
  _ = erlang:cancel_timer(Ref),
  ok.

do_scrape(Url) ->
  ?LOG_INFO("Scraping ~s", [Url]),
  case fetch(Url) of
    {ok, Body} ->
      case parse(Body) of
        {ok, Products} ->
          product_scraper_storage:store(Products),
          ?LOG_INFO("Scraped ~B products", [length(Products)]);
        {error, Reason} ->
          ?LOG_ERROR("Parse failed: ~p", [Reason])
      end;
    {error, Reason} ->
      ?LOG_ERROR("Fetch failed: ~p", [Reason])
  end.

fetch(Url) ->
  case httpc:request(get, {Url, []}, [], [{body_format, binary}]) of
    {ok, {{_, 200, _}, _, Body}} ->
      {ok, Body};
    {ok, {{_, Status, _}, _, _}} ->
      {error, {http_status, Status}};
    {error, Reason} ->
      {error, Reason}
  end.

parse(Json) ->
  try
    #{<<"products">> := RawProducts} = jiffy:decode(Json, [return_maps]),
    ScrapedAt = erlang:system_time(millisecond),
    Products = [make_product(P, ScrapedAt) || P <- RawProducts],
    {ok, Products}
  catch
    _:Reason ->
      {error, Reason}
  end.

make_product(#{<<"id">> := Id, <<"price">> := Price}, ScrapedAt) ->
  #product{
    id = Id,
    price = Price,
    scraped_at = ScrapedAt
  }.