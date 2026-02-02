-module(product_scraper_storage).
-behaviour(gen_server).

-dialyzer({nowarn_function, [select_products/3, build_match_spec/3]}).

%% API.
-export([start_link/0]).

%% gen_server.
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([store/1, get_products/3]).

-include_lib("kernel/include/logger.hrl").
-include("product_scraper.hrl").

-record(state, {
  ready = false :: boolean()
}).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec store([product()]) -> ok.
store(Products) ->
  gen_server:cast(?MODULE, {store, Products}).

-spec get_products(non_neg_integer() | undefined, non_neg_integer() | undefined, non_neg_integer() | undefined) -> [product()] | {error, not_ready}.
get_products(From, To, Id) ->
  gen_server:call(?MODULE, {get_products, From, To, Id}).

%% gen_server.

init([]) ->
  ?LOG_INFO("Start storage"),
  self() ! init_mnesia,
  {ok, #state{}}.

handle_call({get_products, _FromTime, _ToTime, _Id}, _From, #state{ready = false} = State) ->
  {reply, {error, not_ready}, State};

handle_call({get_products, FromTime, ToTime, Id}, _From, #state{ready = true} = State) ->
  Products = select_products(FromTime, ToTime, Id),
  ?LOG_DEBUG("Found ~B products", [length(Products)]),
  {reply, Products, State};

handle_call(Request, _From, State) ->
  ?LOG_WARNING("Unknown request ~p", [Request]),
  {reply, ignored, State}.

handle_cast({store, _Products}, #state{ready = false} = State) ->
  ?LOG_WARNING("Storage not ready, dropping product"),
  {noreply, State};

handle_cast({store, Products}, #state{ready = true} = State) ->
  % we use dirty functions because there is single process write and read db
  lists:foreach(
    fun(P) ->
      mnesia:dirty_write(P),
      ?LOG_DEBUG("Store ~p", [P])
    end, Products
  ),
  ?LOG_DEBUG("Stored ~B products", [length(Products)]),
  {noreply, State};

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(init_mnesia, State) ->
  ok = init_mnesia(),
  ?LOG_INFO("Storage ready"),
  {noreply, State#state{ready = true}}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


%% Internal

init_mnesia() ->
  _ = mnesia:create_schema([node()]),
  ok = mnesia:start(),
  _ = mnesia:create_table(product, [
    {attributes, record_info(fields, product)},
    {disc_copies, [node()]},
    {type, bag}
  ]),
  ok = mnesia:wait_for_tables([product], infinity).

select_products(From, To, Id) ->
  % select records between from and to timestamps with Id if it isn't undefined
  MatchSpec = build_match_spec(From, To, Id),
  mnesia:dirty_select(product, MatchSpec).

build_match_spec(From, To, Id) ->
  IdPattern = case Id of
                undefined -> '$1';
                _ -> Id
              end,
  Guards = build_guards(From, To),
  [{#product{id = IdPattern, price = '$2', scraped_at = '$3'}, Guards, ['$_']}].

build_guards(undefined, undefined) -> [];
build_guards(From, undefined) -> [{'>=', '$3', From}];
build_guards(undefined, To) -> [{'=<', '$3', To}];
build_guards(From, To) -> [{'>=', '$3', From}, {'=<', '$3', To}].
