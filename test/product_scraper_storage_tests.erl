-module(product_scraper_storage_tests).
-include_lib("eunit/include/eunit.hrl").
-include("product_scraper.hrl").

setup() ->
  application:ensure_all_started(mnesia),
  % logger:set_primary_config(level, debug),
  product_scraper_storage:start_link(),
  timer:sleep(100),
  ok.

cleanup(_) ->
  mnesia:clear_table(product),
  ok.

storage_test_() ->
  {setup, fun setup/0, fun cleanup/1, [
    {"store and get all", fun store_and_get_test/0},
    {"filter by time", fun filter_by_time_test/0},
    {"filter by id", fun filter_by_id_test/0},
    {"filter by time and id", fun filter_combined_test/0}
  ]}.

store_and_get_test() ->
  mnesia:clear_table(product),
  Products = [
    #product{id = 1, price = 10.0, scraped_at = 1000},
    #product{id = 2, price = 20.0, scraped_at = 1000}
  ],
  product_scraper_storage:store(Products),
  timer:sleep(50),
  Result = product_scraper_storage:get_products(undefined, undefined, undefined),
  ?assertEqual(2, length(Result)).

filter_by_time_test() ->
  mnesia:clear_table(product),
  Products = [
    #product{id = 1, price = 10.0, scraped_at = 1000},
    #product{id = 2, price = 20.0, scraped_at = 2000},
    #product{id = 3, price = 30.0, scraped_at = 3000}
  ],
  product_scraper_storage:store(Products),
  timer:sleep(50),

  %% only after 1500
  Result1 = product_scraper_storage:get_products(1500, undefined, undefined),
  ?assertEqual(2, length(Result1)),

  %%  only before 2500
  Result2 = product_scraper_storage:get_products(undefined, 2500, undefined),
  ?assertEqual(2, length(Result2)),

  %% between 1500 и 2500
  Result3 = product_scraper_storage:get_products(1500, 2500, undefined),
  ?assertEqual(1, length(Result3)).

filter_by_id_test() ->
  mnesia:clear_table(product),
  Products = [
    #product{id = 1, price = 10.0, scraped_at = 1000},
    #product{id = 1, price = 11.0, scraped_at = 2000},
    #product{id = 2, price = 20.0, scraped_at = 1000}
  ],
  product_scraper_storage:store(Products),
  timer:sleep(50),

  Result = product_scraper_storage:get_products(undefined, undefined, 1),
  ?assertEqual(2, length(Result)),
  lists:foreach(fun(#product{id = Id}) ->
    ?assertEqual(1, Id)
                end, Result).

filter_combined_test() ->
  mnesia:clear_table(product),
  Products = [
    #product{id = 1, price = 10.0, scraped_at = 1000},
    #product{id = 1, price = 11.0, scraped_at = 2000},
    #product{id = 2, price = 20.0, scraped_at = 2000}
  ],
  product_scraper_storage:store(Products),
  timer:sleep(50),

  Result = product_scraper_storage:get_products(1500, undefined, 1),
  ?assertEqual(1, length(Result)),
  [#product{id = 1, price = 11.0}] = Result.