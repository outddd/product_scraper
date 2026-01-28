-module(product_scraper_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("product_scraper.hrl").

%% parse_date

parse_date_undefined_test() ->
  ?assertEqual(undefined, product_scraper_handler:parse_date(undefined)).

parse_date_valid_test() ->
  ?assertEqual(1704067200000, product_scraper_handler:parse_date(<<"2024-01-01">>)).

parse_date_invalid_format_test() ->
  ?assertThrow({invalid, <<"date">>}, product_scraper_handler:parse_date(<<"invalid">>)).

parse_date_invalid_month_test() ->
  ?assertThrow({invalid, <<"date">>}, product_scraper_handler:parse_date(<<"2024-13-01">>)).

parse_date_invalid_day_test() ->
  ?assertThrow({invalid, <<"date">>}, product_scraper_handler:parse_date(<<"2024-01-32">>)).

%% parse_id

parse_id_undefined_test() ->
  ?assertEqual(undefined, product_scraper_handler:parse_id(undefined)).

parse_id_valid_test() ->
  ?assertEqual(123, product_scraper_handler:parse_id(<<"123">>)).

parse_id_invalid_test() ->
  ?assertThrow({invalid, <<"id">>}, product_scraper_handler:parse_id(<<"abc">>)).

parse_id_zero_test() ->
  ?assertThrow({invalid, <<"id">>}, product_scraper_handler:parse_id(<<"0">>)).

parse_id_negative_test() ->
  ?assertThrow({invalid, <<"id">>}, product_scraper_handler:parse_id(<<"-1">>)).

%% parse_interval

parse_interval_undefined_test() ->
  ?assertEqual({error, missing}, product_scraper_handler:parse_interval(undefined)).

parse_interval_valid_test() ->
  ?assertEqual({ok, 30}, product_scraper_handler:parse_interval(<<"30">>)).

parse_interval_invalid_test() ->
  ?assertEqual({error, invalid}, product_scraper_handler:parse_interval(<<"abc">>)).

parse_interval_zero_test() ->
  ?assertEqual({error, invalid}, product_scraper_handler:parse_interval(<<"0">>)).

%% group_by_time

group_by_time_groups_by_scraped_at_test() ->
  Products = [
    #product{id = 1, price = 10.0, scraped_at = 1000},
    #product{id = 2, price = 20.0, scraped_at = 1000},
    #product{id = 3, price = 30.0, scraped_at = 2000}
  ],
  Grouped = product_scraper_handler:group_by_time(Products),
  ?assertEqual(2, maps:size(Grouped)).

group_by_time_same_time_in_same_group_test() ->
  Products = [
    #product{id = 1, price = 10.0, scraped_at = 1000},
    #product{id = 2, price = 20.0, scraped_at = 1000}
  ],
  Grouped = product_scraper_handler:group_by_time(Products),
  TimeKey = product_scraper_handler:format_time(1000),
  ?assertEqual(2, length(maps:get(TimeKey, Grouped))).