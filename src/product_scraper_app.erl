-module(product_scraper_app).
-behaviour(application).

-export([start/2, stop/1]).

-include_lib("kernel/include/logger.hrl").

start(_Type, _Args) ->
	?LOG_INFO("Starting product_scraper"),

	Dispatch = cowboy_router:compile([
		{'_', [
			{"/assembled-products", product_scraper_handler, []},
			{"/set-time", product_scraper_handler, []}
		]}
	]),

	Port = application:get_env(product_scraper, port, 8080),
	{ok, _} = cowboy:start_clear(http_listener,
		[{port, Port}],
		#{env => #{dispatch => Dispatch}}
	),
	?LOG_INFO("HTTP server listening on port ~B", [Port]),

	product_scraper_sup:start_link().

stop(_State) ->
	?LOG_INFO("Stop product_scraper"),
	cowboy:stop_listener(http_listener),
	ok.
