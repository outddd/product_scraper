-module(product_scraper_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	Procs = [
		#{id => storage,
			start => {product_scraper_storage, start_link, []},
			restart => permanent,
			type => worker},
		#{id => worker,
			start => {product_scraper_worker, start_link, []},
			restart => permanent,
			type => worker}
	],
	{ok, {#{strategy => one_for_one, intensity => 5, period => 10}, Procs}}.
