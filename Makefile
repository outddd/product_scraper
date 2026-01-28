PROJECT = product_scraper
PROJECT_DESCRIPTION = Test project for scrapping products
PROJECT_VERSION = 0.1.0

LOCAL_DEPS = mnesia inets ssl
DEPS = cowboy jiffy
dep_cowboy = git https://github.com/ninenines/cowboy 2.14.2
dep_jiffy = git https://github.com/davisp/jiffy 1.1.2

include erlang.mk

run: all
	erl -pa ebin/ -pa deps/*/ebin/ -config config/sys \
	    -eval "application:ensure_all_started(product_scraper)"
