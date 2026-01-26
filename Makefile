PROJECT = product_scraper
PROJECT_DESCRIPTION = Test project for scrapping products
PROJECT_VERSION = 0.1.0

DEPS = cowboy gun jiffy
dep_cowboy = git https://github.com/ninenines/cowboy 2.14.2
dep_gun = git https://github.com/ninenines/gun 2.2.0
dep_jiffy = git https://github.com/davisp/jiffy 1.1.2

include erlang.mk
