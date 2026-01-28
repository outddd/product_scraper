-ifndef(PRODUCT_SCRAPER).
-define(PRODUCT_SCRAPER, true).

-record(product, {
  id          :: non_neg_integer(),
  price       :: float(),
  scraped_at  :: non_neg_integer()
}).

-type product() :: #product{}.

-endif.