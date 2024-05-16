# From https://stackoverflow.com/questions/35369897/find-dates-that-fail-to-parse-in-r-lubridate

library(lubridate)

parse_mdy = function(x) {
  parsed_dates <- 
    lubridate::parse_date_time(x, orders = c("m/d/Y"), quiet = TRUE)
  
  errors = x[!is.na(x) & is.na(parsed_dates)]
  if (length(errors) > 0) {
    cli::cli_warn("Failed to parse some dates: {.val {errors}}")
  }
  parsed_dates
}

all_dates <- 
  # c("10/13/2006", "05/16/2024")
  unique(permits_from_pims__split1$statusdate)
  # c("2014/20/21", "2014/01/01", NA, "2014/01/02", "foobar")
my_date = lubridate::mdy(all_dates,
                         c("m/d/Y"))
#> Warning: 2 failed to parse.
my_date = parse_mdy(all_dates)
#> Warning: Failed to parse some dates: "2014/20/21" and "foobar"
