# From https://stackoverflow.com/questions/35369897/find-dates-that-fail-to-parse-in-r-lubridate

parse_ymd = function(x) {
  d = lubridate::ymd(x, quiet = TRUE)
  errors = x[!is.na(x) & is.na(d)]
  if (length(errors) > 0) {
    cli::cli_warn("Failed to parse some dates: {.val {errors}}")
  }
  d
}

x = c("2014/20/21", "2014/01/01", NA, "2014/01/02", "foobar")
my_date = lubridate::ymd(permits_from_pims__split1$statusdate)
#> Warning: 2 failed to parse.
my_date = parse_ymd(permits_from_pims__split1$statusdate)
#> Warning: Failed to parse some dates: "2014/20/21" and "foobar"
