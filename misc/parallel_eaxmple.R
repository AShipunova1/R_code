# install.packages("furrr")
library(furrr)
library(purrr)
library(tictoc)

# This should take 6 seconds in total running sequentially
plan(sequential)

tic()
nothingness <- future_map(c(2, 2, 2), ~Sys.sleep(.x))
toc()
# 6.47 sec elapsed

# This should take ~2 seconds running in parallel, with a little overhead
# in `future_map()` from sending data to the workers. There is generally also
# a one time cost from `plan(multisession)` setting up the workers.
plan(multisession, workers = 3)

tic()
nothingness <- future_map(c(2, 2, 2), ~Sys.sleep(.x))
toc()
2.11 sec elapsed
