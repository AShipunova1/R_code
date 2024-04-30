## check dnf dates ----

names(SEFHIER_dnfs_short_date__iso) |>
  cat(sep = ", ")

SEFHIER_dnfs_short_date__iso__time_only <-
  SEFHIER_dnfs_short_date__iso |>
  mutate(across(
    where(is.POSIXct),
    .fns = ~ format(.x, "%H%M%S"),
    .names = "{.col}_time_only"
  ))

# DE_time_only
SEFHIER_dnfs_short_date__iso__time_only_23 <-
  SEFHIER_dnfs_short_date__iso__time_only |>
  # select(-USABLE_DATE_TIME_time_only) |>
  filter(if_any(
    .cols = ends_with("_time_only"),
    .fns = ~ grepl("^23", .x)
  ))

SEFHIER_dnfs_short_date__iso__time_only_23 |>
  select(TRIP_ID,
         VESSEL_OFFICIAL_NUMBER,
         TRIP_DATE,
         TRIP_DATE_WEEK,
         DE) |>
  distinct() |>
  head()

