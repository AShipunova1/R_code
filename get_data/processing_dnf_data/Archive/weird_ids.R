### find wrong COAST_GUARD_NBR or STATE_REG_NBR ----
# State reg numbers follow a specific format XX####XX.
# Coast guard number has 7 digits.

#### STATE_REG_NBR ----

not_standard_state_reg_nbr <-
  grep(
    "[[:alpha:]]{2}\\d{4}[[:alpha:]]{2}",
    dnfs$STATE_REG_NBR,
    invert = T,
    value = T
  ) |>
  unique()

length(not_standard_state_reg_nbr)
# 196

#### COAST_GUARD_NBR ----
not_standard_coast_guard_nbr <-
  grep(
    "^\\d{7}$",
    dnfs$COAST_GUARD_NBR,
    invert = T,
    value = T
  ) |>
  unique()

length(not_standard_coast_guard_nbr)
# 620

COAST_GUARD_NBR_len <-
  dnfs |>
  filter(!is.na(COAST_GUARD_NBR)) |>
  rowwise() |>
  mutate(coast_guard_nbr_len = str_length(COAST_GUARD_NBR)) |>
  ungroup() |>
  select(COAST_GUARD_NBR, coast_guard_nbr_len) |>
  distinct()

COAST_GUARD_NBR_len |> count(coast_guard_nbr_len)
# 4     1
# 5     1
# 6   617
# 7   805

# head(COAST_GUARD_NBR_len)
dnfs_coast_guard_nbr <-
  dnfs |>
  filter(!is.na(COAST_GUARD_NBR)) |>
  select(COAST_GUARD_NBR) |>
  distinct()

not_6dig_coast_guard_nbr <-
  grep(
    "\\d{6}",
    dnfs_coast_guard_nbr$COAST_GUARD_NBR,
    invert = T,
    value = T
  ) |>
  unique()

# not_6dig_coast_guard_nbr

#### write out wrong ids ----
not_na_not_standard_state_reg_nbr <-
  dnfs |>
  filter((!is.na(STATE_REG_NBR)) &
           STATE_REG_NBR %in% not_standard_state_reg_nbr) |>
  select(VESSEL_ID,
         COAST_GUARD_NBR,
         STATE_REG_NBR,
         VESSEL_OFFICIAL_NUMBER) |>
  distinct()

# nrow(not_na_not_standard_state_reg_nbr)
# 197

not_na_not_standard_coast_guard_nbr <-
  dnfs |>
  filter((!is.na(COAST_GUARD_NBR)) &
           COAST_GUARD_NBR %in% not_6dig_coast_guard_nbr) |>
  select(VESSEL_ID,
         COAST_GUARD_NBR,
         STATE_REG_NBR,
         VESSEL_OFFICIAL_NUMBER) |>
  distinct()

# nrow(not_na_not_standard_coast_guard_nbr)
# 2

not_standard_ids <-
  rbind(not_na_not_standard_state_reg_nbr,
        not_na_not_standard_coast_guard_nbr)

not_standard_ids_file_path <-
  file.path(Path,
            Outputs,
            str_glue("not_standard_ids_{my_year}.csv"))

write_csv(not_standard_ids,
          file = not_standard_ids_file_path)

