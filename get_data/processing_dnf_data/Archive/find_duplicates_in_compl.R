# The below section is an example of the many to many relationship, using 2023 data (to check, remove 'relationship = "many-to-many"' from the join.)
# We need 'relationship = "many-to-many"':
# ℹ Row 46609 of `x` matches multiple rows in `y`.
in_x_2023 <- SEFHIER_dnfs_short_date__iso[46609, ]
in_x_2022 <- SEFHIER_dnfs_short_date__iso[47376, ]

in_x <- in_x_2022
compl_override_data__renamed__this_year |>
  filter(VESSEL_OFFICIAL_NUMBER ==
           in_x$VESSEL_OFFICIAL_NUMBER &
           COMP_YEAR == in_x$TRIP_DATE_YEAR &
           COMP_WEEK == in_x$TRIP_DATE_WEEK) |>
  glimpse()
# An error in the compliance report, duplicate records.

# ℹ Row 22748 of `y` matches multiple rows in `x`.
# We need the many to many relationship because the DNFs represent a single day in a 7 day week, while the compliance represents a single week. So the relationship between DNFs to Compliance is 7 to 1.

# Find all duplicates:
# 1)
duplicates_in_compl <-
  compl_override_data__renamed__this_year |>
  select(VESSEL_OFFICIAL_NUMBER,
         COMP_YEAR,
         COMP_WEEK,
         SRH_VESSEL_COMP_ID) |>
  group_by(VESSEL_OFFICIAL_NUMBER, COMP_YEAR, COMP_WEEK) |>
  count() |>
  filter(n > 1) |>
  ungroup()

# 2)
dupe2 <-
  compl_override_data__renamed__this_year |>
  select(VESSEL_OFFICIAL_NUMBER,
         COMP_YEAR,
         COMP_WEEK,
         SRH_VESSEL_COMP_ID) |>
  group_by(VESSEL_OFFICIAL_NUMBER, COMP_YEAR, COMP_WEEK) |>
  mutate(dups = any(duplicated(VESSEL_OFFICIAL_NUMBER, COMP_YEAR, COMP_WEEK))) %>%
  ungroup()

compl_override_data__renamed__this_year |>
  filter(VESSEL_OFFICIAL_NUMBER == "694424" &
           COMP_YEAR == 2022 &
           COMP_WEEK == 33) |> glimpse()

# In FHIER only one entry without overridden

dupe_entries <-
  dupe2 |> filter(dups == TRUE) |> select(SRH_VESSEL_COMP_ID)

compl_override_data__renamed__this_year |>
  filter(SRH_VESSEL_COMP_ID %in% dupe_entries$SRH_VESSEL_COMP_ID) |>
  arrange(VESSEL_OFFICIAL_NUMBER, COMP_WEEK) |>
  # filter(VESSEL_OFFICIAL_NUMBER == "694424") |>
  glimpse()

# 3)
compl_override_data__renamed__this_year_dups <-
  compl_override_data__renamed__this_year |>
  group_by(VESSEL_OFFICIAL_NUMBER, COMP_YEAR, COMP_WEEK) |>
  mutate(duplicated = n() > 1) |>
  filter(duplicated == TRUE)

compl_override_data__renamed__this_year_dups |>
  arrange(VESSEL_OFFICIAL_NUMBER, COMP_WEEK) |>
  filter(VESSEL_OFFICIAL_NUMBER == "694424") |>
  glimpse()
