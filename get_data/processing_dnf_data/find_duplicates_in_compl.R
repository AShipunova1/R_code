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

# All duplicates:
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


dup_SRH_VESSEL_COMP_ID <-
  compl_override_data__renamed__this_year |>
  group_by(VESSEL_OFFICIAL_NUMBER, COMP_YEAR, COMP_WEEK) |>
  mutate(dup_entries_in_compl = list(toString(unique(sort(SRH_VESSEL_COMP_ID))))) |>
  # mutate(SRH_VESSEL_COMP_IDs = paste(SRH_VESSEL_COMP_ID)) |>
  ungroup()
# |>
  # mutate_if(is.list, ~paste(unlist(.), collapse = ', ')) %>%

# data[duplicated(data[,2:3]),]

# str(dup_SRH_VESSEL_COMP_ID)
dup_SRH_VESSEL_COMP_ID |>
  mutate(dup_entries_in_compl_len =
           length(dup_entries_in_compl)) |>
  filter(dup_entries_in_compl_len > 1) |>
  glimpse()
