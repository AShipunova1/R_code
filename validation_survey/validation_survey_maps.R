# overlay map of where surveys occurred without logbooks (in Gulf) with where non-compliant Gulf + dual Gulf/S. Atl. vessels (for all 2022) are located

# setup maps ----
needed_packages <- c("tidygeocoder", "ggmap", "usmap")

installed_packages <-
  needed_packages %in% rownames(installed.packages())

if (any(installed_packages == FALSE)) {
  install.packages(needed_packages[!installed_packages])
}

lapply(needed_packages, library, character.only = TRUE)

water_shape_prep_path <-
  file.path(my_paths$git_r,
            "get_data",
            "waters_shape_prep.R")

file.exists(water_shape_prep_path)

source(water_shape_prep_path)

# south_east_coast_states
# east_coast_states
# sa_council_states
# south_atlantic_states
# fl_counties
# my_state_abb
# my_state_name
# GOMsf
# world_state_and_fed_waters_path
# fl_state_w_counties_shp
# GOM_s_fl_state_waters_only
# big_bounding_box
# shp_4326_list: 
# east_coast_sa_state_waters_shp
# gom_fl_state_w_counties_shp
# sa_fl_state_w_counties_shp
# sa_shp
# gom_states_shp
# sa_states_shp

# Map interviews ----
## map interviews using survey data ----

lgb_join_i1__int_lgb__short_for_map_no_lgb_fips <-
  lgb_join_i1__int_lgb__short_for_map_no_lgb |>
  mutate(
    county_fips_surv = paste0(st_2, cnty_3),
    county_fips_pims = paste0(state_code, county_code)
  )

# fips The 5-digit FIPS code corresponding to the county.
# 
# abbr The 2-letter state abbreviation.

lgb_join_i1__int_lgb__short_for_map_no_lgb_fips_surv <-
  lgb_join_i1__int_lgb__short_for_map_no_lgb_fips |>
  select(-c(
    VESSEL_OFFICIAL_NBR,
    county_short,
    state_code,
    state_name,
    county_code,
    county_fips_pims
  )) |>
  distinct() |>
  rename(fips = county_fips_surv, abbr = st_2)

lgb_join_i1__int_lgb__short_for_map_no_lgb_fips_surv_cnt <-
  lgb_join_i1__int_lgb__short_for_map_no_lgb_fips_surv |>
  # select(id_code, fips) |>
  add_count(fips, name = "cnt_surv")

usmap::plot_usmap(
  data = lgb_join_i1__int_lgb__short_for_map_no_lgb_fips_surv_cnt,
  values = "cnt_surv",
  include = lgb_join_i1__int_lgb__short_for_map_no_lgb_fips_surv$abbr,
  color = "green"
) +
  scale_fill_continuous(
    # low = "white",
    high = "green",
    name = "Trips",
    na.value = "white",
    label = scales::comma
  ) +
  labs(title = "Count interviews") +
  theme(legend.position = "right")

## count interview using restored data ----
lgb_join_i1__int_lgb__short_for_map_no_lgb_fips_restored <-
  lgb_join_i1__int_lgb__short_for_map_no_lgb_fips |>
  select(-c(
    survey_vessel_id,
    st_2,
    cnty_3,
    county_fips_surv
  )) |>
  distinct() |>
  rename(fips = county_fips_pims, abbr = state_code)

lgb_join_i1__int_lgb__short_for_map_no_lgb_fips_restored_cnt <-
  lgb_join_i1__int_lgb__short_for_map_no_lgb_fips_restored |>
  add_count(fips, name = "cnt_surv")

usmap::plot_usmap(
  data = lgb_join_i1__int_lgb__short_for_map_no_lgb_fips_restored_cnt,
  values = "cnt_surv",
  include = unique(gom_states_shp$STUSPS),
  color = "green"
) +
  scale_fill_continuous(
    # low = "white",
    high = "green",
    name = "Trips",
    na.value = "white",
    label = scales::comma
  ) +
  labs(title = "Count interviews using restored data") +
  theme(legend.position = "right")

# map non compliant GOM and dual vessels for 2022 ----

processed_logbooks_2022_calendar$IS_COMP |> unique()
processed_logbooks_2022_calendar$compliant_after_override |> unique()

# processed_logbooks_2022_calendar_short <-
  processed_logbooks_2022_calendar |>
  filter(!permit_sa_gom == "sa_only") |>
    count(IS_COMP)
  
#     IS_COMP      n
#     <int>  <int>
# 1       0   7132
# 2       1 187530

processed_logbooks_2022_calendar_non_comp_gom_short <-
    processed_logbooks_2022_calendar |>
    filter(!permit_sa_gom == "sa_only") |>
    filter(IS_COMP == 0) |>
    select(TRIP_ID,
           VESSEL_OFFICIAL_NUMBER,
           END_PORT_COUNTY,
           END_PORT_STATE) |>
    distinct()
  
dim(processed_logbooks_2022_calendar_non_comp_gom_short)
# 1968

processed_logbooks_2022_calendar_non_comp_gom_short |> 
  glimpse()

## convert lgb counties to fips ----

processed_logbooks_2022_calendar_non_comp_gom_short__fips <-
  processed_logbooks_2022_calendar_non_comp_gom_short |>
  rowwise() |>
  mutate(END_PORT_fips =
           try(usmap::fips(state = END_PORT_STATE, county = END_PORT_COUNTY))
  ) |>
  ungroup()

glimpse(processed_logbooks_2022_calendar_non_comp_gom_short__fips)
