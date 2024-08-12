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

# get coords ----

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


lgb_join_i1__int_lgb__short_for_map_no_lgb_fips_surv_cnt |> 
# View(lgb_join_i1__int_lgb__short_for_map_no_lgb_fips_surv)

# usmap::plot_usmap(regions = "counties")
usmap::plot_usmap(
  data = lgb_join_i1__int_lgb__short_for_map_no_lgb_fips_surv,
  values = "cnt_trips",
  include = lgb_join_i1__int_lgb__short_for_map_no_lgb_fips_surv$abbr,
  color = "green"
) +
  scale_fill_continuous(
    # low = "white",
    high = "green",
    name = "Trips",
    na.value = "white"
   # label = scales::comma

  ) +
  labs(title = "Count interview dates") +
  theme(legend.position = "right")
