# setup maps ----
needed_packages <- c("tidygeocoder", "ggmap")

installed_packages <-
  needed_packages %in% rownames(installed.packages())

if (any(installed_packages == FALSE)) {
  install.packages(needed_packages[!installed_packages])
}

lapply(needed_packages, library, character.only = TRUE)

devtools::install_github("UrbanInstitute/urbnmapr")
library(urbnmapr)

water_shape_prep_path <-
  file.path(my_paths$git_r,
            "get_data",
            "waters_shape_prep.R")

file.exists(water_shape_prep_path)

source(water_shape_prep_path)

# get city coords ----

lgb_join_i1__int_lgb__short_for_map_no_lgb_ok <-
  lgb_join_i1__int_lgb__short_for_map_no_lgb |>
  filter(st_2 == state_code) |> 
  select(-c(st_2)) |> 
  distinct()

lgb_join_i1__int_lgb__short_for_map_no_lgb_ok |> 
    filter(!VESSEL_OFFICIAL_NBR == survey_vessel_id) |> 
    glimpse()
# 30

dim(lgb_join_i1__int_lgb__short_for_map_no_lgb_ok)
# 631
lgb_join_i1__int_lgb__short_for_map_no_lgb |>
  dim()
# 966

lgb_join_i1__int_lgb__short_for_map_no_lgb |> 
  mutate(county_fips_surv = paste0(st_2, cnty_3),
         county_fips_pims = paste0(state_code, county_code))

urbnmapr::counties |> 
  left_join(counties, by = "county_fips") |> glimpse()
  
  
  
  filter(state_name =="California") |> 
  ggplot(mapping = aes(long, lat, group = group, fill = horate)) +
  geom_polygon(color = "#ffffff", size = .25) +
  scale_fill_gradientn(labels = scales::percent,
                       guide = guide_colorbar(title.position = "top")) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme(legend.title = element_text(),
        legend.key.width = unit(.5, "in")) +
  labs(fill = "Homeownership rate") +
  theme_urban_map()