# put coordinates on a shapefile
# Using https://www.r-bloggers.com/2014/07/clipping-spatial-data-in-r/
#
# remotes::install_github("r-spatial/mapview")

library(broom)
library(mapview)
library(leafsync)

library(sf)
# library(leaflet)
library(leafem)

## ---- map mrip_fhier_by_state by common name ----
# names(mrip_fhier_by_state)
mrip_fhier_by_state_long <-
  mrip_fhier_by_state %>%
  rename(c("MRIP" = "mrip_estimate_catch_by_species",
           "FHIER" = "fhier_quantity_by_sp_n_state10")) %>%
  # reformat to a long format to have fhier and mrip data side by side
  pivot_longer(
    cols = c(MRIP,
             FHIER),
    names_to = "AGENCY",
    values_to = "CATCH_CNT"
  ) %>% 
  dplyr::mutate(name_cnts = paste(new_sta, AGENCY, CATCH_CNT)) %>%
  ungroup()

# mrip_fhier_by_state_long %>% head()

mrip_fhier_by_state_split_itis <-
  split(mrip_fhier_by_state_long,
        f = mrip_fhier_by_state_long$itis_code)

# str(mrip_fhier_by_state_split_itis[[1]])

to_map <- function(mrip_fhier_by_state_df,
                   jitter_factor = NA
                     ) {
  if(is.na(jitter_factor)) jitter_factor = 0
  x_sf <-
    mrip_fhier_by_state_df %>%
    dplyr::mutate(latitude = jitter(latitude, factor = jitter_factor)) %>%
    dplyr::mutate(longitude = jitter(longitude, factor = jitter_factor)) %>%
    st_as_sf(coords = c("longitude",
                        "latitude"),
             crs = 4326)
  mapview(x_sf,
          zcol = "name_cnts",
          cex = "CATCH_CNT",
          alpha = 0.3,
          col.regions = viridisLite::turbo,
          legend = FALSE,
          layer.name = mrip_fhier_by_state_df$common_name[1]
          ) %>%
  addStaticLabels(label = mrip_fhier_by_state_df$name_cnts,
                  # noHide = TRUE,
                  direction = 'top',
                  # textOnly = TRUE,
                  textsize = "10px")
  
}

first_sp_map <- to_map(mrip_fhier_by_state_split_itis[[1]],
                       jitter_factor = 1)
m_s <- mapview(sa_shp,
               layer.name = "South Altlantic",
               legend = FALSE)
m_g <- mapview(gom_shp,
               layer.name = "Gulf of Mexico",
               legend = FALSE)

# map1 <- first_sp_map + m_s + m_g

## ---- safe into files ----
# works
filename_html <- file.path(my_paths$outputs, "map1.html")
# mapshot(map1, url = filename_html)
# getwd()

# mapview::mapshot2(map1, file = filename_png)
# file.exists(filename_png)

## ==== map each ====
all_10_png <- function(mrip_fhier_by_state_split_itis) {
  map_list <- lapply(mrip_fhier_by_state_split_itis,
                     function(x) {
                       fish = str_replace(x$common_name[1],
                                          "\\W+", "_")
                       filename_png = file.path(my_paths$outputs,
                                                r"(compare_catch\maps)",
                                                paste0(fish,
                                                       ".png"))
                       a_map <- to_map(x, jitter_factor = 1)
                       mapview::mapshot2(a_map, file = filename_png)
                     })
  head(map_list)
}

## ---- FHIER: fish coords lat_lon_cnts to map ----
lat_lon_cnts_rename <-
  lat_lon_cnts %>%
  dplyr::mutate(name_cnts = paste(latitude, longitude, fhier_quantity_by_sp_geo)) %>%
  dplyr::mutate(CATCH_CNT = fhier_quantity_by_sp_geo)

fhier_lat_lon_split_itis <-
  split(lat_lon_cnts_rename,
        f = lat_lon_cnts_rename$catch_species_itis)

a_map <- to_map(fhier_lat_lon_split_itis[[5]])
