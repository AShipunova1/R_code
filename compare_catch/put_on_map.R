# put coordinates on a shapefile
# Using https://www.r-bloggers.com/2014/07/clipping-spatial-data-in-r/
#

library(broom)
library(mapview)
library(leafsync)

library(sf)
library(leaflet)
library(leafem)
# 
#   # filename <- "~/R_code/m_subset.png"
#   # # "c:\Users\anna.shipunova\Documents\R_code\get_subset_f.R"
#   # # file.exists("~/R_code/get_subset_f.R")
#   # mapshot(m_subset, file = filename, selfcontained = FALSE)
# 
# ## ==== map each ====
# 
# map_list <- lapply(most_frequent_fhier10_w_info_state_cnts_abbr_list,
#                    function(x) {x_sf = st_as_sf(x,
#                                                 coords = c("longitude", "latitude"),
#                                                 crs = 4326)
#                    # browser()
#                    mapview(x_sf,
#                            zcol = "name_cnts"
#                    )
#                    }
# )
# map_list[[1]] + m_s + m_g
# all_maps <- Reduce("+", map_list) + m_s + m_g

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
  mutate(name_cnts = paste(new_sta, AGENCY, CATCH_CNT)) %>%
  ungroup()

mrip_fhier_by_state_long %>% head()

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
    mutate(latitude = jitter(latitude, factor = jitter_factor)) %>%
    mutate(longitude = jitter(longitude, factor = jitter_factor)) %>%
    st_as_sf(coords = c("longitude",
                        "latitude"),
             crs = 4326)
  mapview(x_sf,
          zcol = "name_cnts",
          cex = "CATCH_CNT",
          alpha = 0.3,
          col.regions = viridisLite::turbo,
          legend = TRUE,
          layer.name = mrip_fhier_by_state_df$common_name[1]
          )
}

# mrip_fhier_by_state_split_itis[[1]]$common_name[1]

first_sp_map <- to_map(mrip_fhier_by_state_split_itis[[1]],
                       jitter_factor = 1)


m_s <- mapview(sa_shp,
               layer.name = "South Altlantic",
               legend = FALSE)
m_g <- mapview(gom_shp,
               layer.name = "Gulf of Mexico",
               legend = FALSE)

map1 <- first_sp_map + m_s + m_g

# doesn't work'
mapviewOptions(fgb = FALSE)
filename_png <- file.path(my_paths$outputs, "map1.png")
mapshot2(map1, file = filename_png, selfcontained = F)
file.exists(filename_png)

# works
filename_html <- file.path(my_paths$outputs, "map1.html")
mapshot(map1, url = filename_html)
# getwd()


mapshot(map1, file = filename_png,
        remove_controls = c("homeButton", "layersControl"))


# map_title <-
#   mrip_fhier_by_state_split_itis[[1]] %>% 
#   select(common_name) %>%
#   unique()

# first_sp_map@map %>%
#     addTiles() %>% addLegend(
#       position = "topright",
#       colors = as.factor(mrip_fhier_by_state_split_itis[[1]]$CATCH_CNT),
#       labels = mrip_fhier_by_state_split_itis[[1]]$name_cnts,
#       opacity = 1,
#       title = map_title$common_name
#     )
  
