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

file.exists(filename_png)

# works
filename_html <- file.path(my_paths$outputs, "map1.html")
mapshot(map1, url = filename_html)
# getwd()

library(webshot)
filename_png <- file.path(my_paths$outputs, "map1.png")
webshot(map1, file = filename_png)

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
  
# mapviewOptions(fgb = FALSE) # getting mime type error with fgb = TRUE
# m <- mapview(breweries)
mapview::mapshot2(map1, file = filename_png)
file.exists(filename_png)

# mapshot(map1, file = filename_png,  useragent = 'Mozilla/5.0 (compatible; MSIE 10.6; Windows NT 6.1; Trident/5.0; InfoPath.2; SLCC1; .NET CLR 3.0.4506.2152; .NET CLR 3.5.30729; .NET CLR 2.0.50727) 3gpp-gba UNTRUSTED/1.0')
# 
# tmap_save(map1, filename_html)
# webshot2::webshot(filename_html)

# detach("package:mapview", unload=TRUE)
# detach("package:tibble", unload=TRUE)
# detach("package:sf", unload=TRUE)
# detach("package:leaflet", unload=TRUE)
# 

## ==== map each ====
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

map_list <- lapply(mrip_fhier_by_state_split_itis,
                   function(x) {
                     fish = str_replace(x$common_name[1],
                                        "\\W+", "_")
                     filename_png = file.path(my_paths$outputs,
                                         r"(compare_catch\maps)",
                                         paste0(fish,
                                                ".png"
                                         )
                     )
                     a_map <- to_map(x, jitter_factor = 1)
                     mapview::mapshot2(a_map, file = filename_png)
                   }
)
head(map_list)

