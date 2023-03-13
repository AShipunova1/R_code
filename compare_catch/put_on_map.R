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
# ## ---- split by com name ----
# most_frequent_fhier10_w_info_state_cnts_abbr_list <-
#   most_frequent_fhier10_w_info_state_cnts_abbr %>%
#   mutate(name_cnts = paste(common_name, fhier_quantity_by_sp_n_state10)) %>%
#   split(most_frequent_fhier10_w_info_state_cnts_abbr, 
#         f = most_frequent_fhier10_w_info_state_cnts_abbr$common_name)
# 
# str(most_frequent_fhier10_w_info_state_cnts_abbr_list)
# 
# # zcol = paste("state_name",                                                      "fhier_quantity_by_sp_n_state10")
# 
# all3 <- m_state_geo + m_s + m_g
# 
# #                    # %>%
# #   mapview(most_frequent_fhier10_w_info_state_cnts_abbr_geo, 
# #           zcol = "state_name")
# # all3 <- m_my_states_geo + m_s + m_g
# # str(map_list)
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

# mrip_fhier_by_state_split <-
#   split(mrip_fhier_by_state_long,
#         list(mrip_fhier_by_state_long$AGENCY,
             # mrip_fhier_by_state_long$itis_code))

mrip_fhier_by_state_split_itis <-
  split(mrip_fhier_by_state_long,
        f = mrip_fhier_by_state_long$itis_code)

# str(mrip_fhier_by_state_split_itis[[1]])

to_map <- function(mrip_fhier_by_state_df,
                   jitter_factor = NA
                   # ,
                   # my_color = "green"
                     ) {
  # browser()
  if(is.na(jitter_factor)) jitter_factor = 0
  
  x_sf <-
    mrip_fhier_by_state_df %>%
    mutate(latitude = jitter(latitude, factor = jitter_factor)) %>%
    mutate(longitude = jitter(longitude, factor = jitter_factor)) %>%
    st_as_sf(coords = c("longitude",
                        "latitude"),
             crs = 4326)
  # browser()
  mapview(x_sf,
          zcol = "name_cnts",
          cex = "CATCH_CNT",
          alpha = 0.3,
          col.regions = viridisLite::turbo,
          # legend = FALSE
          legend = TRUE,
          layer.name = mrip_fhier_by_state_df$common_name[1]
          )
}

# mrip_fhier_by_state_split_itis[[1]]$common_name[1]

first_sp_map <- to_map(mrip_fhier_by_state_split_itis[[1]],
                       jitter_factor = 1)
# scnd_sp_map <- to_map(mrip_fhier_by_state_split[[2]])
# to_map(first_sp, 10) + 
# to_map(scnd_sp) + m_s + m_g + to_map(first_sp, 10)


m_s <- mapview(sa_shp)
m_g <- mapview(gom_shp)
first_sp_map + m_s + m_g
# %>% 
#   addTiles() %>% addLegend(
#     position = "bottomright",
#     colors = rgb(t(col2rgb(palette())) / 255),
#     labels = palette(), opacity = 1,
#     title = "An Obvious Legend"
#   )

# mapview objects have a @map slot where the leaflet map is stored, hence you can simply do:
#   
#   m@map %>%addMarkers(~Longitude, ~Latitude, label=~Name, labelOptions=labelOptions(noHide=T),data=rawdata)


# nc = st_read(system.file("gpkg/nc.gpkg", package="sf"))
# pts = st_centroid(st_geometry(nc))
# plot(pts)
# plot(st_jitter(pts, .05), add = TRUE, col = 'red')
# plot(st_geometry(nc))
# plot(st_jitter(st_geometry(nc), factor = .01), add = TRUE, col = '#ff8888')
# str(nc)

to_sp <- function(mrip_fhier_by_state_df) {
  # browser()
  x_sf <-
    mrip_fhier_by_state_df %>%
    st_as_sf(coords = c("longitude",
                        "latitude"),
             crs = 4326)
}
str(first_sp_map)
# m_s + m_g + 
first_sp_map@map %>%
    addTiles() %>% addLegend(
      position = "topright",
      colors = as.factor(mrip_fhier_by_state_split_itis[[1]]$CATCH_CNT),
      labels = mrip_fhier_by_state_split_itis[[1]]$name_cnts,
      opacity = 1,
      title = map_title$common_name
    )
  
map_title <-
mrip_fhier_by_state_split_itis[[1]] %>% 
  select(common_name) %>%
  unique()
