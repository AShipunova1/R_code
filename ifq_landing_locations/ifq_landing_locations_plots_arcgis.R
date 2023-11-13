# make sf ----
# In summary, the code takes the 'input_data_convert_dms_short_clean_short_cnt_short' data frame, adds a new factor column 'year_fct' based on the 'USER_NYEAR' column, and then converts the resulting data frame into a spatial object using the 'sf' package. The spatial object is defined with coordinates based on 'use_lon_round' and 'use_lat_round', and it uses the EPSG 4326 CRS (WGS 84). NA values are allowed in the spatial object. The final result is stored in 'input_data_convert_dms_short_clean_short_cnt_sf'.

input_data_convert_dms_short_clean_short_cnt_sf <-
  input_data_convert_dms_short_clean_short_cnt |> 
  # input_data_convert_dms_short_clean_short_cnt_short |>
  mutate(
    year_fct = factor(USER_NYEAR)
  ) |>
  sf::st_as_sf(
    coords = c("use_lon_round", "use_lat_round"),
    crs = 4326,
    na.fail = FALSE
  )

# glimpse(input_data_convert_dms_short_clean_short_cnt_sf)

## plot_by_year (arcGis) ---- 

plot_by_year <- ggplot() +  # Initialize a ggplot object.
  
  # Add a spatial feature layer to the plot using 'geom_sf' and 'input_data_convert_dms_short_clean_short_cnt_sf' as data.
  # Map the size aesthetic to 'count_by_year_and_coord'.
  geom_sf(data = st_union_GOMsf) +
  geom_sf(data = input_data_convert_dms_short_clean_short_cnt_sf,
          mapping = aes(size = count_by_year_and_coord),
          color = "red") + 
  # Create facets based on year, arranging them in a 3-column layout.
  facet_wrap(vars(year_fct), ncol = 3) +
  
  ggtitle("IFQ Landing Locations") +
  
  theme(legend.position = "bottom") +
  
  # Customize the legend for the size aesthetic.
  guides(size = guide_legend(title = "Counts by year and place"))

# tic("plot_by_year")
plot_by_year
# toc()
# 
output_file_name <- 
  "facets_by_year_w_gom_shp_tidy_geo.png"

# output_file_name <- 
#   "facets_by_year_w_states.png"

ggsave(
  file = output_file_name,
  plot = plot_by_year,
  device = "png",
  path = file.path(my_paths$outputs,
                   current_project_dir_name),
  width = 30,
  height = 20,
  units = "cm"
)

# By year, map the landing location with somehow displaying which locations are used the most. ----
# I think we can do this with color coding.

# zcol = "my_label",  # Use the 'my_label' column for labeling map points.
# cex = "total_place_cnt",  # Control the size of map points based on 'total_place_cnt'.
# alpha = 0.3,  # Set the transparency of map points to 0.3 (partially transparent).
# col.regions = viridisLite::turbo,  # Define the color palette for map points using 'turbo' from 'viridisLite'.

input_data_convert_dms_short_clean_short_cnt_sf |>
  mutate(my_label =
           str_glue("{use_addr}; # = {total_place_cnt}")) |>
  mapview(
    zcol = "my_label",
    cex = "total_place_cnt",
    alpha = 0.3,
    col.regions = viridisLite::turbo,
    legend = FALSE,
    layer.name = 'Counts by year and place'
  )
