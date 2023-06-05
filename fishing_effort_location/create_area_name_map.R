library(tictoc)
# to keep the light basemaps
mapviewOptions(basemaps.color.shuffle = FALSE)

# run st_difference with benchmark
with_st_difference <- function(points_sf, polygons_sf) {
  sf::sf_use_s2(FALSE)

  # browser()
  # get param names
  par1 <- rlang::enexpr(points_sf)
  par2 <- rlang::enexpr(polygons_sf)
  
  # start time
  tic(paste0("sf::st_difference(", par1, ", ", par2, ")"))
  res <- sf::st_difference(points_sf, polygons_sf)
  # print time
  toc()
  return(res)
}

m_s <- mapview(
  sa_shp,
  col.regions = "#F4E3FF",
  alpha.regions = 0.2,
  layer.name = "South Altlantic",
  legend = FALSE
)

m_g_r <- mapview(
  gom_reef_shp,
  col.regions = "lightblue",
  alpha.regions = 0.2,
  layer.name = "GOM Reef Fish EFH",
  legend = FALSE
)

lat_long_area <- db_data_w_area %>%
  mutate(TRIP_START_M =
           format(TRIP_START_DATE, "%m")) %>%
  unique()

# all_points <- dim(lat_long_area)[1]
# 75536

# clean coordinates and cut by coordinate boundaries ----
how_many_points_to_draw <- 10000

lat_long_area_clean <- clean_lat_long(lat_long_area, how_many_points_to_draw)
dim(lat_long_area_clean)[1]
# 28008
# 397 (1000)
# 5250 (10000)

# exclude GOM "region" ----
lat_long_area_clean_no_gom <-
  lat_long_area_clean %>%
  filter(!REGION %in% c("GULF OF MEXICO"))

# create sf ----
lat_long_area_clean_sf <-
  lat_long_area_clean_no_gom %>%
  mutate(
    POINT = paste(
      LATITUDE,
      LONGITUDE,
      TRIP_START_M,
      AREA_NAME,
      SUB_AREA_NAME,
      AREA_CODE,
      DISTANCE_CODE_NAME,
      REGION,
      sep = ", "
    )
  ) %>%
  my_to_sf()

lat_long_area_clean_map <-
  lat_long_area_clean_sf %>%
  mapview(
    # zcol = "END_PORT_NAME",
    zcol = "AREA_NAME",
    col.regions = viridisLite::turbo,
    layer.name = 'AREA_NAME',
    # cex = "DISTANCE_CODE_NAME",
    alpha = 0.3,
    legend = F
  )

m_s + m_g_r + lat_long_area_clean_map

# 2) remove GOM reef polygon 

lat_long_area_clean_sf_no_gom_reef_sf <-
  sf::st_difference(lat_long_area_clean_sf, gom_reef_shp)
# 540.17 sec elapsed
# fewer points
# 90.67 sec elapsed

### or read csv ----
lat_long_area_clean_sf_no_gom_reef_sf_file_name <- 
file.path(my_paths$outputs, current_project_name, "lat_long_area_clean_sf_no_gom_reef_sf_short.csv")

lat_long_area_clean_sf_no_gom_reef_sf <-
  read_sf(lat_long_area_clean_sf_no_gom_reef_sf_file_name) %>%
  my_to_sf()

write_csv(
  lat_long_area_clean_sf_no_gom_reef_sf,
  lat_long_area_clean_sf_no_gom_reef_sf_file_name
)

lat_long_area_clean_sf_no_gom_reef_sf_map <-
  lat_long_area_clean_sf_no_gom_reef_sf %>%
  mapview(
    # zcol = "END_PORT_NAME",
    zcol = "AREA_NAME",
    col.regions = viridisLite::turbo,
    layer.name = 'AREA_NAME',
    # cex = "DISTANCE_CODE_NAME",
    alpha = 0.3,
    legend = F
  )

m_s + m_g_r + lat_long_area_clean_sf_no_gom_reef_sf_map
