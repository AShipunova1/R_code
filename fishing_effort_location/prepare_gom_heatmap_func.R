# plot text sizes ----
text_sizes <- list(
  geom_text_size = 7,
  plot_title_text_size = 10,
  axis_title_text_size = 9,
  axis_text_x_size = 13,
  axis_text_y_size = 13,
  plot_caption_text_size = 13,
  legend_title_text_size = 10,
  legend_text_text_size = 10,
  ### common axes for Months ----
  y_left_fontsize = 10
)

# read in sa shp ----
# F2 in RStudio will show the function definition, when the cursor is on the name.
sa_shp <- read_shapefile(r"(sa_eaz_off_states\shapefiles_sa_eez_off_states\SA_EEZ_off_states.shp)"
)

# The South Atlantic Council is responsible for the conservation and management of fishery resources in federal waters ranging from 3 to 200 miles off the coasts of North Carolina, South Carolina, Georgia, and east Florida to Key West.

states_sa <- data.frame(
  state_name = c(
    # "Florida", # exclude, we have it separated by county
    "Georgia",
    "North Carolina",
    "South Carolina"
  )
)

# Reformat the R state df (create a DF of state abbreviations and state names as cols; 2x50)
state_tbl <- data.frame(state.abb, tolower(state.name))
names(state_tbl) = c("state_abb", "state_name")

#from the DF, only grab the SA states defined above
sa_state_abb <-
  # a table from above
  state_tbl %>%
  # get only these in our list
  filter(state_name %in% tolower(states_sa$state_name)) %>%
  # get abbreviations
  select(state_abb)

# # add regions to the FHIER logbook DF
# fhier_logbooks_content_waves__sa_gom <-
#   fhier_logbooks_content_waves_fl_county %>%
#   # add a new column "end_port_sa_gom" with sa or gom for each state
#   # use fix_name aux function to unify state names (lower case, no spaces etc.)
#   mutate(end_port_sa_gom = case_when(
#     # if a name is in our SA list - "sa", otherwise - "gom"
#     fix_names(end_port_state) %in% fix_names(sa_state_abb$state_abb) ~ "sa",
#     .default = "gom"
#   )) %>%
#   # go through the new column again
#   # if an end port state is Florida - use the region from the previous step (column "end_port_fl_reg")
#   # otherwise don't change
#   mutate(end_port_sa_gom = ifelse(
#     tolower(end_port_state) == "fl",
#     end_port_fl_reg,
#     end_port_sa_gom
#   )) %>%
#   # remove this column, we don't need it anymore
#   select(-end_port_fl_reg)

# #### test: check new cols of states and regions ----
# fhier_logbooks_content_waves__sa_gom %>%
#   # look at states and regions
#   select(end_port_state, end_port_sa_gom) %>%
#   unique() %>%
#   glimpse()

## r get Shapefile all waters ----
path_to_federal_state_w <-
  file.path(my_paths$inputs,
            r"(shapefiles\federal_and_state_waters\FederalAndStateWaters.shp)")

file.exists(path_to_federal_state_w)
# T

tic("federal_state_w_sf")
federal_state_w_sf <-
  sf::read_sf(path_to_federal_state_w)
toc()

# rr <-
# federal_state_w_sf |>
#   sf::st_drop_geometry()
#
# rr$Jurisdicti |>
#   cat(sep = ", ")

east_coat_states <- c(
  gom = c("Florida",
          "Texas",
          "Louisiana"),
  sa = c(
  "Alabama",
  "Connecticut",
  "Delaware",
  "Florida",
  "Georgia",
  "Maine",
  "Maryland",
  "Massachusetts",
  "Mississippi",
  "New Hampshire",
  "New Jersey",
  "New York",
  "North Carolina",
  "Pennsylvania",
  "Rhode Island",
  "South Carolina",
  "Virginia",
  "Washington DC")
)
# nc_sql = sf::st_read(system.file("shape/nc.shp", package="sf"),
#                      query = "SELECT NAME, SID74, FIPS FROM \"nc\" WHERE BIR74 > 20000")

federal_state_w_sf_east <-
  federal_state_w_sf |>
  filter(Jurisdicti %in% east_coat_states)

# mapview(sa_shp)
# [1] 21  7

# read in GOM shp ----
GOMsf <-
  sf::read_sf(r"(GOM_heatmap_from Kyle\GOM_400fm\GOM_400fm.shp)") %>%
  group_by(StatZone) %>%
  summarise()
# Bounding box:  xmin: -97.7445 ymin: 23.82277 xmax: -80.37073 ymax: 30.885
# Geodetic CRS:  WGS 84

# str(GOMsf)

# create 5x5 minute grid ----
min_grid <-
  function(my_sf = GOMsf, minute_num = 1) {
    grid <-
      sf::st_make_grid(x = sf::st_bbox(GOMsf),
                       cellsize = 1 / 60 * minute_num) %>%
      sf::st_as_sf() %>%
      mutate(cell_id = 1:nrow(.))

    return(grid)
  }

grid_gom5 <- min_grid(GOMsf, 5)
grid_sa5 <- min_grid(sa_shp, 5)

sf::st_agr(GOMsf) =
  sf::st_agr(sa_shp) =
  sf::st_agr(grid_gom5) =
  sf::st_agr(grid_sa5) =
  "constant"

# create SA 5x5 minute grid ----
min_grid_sa_shp <-
  function(minute_num = 1) {
    grid <-
      sf::st_make_grid(x = sf::st_bbox(sa_shp),
                       cellsize = 1 / 60 * minute_num) %>%
      sf::st_as_sf() %>%
      mutate(cell_id = 1:nrow(.))

    return(grid)
  }

grid_sa <- min_grid_sa_shp(5)

sf::st_agr(sa_shp) = sf::st_agr(grid_sa) = "constant"


### remove internal boundaries from the GOM shape file ----

tic("st_union(GOMsf)")
st_union_GOMsf <- sf::st_union(GOMsf)
toc()
# st_union(GOMsf): 21.59 sec elapsed

# str(GOMsf)
# sf [21 × 2] (S3: sf/tbl_df/tbl/data.frame)
#  $ StatZone: num [1:21] 1 2 3 4 5 6 7 8 9 10 ...
#  $ geometry:sfc_GEOMETRY of length 21; first list element: List of 6

# str(st_union_GOMsf)
# sfc_MULTIPOLYGON of length 1; first list element: List of 15
#  $ :List of 21234

## by n min grid ----
# my_crs <- sf::st_crs(GOMsf)
df_join_grid <-
  function(my_df, grid, my_crs) {
    my_df |>
      # join n minute grid
      sf::st_as_sf(
        coords = c("LONGITUDE", "LATITUDE"),
        crs = my_crs) |>
        # ,
        # remove = FALSE) %>%
        sf::st_join(grid, join = sf::st_nearest_feature) %>%
          return()
        }

crop_by_shape <-
  function(my_sf, my_shp = GOMsf) {
    my_sf |>
      sf::st_join(my_shp, left = FALSE) %>%
      dplyr::mutate(LONGITUDE = sf::st_coordinates(.)[, 1],
             LATITUDE = sf::st_coordinates(.)[, 2]) %>%
      return()
  }

## count trip ids and vessels by grid cell ----
add_vsl_and_trip_cnts <-
  function(my_df, vessel_id_name = "VESSEL_OFFICIAL_NBR") {
    my_df |>
      group_by(cell_id) |>
      mutate(vsl_cnt = n_distinct(!!sym(vessel_id_name)),
             trip_id_cnt = n_distinct(TRIP_ID)) |>
      ungroup() %>%
      return()
  }

## make a plot ----
make_map_trips <-
  function(map_trip_base_data,
           shape_data,
           total_trips_title,
           trip_cnt_name,
           caption_text = "Heat map of SEFHIER trips (5 min. resolution).",
           unit_num = 1,
           print_stat_zone = NULL
           ) {
    # browser()
    max_num <- max(map_trip_base_data[[trip_cnt_name]])
    map_trips <-
      ggplot() +
      geom_sf(data = map_trip_base_data,
              aes(geometry = x,
                  fill = !!sym(trip_cnt_name)),
              colour = NA) +
      geom_sf(data = shape_data, fill = NA)

    # test for an optional argument
    if (!missing(print_stat_zone)) {
      map_trips <-
        map_trips +
        geom_sf_text(data = shape_data,
                     aes(geometry = geometry,
                         label = StatZone),
                     size = 3.5)
    }

    map_trips <-
        map_trips +
      labs(
        x = "",
        y = "",
        fill = "",
        caption = caption_text
      ) +
      theme_bw() +
      scale_fill_gradient2(
        name = total_trips_title,
        labels = scales::comma,
        low = "red",
        mid = "purple",
        high = "blue",
        # trans = "log2",
        trans = "log1p",
        limits = c(1, max_num)
        # ,
        # oob = scales::oob_keep
      ) +
      theme(
        legend.position = "top",
        legend.justification = "left",
        legend.key.width = unit(unit_num, "npc"),
        legend.title = element_text(size =
                                      text_sizes[["legend_title_text_size"]]),
        legend.text = element_text(size =
                                     text_sizes[["legend_text_text_size"]]), # for charter heatmap use 7
        plot.caption = element_text(hjust = 0,
                                    size = text_sizes[["plot_caption_text_size"]]),
    axis.text.x =
      element_text(size = text_sizes[["axis_text_x_size"]]),
axis.text.y =
      element_text(size = text_sizes[["axis_text_y_size"]])
      ) +
      guides(fill = guide_colourbar(title.position = "top"))

    return(map_trips)
  }
