# Load the 'tigris' package to access geographic data.
library(tigris)

# Set the 'tigris_use_cache' option to TRUE. This will enable caching of
# data retrieved from the TIGER/Line Shapefiles service, which can help
# improve data retrieval performance for future sessions.
tigris_use_cache = TRUE

# plot text sizes ----
text_sizes <- list(
  geom_text_size = 7,
  plot_title_text_size = 10,
  axis_title_text_size = 9,
  axis_text_x_size = 13,
  axis_text_y_size = 13,
  plot_caption_text_size = 13,
  legend_title_text_size = 10,
  legend_text_text_size = 9,
  ### common axes for Months ----
  y_left_fontsize = 10
)

# read in sa shp ----
# F2 in RStudio will show the function definition, when the cursor is on the name.
# Read a shapefile (geospatial data) from the specified file path and store it in the 'sa_shp' object.
sa_shp <-
  read_shapefile(r"(shapefiles_sa_eez_off_states\SA_EEZ_off_states.shp)")

# The South Atlantic Council is responsible for the conservation and management of fishery resources in federal waters ranging from 3 to 200 miles off the coasts of North Carolina, South Carolina, Georgia, and east Florida to Key West.

states_sa <- data.frame(
  state_name = c(
    "Florida", # can exclude, if go by county
    "Georgia",
    "North Carolina",
    "South Carolina"
  )
)

# Create a data frame 'state_tbl' containing state abbreviations and state names; 2x50.
# - 'state.abb' provides state abbreviations.
# - 'tolower(state.name)' converts state names to lowercase.
# - The resulting data frame has two columns: 'state_abb' and 'state_name'.
state_tbl <- data.frame(state.abb, tolower(state.name))

# Rename the columns in the 'state_tbl' data frame.
# The first column is named 'state_abb', and the second column is named 'state_name'.
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

#### test: check new cols of states and regions ----
# fhier_logbooks_content_waves__sa_gom %>%
#   # look at states and regions
#   select(end_port_state, end_port_sa_gom) %>%
#   unique() %>%
#   glimpse()

## r get Shapefile all waters ----
path_to_federal_state_w <-
  file.path(
    my_paths$inputs,
    r"(shapefiles\federal_and_state_waters\FederalAndStateWaters.shp)"
  )

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
    "Washington DC"
  )
)
# nc_sql = sf::st_read(system.file("shape/nc.shp", package="sf"),
#                      query = "SELECT NAME, SID74, FIPS FROM \"nc\" WHERE BIR74 > 20000")

# Create a new data frame 'federal_state_w_sf_east' by filtering the existing data frame 'federal_state_w_sf'.
# Rows are retained if the 'Jurisdicti' column matches any of the values in 'east_coat_states'.
federal_state_w_sf_east <-
  federal_state_w_sf |>
  filter(Jurisdicti %in% east_coat_states)

# mapview(sa_shp)
# [1] 21  7

# coast line SA shp ----
# us_bb <-
#   tigris::counties(filter_by = big_bounding_box, progress_bar = FALSE)

# Create a new data frame 'us_s_shp' using the 'tigris' package to obtain U.S. state shapes.
# The 'cb = TRUE' parameter specifies that you want the U.S. state boundaries.
us_s_shp <-
  tigris::states(cb = TRUE, progress_bar = FALSE)

# Rows are retained if the 'NAME' column (state name) matches any of the values in 'states_sa'.
sa_s_shp <-
  us_s_shp |>
  filter(NAME %in% states_sa$state_name)

# sa_s_shp_plot <-
#   ggplot() +
#   geom_sf(data = sa_s_shp)

# sa_counties_shp <- tigris::counties(states_sa, cb = TRUE)

# gg <- ggplot()
# gg <- gg + geom_sf(
#   data = sa_s_shp,
#   color = "black",
#   # fill = "",
#   size = 0.25
# )
# gg

# library(rnaturalearth) #coastline
# library(maps)
# library(mapdata)


atl_shp_file <-
  file.path(my_paths$inputs,
            r"(shapefiles\ATL_SLA\ATL_SLA.shp)")

# Read a shapefile from the specified file path using the 'sf::read_sf' function.
# The resulting spatial data is stored in the 'atl_shp' object.
atl_shp <- sf::read_sf(atl_shp_file)

# Create a plot using 'ggplot2' with the 'atl_shp' spatial data.
# Use 'geom_sf' to display the shapes from 'atl_shp' with no fill (NA, i.e., transparent).
ggplot() +
  geom_sf(data = atl_shp, fill = NA)

# read in GOM shp ----
# Create a file path using 'file.path' by combining elements from 'my_paths' and specifying a shapefile path.
GOM_400fm_path <-
  file.path(my_paths$inputs, r"(..\GOM_heatmap_from Kyle\GOM_400fm\GOM_400fm.shp)")
# file.exists(GOM_400fm_path)
# T

# Read a shapefile from the specified file path using 'sf::read_sf'.
# Then, group the resulting data by 'StatZone' and summarize it.
GOMsf <-
  sf::read_sf(GOM_400fm_path) %>%
  group_by(StatZone) %>%
  summarise()

# Bounding box:  xmin: -97.7445 ymin: 23.82277 xmax: -80.37073 ymax: 30.885
# Geodetic CRS:  WGS 84

# str(GOMsf)

# create 5x5 minute grid ----
# Define a function 'min_grid' that creates a grid of cells within the bounding box of a given spatial data frame.
# - 'my_sf' is the input spatial data frame (default is 'GOMsf').
# - 'minute_num' specifies the grid cell size in minutes.

min_grid <- function(my_sf = GOMsf, minute_num = 1) {
  # Create a grid of cells using 'sf::st_make_grid' within the bounding box of 'my_sf'.
  grid <-
    sf::st_make_grid(x = sf::st_bbox(my_sf),
                     cellsize = 1 / 60 * minute_num) %>%

    # Convert the grid to a spatial data frame using 'sf::st_as_sf'.
    sf::st_as_sf() %>%

    # Add a 'cell_id' column to the grid using 'mutate'.
    mutate(cell_id = 1:nrow(.))

  # Return the created grid.
  return(grid)
}

grid_gom5 <- min_grid(GOMsf, 5)
grid_sa5 <- min_grid(sa_shp, 5)

# Set the aggregate attribute to "constant" for multiple spatial objects.
sf::st_agr(GOMsf) =
  sf::st_agr(sa_shp) =
  sf::st_agr(grid_gom5) =
  sf::st_agr(grid_sa5) =
  "constant"

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

## Trips by n min grid ----
# Define a function 'df_join_grid' that joins a data frame with a grid using specified coordinates and CRS.

df_join_grid <- function(my_df, grid, my_crs) {
  # Convert 'my_df' to a spatial data frame with specified coordinates and CRS using 'sf::st_as_sf'.
  my_df_grid <-
    my_df |>
    sf::st_as_sf(
      coords = c("longitude", "latitude"),
      crs = my_crs) |>

  # Join the resulting spatial data frame with the 'grid' using the nearest feature join.
  sf::st_join(grid, join = sf::st_nearest_feature)

  # Return the joined data frame.
  return(my_df_grid)
}

# Define a function 'crop_by_shape' that crops a spatial object using another spatial object.
# - 'my_sf' is the input spatial object to be cropped.
# - 'my_shp' is the spatial object used for cropping (default is 'GOMsf').

crop_by_shape <- function(my_sf, my_shp = GOMsf) {
  # Join 'my_sf' with 'my_shp' to crop it, leaving only the intersecting geometries.
  my_sf |>
    sf::st_join(my_shp, left = FALSE) %>%

  # extract the longitude and latitude coordinates from the joined spatial object.
  dplyr::mutate(longitude = sf::st_coordinates(.)[, 1],
         latitude = sf::st_coordinates(.)[, 2]) %>%

  # Return the cropped and transformed spatial object.
  return()
}

## count trip ids and vessels by grid cell function ----
# Define a function 'add_vsl_and_trip_cnts' that adds vessel and trip counts to a data frame.
# - 'my_df' is the input data frame.
# - 'vessel_id_name' is the name of the column containing vessel IDs (default is "vessel_official_nbr").

add_vsl_and_trip_cnts <- function(my_df, vessel_id_name = "vessel_official_nbr") {
  # Group the data frame by 'cell_id'.
  my_df |>
    group_by(cell_id) |>

  # Add columns 'vsl_cnt' and 'trip_id_cnt' with counts of distinct vessel and trip IDs.
    # sym() take strings as input and turn them into symbols.
    # The !! (bang-bang or unquote) operator is used to unquote the symbol, allowing it to be used in dplyr verbs like mutate, select, or other functions that accept column names.
    # So, the code !!rlang::sym(vessel_id_name) effectively evaluates to the column name specified by the vessel_id_name variable in the context of a dplyr verb, allowing you to work with the column dynamically based on the variable's value.

    dplyr::mutate(
      vsl_cnt =
        dplyr::n_distinct(!!rlang::sym(vessel_id_name)),
      trip_id_cnt =
        dplyr::n_distinct(trip_id)
    ) |>

  # Ungroup the data frame to remove grouping and return the result.
  dplyr::ungroup() %>%

  # Return the modified data frame.
  return()
}

## make a plot function ----
# Define a function 'make_map_trips' to create a ggplot2 heatmap of trip data.
# - 'map_trip_base_data' is the data containing trip information to be mapped.
# - 'shape_data' is the shape data used as a backdrop for mapping.
# - 'total_trips_title' is the title for the total trips legend.
# - 'trip_cnt_name' is the name of the column with trip counts.
# - 'caption_text' is the caption for the plot.
# - 'unit_num' specifies the unit size for the legend.
# - 'print_stat_zone' is an optional argument to include StatZone labels.
make_map_trips <-
  function(map_trip_base_data,
           shape_data,
           total_trips_title,
           trip_cnt_name,
           caption_text = "Heat map of SEFHIER trips (5 min. resolution).",
           unit_num = 1,
           print_stat_zone = NULL,
           legend_text_text_size = text_sizes[["legend_text_text_size"]]
           ) {
    # Calculate the maximum number of trips for legend scaling.
    max_num <- max(map_trip_base_data[[trip_cnt_name]])

    # Create a ggplot2 plot 'map_trips'.
    map_trips <-
      ggplot() +
      # Add a filled heatmap using 'geom_sf'.
      geom_sf(data = map_trip_base_data,
              aes(geometry = x,
                  fill = !!sym(trip_cnt_name)),
              colour = NA) +
      # Add the shape data with no fill.
      geom_sf(data = shape_data, fill = NA)

    # Check for an optional argument 'print_stat_zone'.
    if (!missing(print_stat_zone)) {
      map_trips <-
        map_trips +
        # Add StatZone labels using 'geom_sf_text'.
        geom_sf_text(data = shape_data,
                     aes(geometry = geometry,
                         label = StatZone),
                     size = 3.5)
    }

    map_trips <-
        map_trips +
      # Set plot labels and theme settings.
      labs(
        x = "",
        y = "",
        fill = "",
        caption = caption_text
      ) +
      # theme_bw() +
      scale_fill_viridis(
        name = total_trips_title,
        labels = scales::comma,
        trans = "log1p",
        limits = c(1, max_num)
      ) +
      # Set fill scale properties.
      # scale_fill_gradient(
      # scale_fill_gradient2(
      # # scale_fill_gradientn(
      # #   colours = heat.colors(100),
      #   # trans = 'reverse',
      #   name = total_trips_title,
      #   labels = scales::comma,
      #   low = "yellow",
      #   mid = "yellow",
      #   high = "red",
      #
      #   # low = "red",
      #   # mid = "purple",
      #   # high = "blue",
      #
      #   # trans = "log2",
      #   trans = "log1p",
      #   limits = c(1, max_num)
      #   # ,
      #   # oob = scales::oob_keep
      # ) +
      theme(
        legend.position = "top",
        legend.justification = "left",
        legend.key.width = unit(unit_num, "npc"),
        legend.title = element_text(size =
                                      text_sizes[["legend_title_text_size"]]),
        legend.text = element_text(size =
                                     legend_text_text_size), # for charter heatmap use 7
        plot.caption = element_text(hjust = 0,
                                    size = text_sizes[["plot_caption_text_size"]]),
    axis.text.x =
      element_text(size = text_sizes[["axis_text_x_size"]]),
axis.text.y =
      element_text(size = text_sizes[["axis_text_y_size"]])
      ) +
      # Add a legend guide for fill color.
      guides(fill = guide_colourbar(title.position = "top"))

    # Return the created 'map_trips' plot.
    return(map_trips)
  }
