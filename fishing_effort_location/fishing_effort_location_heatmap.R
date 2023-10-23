source("~/R_code_github/useful_functions_module.r")
my_paths <- set_work_dir()

# data are from "by_permit"

source(
  file.path(
    my_paths$git_r,
    r"(fishing_effort_location\fishing_effort_location_by_permit.R)"
  )
)

library(ggplot2) # a visualization package
library(ggmap) # extends 'ggplot2' for creating maps and working with spatial data.

# Heatmap ----

source(
  file.path(
    my_paths$git_r,
    r"(fishing_effort_location\prepare_gom_heatmap_func.R)"
  )
)

## heatmap data ----

# for_heatmap_lat_lon_trips_only <-
#   coord_data_2022_short_good_sf_crop_big_df_in_metricks_list$gom_and_dual |>
#   select(TRIP_ID, LATITUDE, LONGITUDE) |>
#   distinct()

# glimpse(for_heatmap_lat_lon_trips_only)
# Rows: 41,455

# gom
for_heatmap_lat_lon_trips_vessels_gom_only <-
  coord_data_2022_short_good_sf_crop_big_df_in_metricks_list$gom_and_dual |>
  select(TRIP_ID, VESSEL_OFFICIAL_NBR, LATITUDE, LONGITUDE) |>
  distinct()

dim(for_heatmap_lat_lon_trips_vessels_gom_only)
# Rows: 41,455
# [1] 46763     4 mv

# sa
for_heatmap_lat_lon_trips_vessels_sa_only <-
  coord_data_2022_short_good_sf_crop_big_df_in_metricks_list$sa_only |>
  select(TRIP_ID, VESSEL_OFFICIAL_NBR, LATITUDE, LONGITUDE) |>
  distinct()

dim(for_heatmap_lat_lon_trips_vessels_sa_only)
# [1] 68122     4
# [1] 44060     4

### remove vessels not in Jeannette's SA list ----

# Build the path to the R script 'vessel_permit_corrected_list.R' by
# combining the base path 'my_paths$git_r' and the script name.
script_path <-
  file.path(my_paths$git_r,
            "vessel_permit_list/vessel_permit_corrected_list.R")

# Source (run) the R script using the constructed script path.
source(script_path)

# Rows are filtered to exclude vessels whose 'VESSEL_OFFICIAL_NBR' is in the
# 'vessels_to_remove_from_ours' vector.
for_heatmap_lat_lon_trips_vessels_sa_only_rm <-
  for_heatmap_lat_lon_trips_vessels_sa_only |>
  filter(!VESSEL_OFFICIAL_NBR %in% vessels_to_remove_from_ours)

dim(for_heatmap_lat_lon_trips_vessels_sa_only_rm)
# [1] 67983     4

## add the grid ----
# assuming data is dataframe with variables LATITUDE, LONGITUDE, and trips

# tic("effort")
# effort <- df_join_grid(for_heatmap_lat_lon_trips_only)
# toc()
# effort: 0.75 sec elapsed

tic("effort_vsl_gom")
# Create a new object 'my_crs' by extracting the coordinate reference system (CRS)
# from a spatial object 'GOMsf'.
my_crs <- sf::st_crs(GOMsf)
# Create a new data frame 'effort_vsl_gom' by joining the data frames
# 'for_heatmap_lat_lon_trips_vessels_gom_only' and 'grid_gom5' based on a common
# spatial reference system defined by 'my_crs'.
effort_vsl_gom <-
  df_join_grid(for_heatmap_lat_lon_trips_vessels_gom_only,
               grid_gom5,
               my_crs)
toc()
# effort_vsl: 0.62 sec elapsed

# class(effort_vsl)

tic("effort_vsl_sa")
# Create a new object 'my_crs_sa' by extracting the coordinate reference system (CRS)
# from a spatial object 'sa_shp'.
my_crs_sa <- sf::st_crs(sa_shp)
# sf::st_geometry(grid_sa5)
# Geodetic CRS:  NAD83
# sf::st_geometry(sa_shp)
# Geodetic CRS:  NAD83

# Create a new data frame 'effort_vsl_sa' by joining the data frames
# 'for_heatmap_lat_lon_trips_vessels_sa_only_rm' and 'grid_sa5' based on a
# spatial reference system defined by 'my_crs_sa'.
effort_vsl_sa <-
  df_join_grid(for_heatmap_lat_lon_trips_vessels_sa_only_rm,
               grid_sa5,
               my_crs = my_crs_sa)
toc()
# effort_vsl_sa: 1.22 sec elapsed

## crop by the shape ----
tic("effort_vsl_cropped_gom")
effort_vsl_cropped_gom <- crop_by_shape(effort_vsl_gom)
toc()
# effort_cropped2: 0.44 sec elapsed

dim(effort_vsl_cropped_gom)
# [1] 35822     7
# [1] 40604     7 mv

tic("effort_vsl_cropped_sa")
effort_vsl_cropped_sa <- crop_by_shape(effort_vsl_sa, sa_shp)
toc()
# effort_vsl_cropped_sa: 0.54 sec elapsed

dim(effort_vsl_cropped_sa)
# [1] 21461     8
# [1] 20147     8 mv

## count trip ids and vessels by grid cell ----

# Create a list 'effort_vsl_cropped_cnt_l' by applying 'add_vsl_and_trip_cnts' function to data frames.

effort_vsl_cropped_cnt_l <-
  list(effort_vsl_cropped_gom, effort_vsl_cropped_sa) |>

  # Use the 'map' function to apply a function to each element in the list.
  purrr::map(function(effort_vsl_cropped) {

    # Apply the 'add_vsl_and_trip_cnts' function to each 'effort_vsl_cropped' data frame.
    add_vsl_and_trip_cnts(effort_vsl_cropped)
  })

map(effort_vsl_cropped_cnt_l, dim)
# [[1]]
# [1] 35822     9
#
# [[2]]
# [1] 21461    10
# mv data:
# [[1]]
# [1] 40604     9
#
# [[2]]
# [1] 20147    10

# check
# effort_vsl_cropped_cnt_l[[1]] |>
#   sf::st_drop_geometry() |>
#   filter(cell_id == 1864) |>
#   select(vsl_cnt, trip_id_cnt) |>
#   distinct() |>
#   glimpse()
# vsl_cnt     <int> 11
# trip_id_cnt <int> 236

# class(effort_vsl_cropped_cnt2)

### no rule3 ----
# Create a list 'effort_cropped_short_cnt2_short_l' by applying a set of operations to data frames.

effort_cropped_short_cnt2_short_l <-
  effort_vsl_cropped_cnt_l |>

  # Use the 'map' function to apply a function to each element in the list.
  purrr::map(function(effort_vsl_cropped_cnt) {

    # Use the 'select' function to remove specific columns,
    # 'LATITUDE', 'LONGITUDE', 'TRIP_ID', and 'VESSEL_OFFICIAL_NBR', from each data frame.
    effort_vsl_cropped_cnt |>
      select(-c(LATITUDE, LONGITUDE, TRIP_ID, VESSEL_OFFICIAL_NBR))
  })

# map(effort_cropped_short_cnt2_short_l, dim)

### not used, with rule 3 ----
# effort_cropped_short_cnt_rule3_short <-
#   effort_cropped_short_cnt_rule3 |>
#   select(-c(LATITUDE, LONGITUDE, TRIP_ID, VESSEL_OFFICIAL_NBR))
#
# dim(effort_cropped_short_cnt_rule3_short)
# # [1] 31981     5
#
## join with min grid ----
### not used, rule 3 ----
# heat.plt_rule3 <-
#   effort_cropped_short_cnt_rule3_short |>
#   # have to use data.frame, to avoid
#   # Error: y should not have class sf; for spatial joins, use st_join
#   inner_join(data.frame(grid))
# # Joining with `by = join_by(cell_id)`
#
# # mapview(grid)
# dim(heat.plt_rule3)
# # [1] 35828     6
# # [1] 33883     6 (rule3)
# # [1] 31981     6
#
### no rule 3 ----
heat.plt_gom <-
  # Extract the first element from the list.
  # Use the pipe operator to pass it to the next operation.
  effort_cropped_short_cnt2_short_l[[1]] |>
  # Perform an inner join with the data frame 'grid_gom5'
  # using a common column specified by 'join_by(cell_id)'.
  # Store the result in the variable 'heat.plt_gom'.
  # have to use data.frame, to avoid:
  # Error: y should not have class sf; for spatial joins, use st_join
  inner_join(data.frame(grid_gom5))

# the same for SA
heat.plt_sa <-
  effort_cropped_short_cnt2_short_l[[2]] |>
  # have to use data.frame, to avoid
  # Error: y should not have class sf; for spatial joins, use st_join
  inner_join(data.frame(grid_sa5))
# Joining with `by = join_by(cell_id)`

## make a plot ----

max_num3_gom <- max(heat.plt_gom$trip_id_cnt)
# 1209
# 1317 mv

max_num3_sa <- max(heat.plt_sa$trip_id_cnt)
# 590
# 561 mv

# map_trips_rule_3 <-
#   make_map_trips(heat.plt,
#            st_union_GOMsf,
#            "total trips",
#            max_num = max_num3,
#            caption_text = "Heat map of SEFHIER trips (5 min. resolution). 2022. GoM permitted vessels. Only squares with more than 3 reporting vessels are shown. ")

# map_trips_no_rule_3
# print_df_names(heat.plt_gom)
map_trips_no_rule_3_gom <-
  make_map_trips(heat.plt_gom,
           st_union_GOMsf,
           "total trips",
           trip_cnt_name = "trip_id_cnt",
           unit_num = 1.2)

map_trips_no_rule_3_gom


map_trips_no_rule_3_sa <-
  make_map_trips(heat.plt_sa,
           sa_shp,
           "total trips",
           trip_cnt_name = "trip_id_cnt",
           unit_num = 0.9,
           legend_text_text_size = 8)

map_trips_no_rule_3_sa +
  geom_sf(data = sa_s_shp) +
  geom_sf_text(data = sa_s_shp,
               label = sa_s_shp$NAME,
               size = 3)

# make_map_trips <-
#   function(map_trip_base_data,
#            shape_data,

#            total_trips_title,
#            trip_cnt_name,
#            caption_text = "Heat map of SEFHIER trips (5 min. resolution).",
#            unit_num = 1,
#            print_stat_zone = NULL
#            ) {

source(
  file.path(
    my_paths$git_r,
    r"(fishing_effort_location\fishing_effort_location_by_permit_and_end_port.R)"
  )
)

## by zone ----

### join data and gomsf ----
tic("for_heatmap_lat_lon_trips_vessels_only_join_gomsf")
for_heatmap_lat_lon_trips_vessels_only_join_gomsf <-
  for_heatmap_lat_lon_trips_vessels_gom_only |>
        st_as_sf(
        coords = c("LONGITUDE", "LATITUDE"),
        crs = st_crs(GOMsf)) |>
        # ,
        # remove = FALSE) %>%
        st_join(GOMsf, join = st_nearest_feature)
toc()
# for_heatmap_lat_lon_trips_vessels_only_join_gomsf: 75.5 sec elapsed

# all points for GOM permitted
# mapview(for_heatmap_lat_lon_trips_vessels_only_join_gomsf)
# [1] 41455     4

tic("for_heatmap_lat_lon_trips_vessels_only_inters_gomsf")
for_heatmap_lat_lon_trips_vessels_only_inters_gomsf <-
  for_heatmap_lat_lon_trips_vessels_gom_only |>
        st_as_sf(
        coords = c("LONGITUDE", "LATITUDE"),
        crs = st_crs(GOMsf)) |>
  st_join(GOMsf, left = FALSE) %>%
  mutate(LONGITUDE = st_coordinates(.)[, 1],
         LATITUDE = st_coordinates(.)[, 2])
toc()
# for_heatmap_lat_lon_trips_vessels_only_join_gomsf_1: 0.5 sec elapsed

# only points in GOM
dim(for_heatmap_lat_lon_trips_vessels_only_inters_gomsf)
# [1] 35822     6
# [1] "TRIP_ID, VESSEL_OFFICIAL_NBR, geometry, StatZone, LONGITUDE, LATITUDE"

### count trip ids and vessels by statZone ----
for_heatmap_lat_lon_trips_vessels_only_inters_gomsf_cnt <-
  for_heatmap_lat_lon_trips_vessels_only_inters_gomsf |>
  # sf::st_drop_geometry() |>
  group_by(StatZone) |>
  mutate(vsl_cnt_stat_zone = n_distinct(VESSEL_OFFICIAL_NBR),
         trip_id_cnt_stat_zone = n_distinct(TRIP_ID)) |>
  ungroup()

View(for_heatmap_lat_lon_trips_vessels_only_inters_gomsf_cnt)

# for_heatmap_lat_lon_trips_vessels_only_inters_gomsf_cnt |>
#   filter(vsl_cnt_stat_zone < 3) |>
#   write_csv("state_zone_less_2_vsl.csv")

# check
for_heatmap_lat_lon_trips_vessels_only_inters_gomsf_cnt |>
    sf::st_drop_geometry() |>
    select(vsl_cnt_stat_zone) |>
    distinct() |>
    arrange(vsl_cnt_stat_zone)

### remove extra columns ----
for_heatmap_lat_lon_trips_vessels_only_inters_gomsf_cnt_short <-
  for_heatmap_lat_lon_trips_vessels_only_inters_gomsf_cnt |>
  select(-c(LATITUDE, LONGITUDE, TRIP_ID, VESSEL_OFFICIAL_NBR)) |>
  distinct() |>
  ungroup()

# View(for_heatmap_lat_lon_trips_vessels_only_inters_gomsf_cnt_short)
# [1] 35822     4

# dots
# for_heatmap_lat_lon_trips_vessels_only_inters_gomsf_cnt_short |>
  # filter(vsl_cnt_stat_zone < 10) |>
  # mapview()

#### only vsl > 3 ----
for_heatmap_lat_lon_trips_vessels_only_inters_gomsf_cnt_short_rule3 <-
  for_heatmap_lat_lon_trips_vessels_only_inters_gomsf_cnt_short |>
  filter(vsl_cnt_stat_zone > 2)

### combine cnts with statzone geometry ----
for_heatmap_lat_lon_trips_vessels_only_inters_gomsf_cnt_short_df <-
  for_heatmap_lat_lon_trips_vessels_only_inters_gomsf_cnt_short |>
  sf::st_drop_geometry() |>
  distinct()
# 21

# class(for_heatmap_lat_lon_trips_vessels_only_inters_gomsf_cnt_short_df)

gomshp_zone_cnt <-
left_join(GOMsf,
          for_heatmap_lat_lon_trips_vessels_only_inters_gomsf_cnt_short_df,
         join_by("StatZone"))

dim(gomshp_zone_cnt)
# [1] 21  4

shape_data <- gomshp_zone_cnt


### plot by zone ----
# ggplot(map.df,aes(x=long,y=lat,group=group))+
#   geom_polygon(aes(fill=delta),color="grey20")+

# glimpse(gomshp_zone_cnt)
# mapview(for_heatmap_lat_lon_trips_vessels_only_inters_gomsf_cnt_short)

plot_zone_cnt <-
    function(shape_data,
           total_cnt_title,
           cnts_col_name,
           caption_text) {
      # browser()
      max_num <-
        max(shape_data[[cnts_col_name]])
      # max(gomshp_zone_cnt$vsl_cnt_stat_zone)
      # 148
      min_num <-
        min(shape_data[[cnts_col_name]])
      # 2

      map_vsl_zone <-
        ggplot() +
        geom_sf(data = shape_data,
                aes(geometry = geometry,
                    #                   fill = !!sym(trip_cnt_name)),
                    fill = !!sym(cnts_col_name)),
                colour = NA) +
        geom_sf_text(data = shape_data,
                     aes(geometry = geometry,
                         label = StatZone),
                     size = 3.5) +
        labs(
          x = "",
          y = "",
          fill = "",
          caption = caption_text
        ) +
        theme_bw() +
        scale_fill_gradient2(
          name = total_cnt_title,
          labels = scales::comma,
          low = "red",
          mid = "purple",
          high = "blue",
          # trans = "log2",
          trans = "log1p",
          limits = c(min_num, max_num)
          # ,
          # oob = scales::oob_keep
        ) +
        theme(
          legend.position = "top",
          legend.justification = "left",
          legend.key.width = unit(0.9, "npc"),
          # legend.key.width = unit(3, "cm"),
          plot.caption = element_text(hjust = 0)
        ) +
        guides(fill = guide_colourbar(title.position = "top"))
    }

# shape_data = gomshp_zone_cnt
# total_cnt_title = "total reporting vessels"
# cnts_col_name = "vsl_cnt_stat_zone"
# caption_text = "Heat map of SEFHIER trips. 2022. GoM permitted vessels."

map_vsls_stat_zone <-
  plot_zone_cnt(
    gomshp_zone_cnt,
    "total reporting vessels",
    "vsl_cnt_stat_zone",
    caption_text = "Heat map of SEFHIER reporting vessels. 2022. GoM permitted only."
  )

# map_vsls_stat_zone

map_trips_stat_zone <-
  plot_zone_cnt(gomshp_zone_cnt,
                "total trips",
                "trip_id_cnt_stat_zone",
                caption_text = "Heat map of SEFHIER trips by stat zones. 2022. GoM permitted vessels only.")

# map_trips_stat_zone
#

# Repeat separately for charter and headboat ----

# Thought for exploration and not the Council meeting coming up - can we show this just for charter and the just for headboat trips?  Headboat being that they selected that in the logbook.

## get trip_type data ----

my_vessels_trips <-
  coord_data_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list$gom_and_dual |>
  select(VESSEL_OFFICIAL_NBR,
         TRIP_ID) |>
  distinct()

str(my_vessels_trips)
# chr [1:626] "FL9312NA" "FL6074MJ" "FL4038KN" "FL0957RW" "FL4521RU" "994360" ...


### create a db query with chunks, otherwise Oracle error ----

my_vessels_ids_u <- unique(my_vessels_trips$VESSEL_OFFICIAL_NBR)

full_length <- length(my_vessels_ids_u)
# 626

max_chunk_num <- 3

# repeat max_chunk_num times
all_ch <-
  lapply(1:max_chunk_num, function(i) {
    # count the current start and end
    one_chunk_length <- ceiling(full_length / max_chunk_num)
    current_end <- one_chunk_length * i
    current_start <- current_end - one_chunk_length

    # pull the chunk from start to end
    my_vessels_ids_u[current_start:current_end] |>
      # and paste in into a comma separated string
      paste0(collapse = "', '")
  })

tail(all_ch[[3]])
tail(my_vessels_ids_u)
# str(all_ch)
# tibble [626 × 1] (S3: tbl_df/tbl/data.frame)

                 # paste0(collapse = "', '")

vsl_id_q_part <-
  all_ch |>
  map(\(one_chunk) str_glue("vessel_official_nbr in ('{one_chunk}')"))

collect_parts <-
  paste(vsl_id_q_part, collapse = ' OR ')

# cat(collect_parts)

get_trip_type_data_from_db <- function() {
  # browser()
  con = dbConnect(
    dbDriver("Oracle"),
    username = keyring::key_list("SECPR")[1, 2],
    password = keyring::key_get("SECPR", keyring::key_list("SECPR")[1, 2]),
    dbname = "SECPR"
  )

  "
DECLARE
  str varchar2(32767);
BEGIN
  str := 'Very-very-...-very-very-very-very-very-very long string value';
  update t1 set col1 = str;
END;
/
"

  request_query <-
    paste0(
      "SELECT distinct
      trip_id,
    vessel_official_nbr,
    trip_type_name
FROM
  srh.mv_safis_trip_download@secapxdv_dblk.sfsc.noaa.gov
WHERE
  trip_type in ('H', 'A')
  AND TRIP_START_DATE >= TO_DATE('01-JAN-22', 'dd-mon-yy')
  AND (
  ",
collect_parts,
")"
    )

  # nchar(request_query)
  # [1] 523009

  # cat(request_query)

  db_data = dbGetQuery(con,
                       request_query)

  dbDisconnect(con)

  return(db_data)
}

tic("trip_type_data_from_db")
trip_type_data_from_db <- get_trip_type_data_from_db()
toc()
# trip_type_data_from_db: 9.46 sec elapsed

# data_overview(trip_type_data_from_db)
# TRIP_ID             47702
# VESSEL_OFFICIAL_NBR 618

# glimpse(my_vessels_trips)

glimpse(trip_type_data_from_db)
# Rows: 47,702

## keep only trips we have in our original data ----
trip_type_data_from_db_by_t_id <-
  trip_type_data_from_db |>
  filter(TRIP_ID %in% my_vessels_trips$TRIP_ID) |>
  distinct()

glimpse(trip_type_data_from_db_by_t_id)
# Rows: 39,977

## add trip_type data to the original data ----
trip_type_data_from_db_by_t_id <-
  mutate(trip_type_data_from_db_by_t_id,
       TRIP_ID = as.character(TRIP_ID))

trip_type_data_from_db_by_t_id_types <-
  coord_data_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list$gom_and_dual |>
  left_join(trip_type_data_from_db_by_t_id)
# Joining with `by = join_by(TRIP_ID, VESSEL_OFFICIAL_NBR)`

## separate by trip type ----
trip_type_data_from_db_by_t_id_types_l <-
  trip_type_data_from_db_by_t_id_types |>
  split(as.factor(trip_type_data_from_db_by_t_id_types$TRIP_TYPE_NAME)) |>
  # remove extra columns in each df
  map(\(x)
      x |>
        dplyr::select(TRIP_ID, VESSEL_OFFICIAL_NBR, LATITUDE, LONGITUDE) |>
        distinct())


# glimpse(trip_type_data_from_db_by_t_id_types)
# List of 2
#  $ CHARTER :'data.frame':	39835 obs. of  3 variables:
#  $ HEADBOAT:'data.frame':	142 obs. of  3 variables:

# str(trip_type_data_from_db_by_t_id_types_l)

## create 5 min heatmaps for both trip types ----
# trip_type_data_from_db_by_t_id_types_l

tic("effort_t_type")
effort_t_type <-
  map(trip_type_data_from_db_by_t_id_types_l, df_join_grid)
toc()
# effort_t_type: 0.7 sec elapsed
# dim(effort_t_type)

tic("effort_t_type_cropped")
effort_t_type_cropped <- map(effort_t_type, crop_by_shape)
toc()
# effort_t_type_cropped: 1.04 sec elapsed

str(effort_t_type_cropped)

effort_t_type_cropped_cnt <- map(effort_t_type_cropped, add_vsl_and_trip_cnts)

map_df(effort_t_type_cropped_cnt, dim)
#   CHARTER HEADBOAT
#     <int>    <int>
# 1   34696       13
# 2       9        9

# data_overview(effort_t_type_cropped_cnt$CHARTER)
# cell_id              2785

# View(grid)

### join with min grid ----
effort_t_type_cropped_cnt_join_grid <-
  map(effort_t_type_cropped_cnt,
      \(x)
      # have to use data.frame, to avoid
      # Error: y should not have class sf; for spatial joins, use st_join
      inner_join(x, data.frame(grid),
                 by = join_by(cell_id)
)
      )

# print_df_names(effort_t_type_cropped_cnt_join_grid$CHARTER)
# [1] "TRIP_ID, VESSEL_OFFICIAL_NBR, geometry, cell_id, StatZone, LONGITUDE, LATITUDE, vsl_cnt, trip_id_cnt, x"

# effort_t_type_cropped_cnt_join_grid$CHARTER

map_trips_types <-
  names(effort_t_type_cropped_cnt_join_grid) |>
  map(
    \(charter_headb) make_map_trips(
      effort_t_type_cropped_cnt_join_grid[[charter_headb]],
      shape_data = st_union_GOMsf,
      total_trips_title = "total trips",
      trip_cnt_name = "trip_id_cnt",
      caption_text = str_glue("Heat map of SEFHIER {tolower(charter_headb)} trips (5 min. resolution). 2022. GoM permitted vessels.",
                              unit_num = 0.6)
    )
  )

# map_trips_types[[1]]
