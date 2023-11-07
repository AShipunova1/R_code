## by zone ----

### join data and gomsf ----
tic("for_heatmap_lat_lon_trips_vessels_only_join_gomsf")
for_heatmap_lat_lon_trips_vessels_only_join_gomsf <-
  for_heatmap_lat_lon_trips_vessels_gom_only |>
        st_as_sf(
        coords = c("longitude", "latitude"),
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
        coords = c("longitude", "latitude"),
        crs = st_crs(GOMsf)) |>
  st_join(GOMsf, left = FALSE) %>%
  dplyr::mutate(longitude = st_coordinates(.)[, 1],
         latitude = st_coordinates(.)[, 2])
toc()
# for_heatmap_lat_lon_trips_vessels_only_join_gomsf_1: 0.5 sec elapsed

# only points in GOM
dim(for_heatmap_lat_lon_trips_vessels_only_inters_gomsf)
# [1] 35822     6
# [1] "trip_id, vessel_official_nbr, geometry, StatZone, longitude, latitude"

### count trip ids and vessels by statZone ----
for_heatmap_lat_lon_trips_vessels_only_inters_gomsf_cnt <-
  for_heatmap_lat_lon_trips_vessels_only_inters_gomsf |>
  # sf::st_drop_geometry() |>
  group_by(StatZone) |>
  dplyr::mutate(vsl_cnt_stat_zone = n_distinct(vessel_official_nbr),
         trip_id_cnt_stat_zone = n_distinct(trip_id)) |>
  ungroup()

View(for_heatmap_lat_lon_trips_vessels_only_inters_gomsf_cnt)

# for_heatmap_lat_lon_trips_vessels_only_inters_gomsf_cnt |>
#   filter(vsl_cnt_stat_zone < 3) |>
#   write_csv("state_zone_less_2_vsl.csv")

# check
for_heatmap_lat_lon_trips_vessels_only_inters_gomsf_cnt |>
    sf::st_drop_geometry() |>
    dplyr::select(vsl_cnt_stat_zone) |>
    distinct() |>
    arrange(vsl_cnt_stat_zone)

### remove extra columns ----
for_heatmap_lat_lon_trips_vessels_only_inters_gomsf_cnt_short <-
  for_heatmap_lat_lon_trips_vessels_only_inters_gomsf_cnt |>
  dplyr::select(-c(latitude, longitude, trip_id, vessel_official_nbr)) |>
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

# Thought for exploration and not the Council meeting coming up - can we show this just for charter and the just for headboat trips?  Headboat being that they dplyr::selected that in the logbook.

## get trip_type data ----

my_vessels_trips <-
  coord_data_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list$gom_and_dual |>
  dplyr::select(vessel_official_nbr,
         trip_id) |>
  distinct()

str(my_vessels_trips)
# chr [1:626] "FL9312NA" "FL6074MJ" "FL4038KN" "FL0957RW" "FL4521RU" "994360" ...


### create a db query with chunks, otherwise Oracle error ----

my_vessels_ids_u <- unique(my_vessels_trips$vessel_official_nbr)

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
  filter(trip_id %in% my_vessels_trips$trip_id) |>
  distinct()

glimpse(trip_type_data_from_db_by_t_id)
# Rows: 39,977

## add trip_type data to the original data ----
trip_type_data_from_db_by_t_id <-
  dplyr::mutate(trip_type_data_from_db_by_t_id,
       trip_id = as.character(trip_id))

trip_type_data_from_db_by_t_id_types <-
  coord_data_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list$gom_and_dual |>
  left_join(trip_type_data_from_db_by_t_id)
# joining with `by = join_by(trip_id, vessel_official_nbr)`

## separate by trip type ----
trip_type_data_from_db_by_t_id_types_l <-
  trip_type_data_from_db_by_t_id_types |>
  split(as.factor(trip_type_data_from_db_by_t_id_types$trip_type_name)) |>
  # remove extra columns in each df
  map(\(x)
      x |>
        dplyr::select(trip_id, vessel_official_nbr, latitude, longitude) |>
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
