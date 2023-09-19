# data are from "by_permit"
library(ggplot2)
library(ggmap)
# library(RColorBrewer)

# Heatmap ----

# read in GOM trip ticket grid
GOMsf = read_sf(r"(GOM_heatmap_from Kyle\GOM_400fm\GOM_400fm.shp)") %>%
  group_by(StatZone) %>% summarise()
# Bounding box:  xmin: -97.7445 ymin: 23.82277 xmax: -80.37073 ymax: 30.885
# Geodetic CRS:  WGS 84

# str(GOMsf)

# create GOM 5x5 minute grid
min_grid <-
  function(minute_num = 1) {
    grid <-
      sf::st_make_grid(x = st_bbox(GOMsf), cellsize = 1 / 60 * minute_num) %>%
      st_as_sf() %>%
      mutate(cell_id = 1:nrow(.))

    return(grid)
  }

grid <- min_grid(5)

st_agr(GOMsf) = st_agr(grid) = "constant"

### remove internal boundaries from the shape file ----

tic("st_union(GOMsf)")
st_union_GOMsf <- st_union(GOMsf)
toc()
# st_union(GOMsf): 21.59 sec elapsed

# str(GOMsf)
# sf [21 × 2] (S3: sf/tbl_df/tbl/data.frame)
#  $ StatZone: num [1:21] 1 2 3 4 5 6 7 8 9 10 ...
#  $ geometry:sfc_GEOMETRY of length 21; first list element: List of 6

# str(st_union_GOMsf)
# sfc_MULTIPOLYGON of length 1; first list element: List of 15
#  $ :List of 21234

## heatmap data ----

# short_example_3_cnts_short |> glimpse()
glimpse(safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list$gom_dual)

# glimpse(safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list_cnts$gom_dual)

# for_heatmap_lat_lon_trips_only <-
#   safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list_cnts$gom_dual |>
#   select(TRIP_ID, LATITUDE, LONGITUDE) |>
#   distinct()

# glimpse(for_heatmap_lat_lon_trips_only)
# Rows: 41,455

for_heatmap_lat_lon_trips_vessels_only <-
  safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list_cnts$gom_dual |>
  select(TRIP_ID, VESSEL_OFFICIAL_NBR, LATITUDE, LONGITUDE) |>
  distinct()

# dim(for_heatmap_lat_lon_trips_vessels_only)
# Rows: 41,455

#### assuming data is dataframe with variables LATITUDE, LONGITUDE, and trips ####

## by n min grid ----
df_join_grid <-
  function(my_df) {
    my_df |>
      # join n minute grid
      st_as_sf(
        coords = c("LONGITUDE", "LATITUDE"),
        crs = st_crs(GOMsf)) |>
        # ,
        # remove = FALSE) %>%
        st_join(grid, join = st_nearest_feature) %>%
          return()
        }

# tic("effort")
# effort <- df_join_grid(for_heatmap_lat_lon_trips_only)
# toc()
# effort: 0.75 sec elapsed

tic("effort_vsl")
effort_vsl <- df_join_grid(for_heatmap_lat_lon_trips_vessels_only)
toc()
# effort_vsl: 0.62 sec elapsed

# class(effort_vsl)

## crop by the shape ----
# effort_cropped <-
#   with_st_intersection(effort,
#           GOMsf)
# effort_cropped: 81.51 sec elapsed

tic("effort_vsl_cropped")
effort_vsl_cropped <-
  effort_vsl |>
  st_join(GOMsf, left = FALSE) %>%
  mutate(LONGITUDE = st_coordinates(.)[, 1],
         LATITUDE = st_coordinates(.)[, 2])
toc()
# effort_cropped2: 0.44 sec elapsed

# class(effort_cropped)

# print_df_names(effort_vsl_cropped)

## count trip ids and vessels by grid cell ----
effort_vsl_cropped_cnt2 <-
  effort_vsl_cropped |>
  # sf::st_drop_geometry() |>
  group_by(cell_id) |>
  mutate(vsl_cnt = n_distinct(VESSEL_OFFICIAL_NBR),
         trip_id_cnt = n_distinct(TRIP_ID)) |>
  ungroup()
# [1] 35822     8

# check
effort_vsl_cropped_cnt2 |>
  sf::st_drop_geometry() |>
  filter(cell_id == 1864) |>
  select(vsl_cnt, trip_id_cnt) |>
  distinct() |>
  glimpse()
# vsl_cnt     <int> 11
# trip_id_cnt <int> 236

# class(effort_vsl_cropped_cnt2)

dim(effort_vsl_cropped_cnt2)
# [1] 35822     9

effort_cropped_short_cnt_rule3 <-
  effort_vsl_cropped_cnt2 |>
  filter(vsl_cnt > 2)

dim(effort_cropped_short_cnt_rule3)
# [1] 31981     9

## remove extra columns ----
print_df_names(effort_cropped_short_cnt_rule3)

effort_cropped_short_cnt_rule3_short <-
  effort_cropped_short_cnt_rule3 |>
  select(-c(LATITUDE, LONGITUDE, TRIP_ID, VESSEL_OFFICIAL_NBR))

dim(effort_cropped_short_cnt_rule3_short)
# [1] 31981     5

## join with min grid ----
heat.plt <-
  effort_cropped_short_cnt_rule3_short |>
  # have to use data.frame, to avoid
  # Error: y should not have class sf; for spatial joins, use st_join
  inner_join(data.frame(grid))
# Joining with `by = join_by(cell_id)`

# mapview(grid)
dim(heat.plt)
# [1] 35828     6
# [1] 33883     6 (rule3)
# [1] 31981     6

## make a plot ----
make_map_trips <-
  function(map_trip_base_data,
           shape_data,
           total_trips_title,
           trip_cnt_name,
           caption_text = "Heat map of SEFHIER trips (5 min. resolution).") {

    max_num <- max(map_trip_base_data[[trip_cnt_name]])
    map_trips <-
      ggplot() +
      geom_sf(data = map_trip_base_data,
              aes(geometry = x,
                  fill = !!sym(trip_cnt_name)),
              colour = NA) +
      geom_sf(data = shape_data, fill = NA) +
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
        legend.key.width = unit(0.9, "npc"),
        # legend.key.width = unit(3, "cm"),
        plot.caption = element_text(hjust = 0)
      ) +
      guides(fill = guide_colourbar(title.position = "top"))

    return(map_trips)
  }

max_num3 <- max(heat.plt$trip_id_cnt)
# map_trips_rule_3 <-
#   make_map_trips(heat.plt,
#            st_union_GOMsf,
#            "total trips",
#            max_num = max_num3,
#            caption_text = "Heat map of SEFHIER trips (5 min. resolution). 2022. GoM permitted vessels. Only squares with more than 3 reporting vessels are shown. ")

# map_trips_rule_3

map_trips_no_rule_3 <-
  make_map_trips(heat.plt_no_rule3_all_dots,
           st_union_GOMsf,
           "total trips")


## by zone ----

### join data and gomsf ----
tic("for_heatmap_lat_lon_trips_vessels_only_join_gomsf")
for_heatmap_lat_lon_trips_vessels_only_join_gomsf <-
  for_heatmap_lat_lon_trips_vessels_only |>
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
  for_heatmap_lat_lon_trips_vessels_only |>
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

for_heatmap_lat_lon_trips_vessels_only_inters_gomsf_cnt |>
  filter(vsl_cnt_stat_zone < 3) |>
  write_csv("state_zone_less_2_vsl.csv")

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

# repeat separately for charter and headboat ----

# Thought for exploration and not the Council meeting coming up - can we show this just for charter and the just for headboat trips?  Headboat being that they selected that in the logbook.

## get trip_type data ----

my_vessels_trips <-
  safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom_ten_min_perm_list$gom_dual |>
  select(VESSEL_OFFICIAL_NBR,
         TRIP_ID) |>
  distinct()

View(my_vessels_trips)
# str(my_vessels)
 # chr [1:626] "FL9312NA" "FL6074MJ" "FL4038KN" "FL0957RW" "FL4521RU" "994360" ...

my_vessels_str <-
  my_vessels_trips |>
  select(VESSEL_OFFICIAL_NBR) |>
  distinct() |>
  paste0(my_vessels, collapse = "', '")

get_trip_type_data_from_db <- function() {
  browser()
  con = dbConnect(
    dbDriver("Oracle"),
    username = keyring::key_list("SECPR")[1, 2],
    password = keyring::key_get("SECPR", keyring::key_list("SECPR")[1, 2]),
    dbname = "SECPR"
  )

  request_query <-
    paste0(
      "SELECT distinct
    vessel_official_nbr,
    trip_type_name
FROM
  srh.mv_safis_trip_download@secapxdv_dblk.sfsc.noaa.gov
WHERE
  trip_type in ('H', 'A')
  AND TRIP_START_DATE >= TO_DATE('01-JAN-22', 'dd-mon-yy')
  AND vessel_official_nbr in ('",
my_vessels_str,
"')
  "
    )

  db_data = dbGetQuery(con,
                       request_query)

  dbDisconnect(con)

  return(db_data)
}

tic("trip_type_data_from_db")
trip_type_data_from_db <- get_trip_type_data_from_db()
toc()
# data_overview(db_data)

