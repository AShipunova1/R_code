# # setup ----
library(mapview)
# library(ggplot2)
# library(ggmap)
# library(leaflet)
# library(tigris)
# tigris_use_cache = TRUE
#
# library(rnaturalearth) #coastline
library(knitr)
# library(maps)
# library(mapdata)
# library(sf)

source("~/R_code_github/useful_functions_module.r")
my_paths <- set_work_dir()
current_project_dir_path <-
  # get_current_file_directory()
  file.path(my_paths$git_r, "geo_errors")

# get data ----
source(file.path(my_paths$git_r, r"(get_db_data\get_db_data.R)"))

tic("run_all_get_db_data()")
all_get_db_data_result_l <- run_all_get_db_data()
toc(log = TRUE, quiet = TRUE)
# run_all_get_db_data(): 2.27 sec elapsed (from csv)

# View(all_get_db_data_result_l)

trip_coord_info <-
  all_get_db_data_result_l[["trip_coord_info"]] |>
  remove_empty_cols()

## get vessel o. number  ----
vessel_permits_info <-
  all_get_db_data_result_l[["vessels_permits"]] |>
  remove_empty_cols()

# keep only id info ---
vessel_permits_ids <-
  vessel_permits_info |>
  select(VESSEL_VESSEL_ID, PERMIT_VESSEL_ID) |>
  dplyr::distinct()

# r prepare vendor columns ----
trip_coord_info_vendors <-
  trip_coord_info |>
  dplyr::group_by(LATITUDE, LONGITUDE) |>
  # dplyr::mutate(all_permits = toString(unique(TOP))) |>
  dplyr::mutate(vendor_trip = toString(unique(T_UE)),
         vendor_effort = toString(unique(E_UE))) |>
  dplyr::ungroup()

tic("trip_coord_info_vendors3_trip")
trip_coord_info_vendors3_trip <-
  trip_coord_info |>
  dplyr::group_by(LATITUDE, LONGITUDE) |>
  dplyr::mutate(vendor_trip_cat = case_when(
    trimws(tolower(T_UE)) == "vms" ~ "vms",
    trimws(tolower(T_UE)) %in% c("vesl", "bluefin") ~ "vesl",
    .default = "etrips"
  )) |>
  dplyr::ungroup()
toc(log = TRUE, quiet = TRUE)
# print_toc_log()
# trip_coord_info_vendors3_trip: 71.47 sec elapsed

trip_coord_info_vendors3 <-
  trip_coord_info_vendors3_trip |>
  dplyr::mutate(year_start = year(TRIP_START_DATE))

trip_coord_info_vendors3 |>
  select(LATITUDE, LONGITUDE, vendor_trip_cat) |>
  count(vendor_trip_cat) |>
  kable(caption = "ALL: count(vendor_trip_cat)")

trip_coord_info_vendors3 |>
  select(LATITUDE, LONGITUDE, vendor_trip_cat, year_start) |>
  count(vendor_trip_cat, year_start) |>
  kable(caption = "ALL: count(vendor_trip_cat, year_start)")

# all vendors
# etrips	47731
# vesl	77352
# vms	16267


## positive_long ----
positive_long <-
  trip_coord_info_vendors3 %>%
  filter(LONGITUDE > 0)

# world_coast map ----
world_coast <-
  rnaturalearth::ne_coastline(returnclass = "sf")

# maps functions and help ----
crs4326 <- 4326

big_bounding_box <- c(
   xmin = -97.79954,
   ymin = 21.521757, #Cuba
   xmax = -64.790337, #Bermuda
   ymax = 49 #Canada
 )

red_bounding_box <-
  geom_rect(
    aes(
      xmin = big_bounding_box[["xmin"]],
      xmax = big_bounding_box[["xmax"]],
      ymin = big_bounding_box[["ymin"]],
      ymax = big_bounding_box[["ymax"]]
    ),
    color = "red",
    fill = NA
  )

## mapview the big box ----
big_box_map <- sf::st_bbox(
  c(
    xmin = big_bounding_box[["xmin"]],
    xmax = big_bounding_box[["xmax"]],
    ymin = big_bounding_box[["ymin"]],
    ymax = big_bounding_box[["ymax"]]
  ),
  crs = crs4326
)

map_plot <-
  function(my_df_sf, coast_map = world_coast, my_title) {
    ggplot() +
      ggplot2::geom_sf(data = coast_map) +
      ggplot2::geom_sf(data = my_df_sf,
                       color = "blue") +
      ggplot2::ggtitle(my_title)
  }


# get land map ----
ne_10m_land_sf <-
  sf::read_sf(r"(my_inputs\shapefiles\ne_10m_land\ne_10m_land.shp)")

# sf::st_geometry(ne_10m_land_sf)
# Geodetic CRS:  WGS 84
# Geometry type: MULTIPOLYGON

ne_10m_land_sf_bb <-
  sf::st_crop(ne_10m_land_sf,
              big_bounding_box)
# plot(ne_10m_land_sf_bb)

# get ocean map ne ----
ne_10m_ocean_sf <-
  sf::read_sf(r"(my_inputs\shapefiles\ne_10m_ocean\ne_10m_ocean.shp)")

ne_10m_ocean_sf_bb <-
  sf::st_crop(ne_10m_ocean_sf,
              big_bounding_box)

# sf::st_crs(ne_10m_ocean_sf_bb)
    # ID["EPSG",4326]]

# prepare data ----
trip_coord_info_short <-
  trip_coord_info |>
  select(LATITUDE, LONGITUDE, TRIP_ID, VESSEL_ID) |>
  dplyr::distinct()

trip_coord_info_short_sf <-
  trip_coord_info_short |>
  filter(!is.na(LATITUDE) & !is.na(LONGITUDE)) |>
  sf::st_as_sf(
    coords = c("LONGITUDE",
               "LATITUDE"),
    crs = crs4326,
    remove = FALSE
  )

# cnt err per vessel, compare with total lgb ----
## cnt all ----

trip_coord_info_short_cnt_coord_per_vsl <-
  trip_coord_info_short |>
  select(-TRIP_ID) |>
  add_count(LATITUDE, LONGITUDE, name = "total_coords_per_vsl")
# head(trip_coord_info_short_cnt_coord_per_vsl)

trip_coord_info_short_cnt_total_trips_per_vsl <-
  trip_coord_info_short |>
  select(-c(LATITUDE, LONGITUDE)) |>
  count(VESSEL_ID, name = "total_trips_by_vsl")

# all wrong points ----

trip_coord_info_sf_out <-
  trip_coord_info_short_sf |>
  filter(lengths(
    sf::st_intersects(trip_coord_info_short_sf, ne_10m_ocean_sf_bb)
  ) == 0)

dim(trip_coord_info_short_sf)
# [1] 140748      3

str(trip_coord_info_sf_out)
# [1] 35061     3

## cnt all errors ----
trip_coord_info_sf_out_cnt_coord_per_vsl <-
  trip_coord_info_sf_out |>
  sf::st_drop_geometry() |>
  select(-TRIP_ID) |>
  add_count(LATITUDE, LONGITUDE, name = "wrong_coords_per_vsl")
head(trip_coord_info_sf_out_cnt_coord_per_vsl)

trip_coord_info_sf_out_cnt_total_trips_per_vsl <-
  trip_coord_info_sf_out |>
  sf::st_drop_geometry() |>
  select(-c(LATITUDE, LONGITUDE)) |>
  count(VESSEL_ID, name = "wrong_trips_by_vsl")
str(trip_coord_info_sf_out_cnt_total_trips_per_vsl)

# cnt positive lon ----
trip_coord_info_sf_pos_lon_cnt_coord_per_vsl <-
  trip_coord_info_sf_out |>
  sf::st_drop_geometry() |>
  select(-TRIP_ID) |>
  filter(LONGITUDE > 0) |>
  add_count(LATITUDE, LONGITUDE, name = "pos_lon_per_vsl") |>
  dplyr::distinct()
# head(trip_coord_info_sf_pos_lon_cnt_coord_per_vsl)
#   LATITUDE LONGITUDE VESSEL_ID pos_lon_per_vsl
# 1 24.00000  82.00000    326229             121
# 2 32.85700  78.53200    397126               1
# 3 24.00000  80.00000    247243              84

trip_coord_info_sf_out_cnt_pos_lon_trips_per_vsl <-
  trip_coord_info_sf_out |>
  sf::st_drop_geometry() |>
  filter(LONGITUDE > 0) |>
  select(-c(LATITUDE, LONGITUDE)) |>
  count(VESSEL_ID, name = "pos_lon_trips_by_vsl")
str(trip_coord_info_sf_out_cnt_pos_lon_trips_per_vsl)

# find fixable coords ----
# change the sign,
# good: inside the bb, not on land
## Positive longitude, corrected ----

positive_long_corrected <-
  positive_long |>
  select(VESSEL_ID, LATITUDE, LONGITUDE, TRIP_ID, vendor_trip_cat) |>
  dplyr::mutate(LONGITUDE = -abs(LONGITUDE))

positive_long_corrected_vsl_ids <-
  positive_long_corrected |>
  select(VESSEL_ID) |>
  dplyr::distinct()

dim(positive_long_corrected_vsl_ids)
# [1] 350   1

positive_long_corrected_sf <-
  positive_long_corrected |>
  sf::st_as_sf(
    coords = c("LONGITUDE",
               "LATITUDE"),
    crs = crs4326,
    remove = FALSE
  )

positive_long_corrected_map <-
  positive_long_corrected_sf |>
  map_plot(my_title = 'Positive longitude, corrected') +
  red_bounding_box

# good: inside the bb, not on land ----
positive_long_corrected_sf_good <-
  positive_long_corrected_sf |>
  filter(lengths(
    sf::st_intersects(positive_long_corrected_sf, ne_10m_ocean_sf_bb)
  ) > 0)
# mapview(positive_long_corrected_sf_good)
glimpse(positive_long_corrected_sf_good)

# bad sf ----
positive_long_corrected_sf_bad <-
  positive_long_corrected_sf |>
  filter(lengths(
    sf::st_intersects(positive_long_corrected_sf, ne_10m_ocean_sf_bb)
  ) == 0)

# mapview(positive_long_corrected_sf_bad)
# good vessels ----
positive_long_corrected_good_vsl_ids <-
  positive_long_corrected_sf_good |>
  sf::st_drop_geometry() |>
  select(VESSEL_ID, LATITUDE, LONGITUDE, TRIP_ID) |>
  dplyr::distinct()

positive_long_corrected_good_vsl_ids |> dim()
# [1] 303   1 VESSEL_ID
# [1] 3875    3 VESSEL_ID, LATITUDE, LONGITUDE
# [1] 12186     4 VESSEL_ID, LATITUDE, LONGITUDE, TRIP_ID

# positive_long_corrected_good_vsl_ids |>
#   filter(VESSEL_ID == "162619")


# bad vessels ----

positive_long_corrected_bad_vsl_ids <-
  positive_long_corrected_sf_bad |>
  sf::st_drop_geometry() |>
  select(VESSEL_ID) |>
  dplyr::distinct()

dim(positive_long_corrected_bad_vsl_ids)
# [1] 176   1

# vsl ids both good and bad ----
both <-
  dplyr::intersect(positive_long_corrected_good_vsl_ids$VESSEL_ID,
                 positive_long_corrected_bad_vsl_ids$VESSEL_ID)
length(both)
# 129

good_only <-
  dplyr::setdiff(positive_long_corrected_good_vsl_ids$VESSEL_ID,
                 positive_long_corrected_bad_vsl_ids$VESSEL_ID)
length(good_only)
# 174

bad_only <-
    dplyr::setdiff(positive_long_corrected_bad_vsl_ids$VESSEL_ID,
                   positive_long_corrected_good_vsl_ids$VESSEL_ID)

length(bad_only)
# 47

# > 47+174+129
# [1] 350
# ok, same as positive_long_corrected_vsl_ids

# bad only vsl and counts ----
trip_coord_info_sf_pos_lon_cnt_coord_per_vsl |>
  filter(VESSEL_ID %in% bad_only) |>
  dplyr::arrange(desc(pos_lon_per_vsl)) |>
  dplyr::glimpse()
# 'data.frame':	907 obs. of  4 variables:

pos_lon_bad <-
trip_coord_info_sf_out_cnt_pos_lon_trips_per_vsl |>
  filter(VESSEL_ID %in% bad_only) |>
  dplyr::arrange(desc(pos_lon_trips_by_vsl))

tot_bad <-
trip_coord_info_short_cnt_total_trips_per_vsl |>
    filter(VESSEL_ID %in% bad_only) |>
  dplyr::arrange(desc(total_trips_by_vsl))
dim(tot_bad)
# [1] 47  2

bad_cnt_join <-
full_join(tot_bad,
          pos_lon_bad) |>
  dplyr::mutate(cnt_diff = total_trips_by_vsl - pos_lon_trips_by_vsl,
         wrong_perc = pos_lon_trips_by_vsl * 100 / total_trips_by_vsl)
# Joining with `by = join_by(VESSEL_ID)`

lattice::histogram(~ cnt_diff, data = bad_cnt_join,
          xlab = "Difference between total and wrong trip coordinates")

# hist(bad_cnt_join$cnt_diff,
#      main = "Difference between total and wrong trip coordinates",
#      xlab = "Count difference")
# hist(faithful$waiting,
#      main = "Waiting Time between Eruptions",
#      xlab = "Waiting Time (in minutes)")

# check both good and bad ----
both_tot <-
  trip_coord_info_short_cnt_total_trips_per_vsl |>
  filter(VESSEL_ID %in% both) |>
  dplyr::arrange(desc(total_trips_by_vsl))

dim(both_tot)
# [1] 129   2

head(both_tot, 3)
#   VESSEL_ID total_trips_by_vsl
# 1    326154                532
# 2    390420                268
# 3    247478                257

## add coords to total cnts ----
trip_coord_info_short_both_tot_w_coords <-
  trip_coord_info_short |>
  right_join(both_tot)

head(trip_coord_info_short_both_tot_w_coords, 3)
#   LATITUDE LONGITUDE  TRIP_ID VESSEL_ID total_trips_by_vsl
# 1    24.00     80.00 66960181    247243                 94
# 2    29.58     87.23 59404746    247478                257
# 3    29.00     86.00 59406159    174924                216

# mapview(positive_long_corrected_sf_bad_both) +
  # big_box_map

## prepare good_sf: not corrected, add a col ----

positive_long_corrected_good_vsl_ids_coord_pair <-
  positive_long_corrected_good_vsl_ids |>
  dplyr::mutate(coord_pair = paste(LATITUDE, -LONGITUDE))

# dplyr::glimpse(positive_long_corrected_good_vsl_ids_coord_pair)
# positive_long__good_pairs <-
  # positive_long |>
    # dplyr::mutate(coord_pair = paste(LATITUDE, LONGITUDE)) |>
  # right_join(positive_long_corrected_good_vsl_ids_coord_pair,
             # by = join_by(LATITUDE, VESSEL_ID, coord_pair, TRIP_ID))
# exclude corrected longitude
# Joining with `by = join_by(LATITUDE, LONGITUDE, VESSEL_ID, vendor_trip_cat, coord_pair)`

# dplyr::glimpse(positive_long__good_pairs)

#   filter(VESSEL_ID %in% positive_long_corrected_good_vsl_ids$VESSEL_ID) |>
#   dplyr::mutate(coord_pair = paste(LATITUDE, LONGITUDE)) |>
#   select(VESSEL_ID, TRIP_ID, LATITUDE, LONGITUDE, vendor_trip_cat, year_start, coord_pair)
#
# dim(positive_long__good_pairs)
# # [1] 14026     7

# join all info and positive lon good ----
both_tot_w_coords__and_good_pairs <-
  trip_coord_info_short_both_tot_w_coords |>
  dplyr::mutate(coord_pair = paste(LATITUDE, LONGITUDE)) |>
  # left_join to get all trips in the "both" category only
  # and add info from positive_long__good_pairs where "good"
  left_join(positive_long_corrected_good_vsl_ids_coord_pair,
            join_by(VESSEL_ID,
                    coord_pair,
                    TRIP_ID),
            relationship = "many-to-many",
            suffix = c("_tot", "_corr_good"))

dim(both_tot_w_coords__and_good_pairs)
# [1] 24095    10
# [1] 440476     10
# [1] 16402    10 full_j
# [1] 11871    10
# [1] 11866     8

glimpse(both_tot_w_coords__and_good_pairs)

both_tot_w_coords__and_good_pairs |>
  filter(is.na(LATITUDE_tot)) |>
  dplyr::glimpse()
# 0

both_tot_w_coords__and_good_pairs_mark <-
  both_tot_w_coords__and_good_pairs |>
  dplyr::mutate(coord_mark =
           case_when(is.na(LATITUDE_corr_good)
                     ~ "wrong",
                     .default = "good"))

glimpse(both_tot_w_coords__and_good_pairs_mark)

# should be both good and wrong marks for each vsl ----

both_tot_w_coords__and_good_pairs_mark |>
  filter(VESSEL_ID == "162619") |>
  count(coord_mark)
# 1       good 73
# 2      wrong  1

both_tot_w_coords__and_good_pairs_mark_cnts_n_tot <-
  both_tot_w_coords__and_good_pairs_mark |>
  count(VESSEL_ID, coord_mark,
        name = "count_marks_per_vsl") |>
  tidyr::pivot_wider(id_cols = VESSEL_ID,
              names_from = coord_mark,
              values_from = count_marks_per_vsl) |>
  dplyr::group_by(VESSEL_ID) |>
  dplyr::mutate(tot = good + wrong) |>
  dplyr::ungroup()

both_tot_w_coords__and_good_pairs_mark_cnts <-
  both_tot_w_coords__and_good_pairs_mark |>
  add_count(VESSEL_ID, coord_mark,
            name = "count_marks_per_vsl") |>
  select(VESSEL_ID,
         total_trips_by_vsl,
         coord_mark,
         count_marks_per_vsl) |>
  dplyr::distinct()

both_tot_w_coords__and_good_pairs_mark_cnts_wide <-
  both_tot_w_coords__and_good_pairs_mark_cnts |>
  tidyr::pivot_wider(names_from = coord_mark,
              values_from = count_marks_per_vsl) |>
  dplyr::group_by(VESSEL_ID) |>
  dplyr::mutate(good_over_bad = round(good / wrong, 2),
         bad_over_good = round(wrong / good, 2)) |>
  dplyr::ungroup()

both_tot_w_coords__and_good_pairs_mark_cnts_wide_long <-
  both_tot_w_coords__and_good_pairs_mark_cnts_wide |>
  tidyr::pivot_longer(c(good, wrong),
               names_to = "coord_mark",
               values_to = "count_marks_per_vsl")

### check tot cnts ----
# both_tot_w_coords__and_good_pairs_mark_cnts_wide |>
  # filter(!total_trips_by_vsl == tot)
# 0 ok
head(both_tot_w_coords__and_good_pairs_mark_cnts_wide, 3)
#   VESSEL_ID total_trips_by_vsl  good wrong   tot
#       <int>              <int> <int> <int> <int>
# 1    247243                 94    86     8    94
# 2    247478                257   250     7   257

# ## add proportion of good over bad coords ----
# both_tot_w_coords__and_good_pairs_mark_cnts_wide_perc_g <-
#   both_tot_w_coords__and_good_pairs_mark_cnts_wide |>
#   dplyr::group_by(VESSEL_ID) |>
#   dplyr::mutate(good_percent =
#            good * 100 / total_trips_by_vsl) |>
#   dplyr::ungroup()

## histogram ----
lattice::histogram( ~ good , data = both_tot_w_coords__and_good_pairs_mark_cnts_wide,
                    xlab = "Good trip coordinates where vessels have both good and wrong coordinates")

lattice::histogram( ~ wrong , data = both_tot_w_coords__and_good_pairs_mark_cnts_wide,
                    xlab = "Wrong trip coordinates where vessels have both good and wrong coordinates")

## plot good/wrong cnts ----
# dplyr::glimpse(both_tot_w_coords__and_good_pairs_mark_cnts)

make_both_sorted_plot <-
  function(my_df,
           order_by = "total_trips_by_vsl",
           ordered_name = "total number of trips",
           ycol = "count_marks_per_vsl",
           y_title = ycol) {
    title_all = stringr::str_glue(
      " Good and wrong coordinates ordered by {ordered_name}\n For vessels having both and at least one positive lon error"
    )

    both_sorted_plot <-
      my_df |>
      ggplot(
        aes(
          fill = coord_mark,
          y = !!sym(ycol),
          x = reorder(VESSEL_ID,
                      as.integer(factor(!!sym(
                        order_by
                      ))),
                      FUN = min)
        )
      ) +
      geom_bar(position = "stack", stat = "identity") +
      labs(
        title = title_all,
        x = stringr::str_glue("Vessels sorted by {ordered_name}"),
        y = y_title
      ) +
      theme(axis.text.x = element_blank())

    return(both_sorted_plot)
  }

both_sort_by_total <-
  make_both_sorted_plot(both_tot_w_coords__and_good_pairs_mark_cnts)

# dplyr::glimpse(both_tot_w_coords__and_good_pairs_mark_cnts_wide_long)

both_sorted_by_good_plot <-
  make_both_sorted_plot(
    both_tot_w_coords__and_good_pairs_mark_cnts_wide_long,
    order_by = "good_over_bad",
    ordered_name = "good coordinates"
  )

both_sorted_by_good_plot

both_sorted_by_wrong_plot <-
  make_both_sorted_plot(
    both_tot_w_coords__and_good_pairs_mark_cnts_wide_long,
    order_by = "bad_over_good",
    ordered_name = "wrong coordinates"
  )

good_wrong_coords_dir <-
  r"(my_outputs\geo_errors\good_wrong_coords)"

ggsave(file.path(good_wrong_coords_dir, "both_sort_by_total.png"),
       plot = both_sort_by_tot_plot,
       units = "in",
       width = 7,
       # height = 4,
       dpi = 600)


ggsave(file.path(good_wrong_coords_dir, "both_sorted_by_good_plot.png"),
       plot = both_sorted_by_good_plot,
       units = "in",
       width = 7,
       # height = 4,
       dpi = 600)

ggsave(file.path(good_wrong_coords_dir, "both_sorted_by_wrong_plot.png"),
       plot = both_sorted_by_wrong_plot,
       units = "in",
       width = 7,
       # height = 4,
       dpi = 600)

# line plots ----
# dplyr::glimpse(both_tot_w_coords__and_good_pairs_mark_cnts_wide)
line_2_plot <-
  both_tot_w_coords__and_good_pairs_mark_cnts_wide |>
    # ggplot(aes(date, value01, colour = variable)) +
  # geom_line()
    ggplot(aes(x = order(good_over_bad),
               y = total_trips_by_vsl)) +
    geom_line(colour = "red") +
  # geom_point() +
    geom_line(aes(x = order(bad_over_good),
               y = total_trips_by_vsl),
              colour = "blue") +
  theme_bw() +
  labs(title = "Is there a correlation between\ngood (red), wrong (blue) and total number (y) of coordinates?",
       caption = "For vessels having both wrong and good coordinates and at least one positive lon error")

ggsave(file.path(good_wrong_coords_dir, "line_2_plot.png"),
       plot = line_2_plot,
       units = "in",
       width = 7,
       # height = 4,
       dpi = 600)
  # +
  # # text on dots
  # geom_text(aes(label = cnt_v_in_bucket2), vjust = -0.3) +
  # geom_text(aes(label = cnt_vsl_m_compl),
            # vjust = 1.3,
            # color = "blue") +
  # scale_y_continuous(breaks = seq(0, 100, by = 10),
  # labels = pecent_names) +
  # scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  # theme(legend.position = "none") +
  # labs(size = "Groups of percentage",
  #      x = "Months (2022)",
  #      y = "Number of Vessels"
       # y = "Number of Non-Compliant Vessels That Were Compliant More Than 50%",
       # y = "Percent of non-compliant vessels been non-compliant less than half a month",
       # title = "Distribution of number of weeks when a vessel was non compliant (2022 GOM + dual)")
       # ) +
  # labs(title = "The Number of Non-Compliant Vessels Each Month\nThat Were Compliant More Than 50% of a Month in 2022") +
  # # theme(plot.title = element_text(lineheight = 0.9)) +
  # labs(caption = "(The blue number is a total number of non-compliant vessels per month.)")
# guides(color = guide_legend(title = "nc weeks")) +
# ylim(0, 100)

## bad vs wrong line plot ----
line_2_plot_2 <-
both_tot_w_coords__and_good_pairs_mark_cnts_wide |>
  # ggplot(aes(date, value01, colour = variable)) +
  # geom_line()
  # ggplot(aes(x = reorder(good, bad_over_good),
  ggplot(aes(x = good,
             y = wrong)) +
  geom_line(colour = "green") +
  geom_point() +
  geom_text(aes(label = good),
            vjust = 1.3,
            color = "green") +
  geom_text(aes(label = wrong),
            vjust = -1.3,
            color = "blue") +
  theme_bw() +
  labs(title = "Is there a correlation between\ngood (green) and wrong (blue) number of coordinates?",
       caption = "For vessels having both wrong and good coordinates and at least one positive lon error") +
  theme(axis.text.x = element_blank())

line_2_plot_2
## histogram good_over_bad ----
lattice::histogram( ~ good_over_bad,
                    data = both_tot_w_coords__and_good_pairs_mark_cnts_wide,
                    xlab = "good_over_bad",
                    main = "good_over_bad")

f <- r"(..\R_code_github\geo_errors\geo_err_positive_long.R)"
file.exists(f)

# knitr::spin(r"(..\R_code_github\geo_errors\geo_err_positive_long.R)", knit = FALSE)
#
