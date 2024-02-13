# Non compliant vessels (2023) by home port
# identifying any particular areas of high non-compliance to help focus future outreach efforts.
# do this as a map
# Included overriden compliance in the total counts

# home port from the permit as an area

# source the usual setup
# get data
# remove not in metrics
# separate by permit region
# add home port

# Load the 'maps' and 'mapdata' libraries, which provide functionality for working with maps in R.
library(maps)
library(mapdata)

# Load the 'mapview' library for interactive viewing of spatial data.
library(mapview)
# Load the 'leafpop' and 'leaflet' libraries, which are used for creating interactive maps in R.
library(leafpop)
library(leaflet)

library(viridis)

# The tidygeocoder package makes getting data from geocoder services easy.
# Check if the 'tidygeocoder' package is already installed; if not, install it and load the library.
if (!require(tidygeocoder)) {
  install.packages("tidygeocoder")  # Install 'tidygeocoder' package if not already installed.
  library(tidygeocoder)  # Load the 'tidygeocoder' library after installation.
}
# help(tidygeocoder)

## Load the 'tigris' package to access geographic data.
library(tigris)
## Set the 'tigris_use_cache' option to TRUE. This will enable caching of
## data retrieved from the TIGER/Line Shapefiles service, which can help
## improve data retrieval performance for future sessions.
tigris_use_cache = TRUE

# set up common functions and get data ----
source("~/R_code_github/useful_functions_module.r")
my_paths <- set_work_dir()

current_project_dir_path <- this.path::this.dir()

current_project_basename <- basename(current_project_dir_path)

current_output_dir <-
  file.path(my_paths$outputs,
            current_project_basename)

# dir.exists(current_output_dir)
my_year1 <- "2022"
my_beginning1 <- str_glue("{my_year1}-01-01")
my_end1 <- str_glue("{my_year1}-12-31")

my_year2 <- "2023"
my_beginning2 <- str_glue("{my_year2}-01-01")
my_end2 <- str_glue("{my_year2}-12-31")

# prepare data ----
get_data_file_path <-
  file.path(my_paths$git_r,
            current_project_basename,
            "non_compliant_areas_get_data.R")

source(get_data_file_path)

## Fix ports ----

fix_lat_lon_file_path <-
  file.path(my_paths$git_r,
            current_project_basename,
            "non_compliant_areas_fix_lat_lon.R")
# file.exists(fix_lat_lon_file_path)

source(fix_lat_lon_file_path)

## split by permit and year ----
compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col_addr__fixed_2__list <-
  compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col_addr__fixed_2_more_ports_more_ports |>
  split(
    as.factor(
      compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col_addr__fixed_2$year_permit_sa_gom_dual
    )
  )

# print_df_names(compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col_addr__fixed_2__list)
# [1] "2022 dual, 2022 gom_only, 2022 sa_only, 2023 dual, 2023 gom_only, 2023 sa_only"

# vessel_by_state_cnt ----
# rm(vessel_by_state_cnt)
vessel_by_state_cnt <-
  compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col_addr__fixed_2__list |>
  purrr::map(\(curr_df) {
    curr_df |>
      dplyr::select(vessel_official_number, state_fixed) |>
      dplyr::distinct() |>
      dplyr::add_count(state_fixed, name = "total_vsl_by_state_cnt") |> 
      arrange(vessel_official_number, 
              state_fixed, 
              total_vsl_by_state_cnt)
  })

# vessel_by_state_cnt |> glimpse()
 # $ 2023 sa_only : tibble [2,178 × 3] (S3: tbl_df/tbl/data.frame)
 #  ..$ vessel_official_number: chr [1:2178] "684541" "641822" "992615" "1169835" ...
 #  ..$ state_fixed           : chr [1:2178] "FL" "VA" "NC" "NJ" ...
 #  ..$ n                     : int [1:2178] 1166 45 374 45 1166 1166 45 1166 1166 1166 ...

 # $ 2023 sa_only : tibble [2,145 × 3] (S3: tbl_df/tbl/data.frame)
 #  ..$ vessel_official_number: chr [1:2145] "1000164" "1000241" "1020057" "1020529" ...
 #  ..$ state_fixed           : chr [1:2145] "FL" "NJ" "NC" "MD" ...
 #  ..$ total_vsl_by_state_cnt: int [1:2145] 1206 47 396 85 61 85 1206 1206 396 396 ...

# rm(vessels_permits_home_port_22_compliance_list_vessel_by_state_cnt)
vessels_permits_home_port_22_compliance_list_vessel_by_state_cnt <-
  compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col_addr__fixed_2__list |>
  purrr::map(\(curr_df) {
    curr_df |>
      group_by(state_fixed) |>
      mutate(total_vsl_by_state_cnt =
               n_distinct(vessel_official_number)) |> 
      ungroup() |> 
      arrange(vessel_official_number, state_fixed, total_vsl_by_state_cnt)
  })

## check ----
vessels_permits_home_port_22_compliance_list_vessel_by_state_cnt[["2023 sa_only"]] |>
  filter(state_fixed == "FL") |>
  select(vessel_official_number) |>
  distinct() |>
  nrow()
# [1] 1166
# 1226
# 1206 ?

# vessels_permits_home_port_22_compliance_list_vessel_by_state_cnt[["2023 sa_only"]] |>
#   filter(vessel_official_number %in% c("684541", "641822", "992615", "1169835")) |> View()
  
vessels_permits_home_port_22_compliance_list_vessel_by_state_cnt[["2023 sa_only"]] |>
  select(state_fixed, total_vsl_by_state_cnt) |>
  distinct() |> 
  glimpse()
# Rows: 22
# Columns: 2
# $ state_fixed            <chr> "FL", "VA", "NC", "NJ", "MD", "SC", "DE", "GA", "N…
# $ total_vsl_by_state_cnt <int> 1166, 45, 374, 45, 78, 189, 41, 59, 112, 14, 8, 16…
 
# Rows: 21
# Columns: 2
# $ state_fixed            <chr> "FL", "NJ", "NC", "MD", "GA", "SC", "VA", "DE", "T…
# $ total_vsl_by_state_cnt <int> 1206, 47, 396, 85, 61, 191, 47, 45, 16, 11, 14, 4,…

vessels_permits_home_port_22_compliance_list_vessel_by_state_cnt[["2023 sa_only"]]$state_fixed |> unique() |> length()
# 22  
# 21

# vessels_permits_home_port_22_compliance_list_vessel_by_state_cnt[["2023 sa_only"]] |> 
#   select(state_fixed) |> 
#   distinct() |> 
#   View()

# combine compliance by year ----
vessels_permits_home_port_22_compliance_list_vessel_by_state_cnt_list_compl_wide <-
  vessels_permits_home_port_22_compliance_list_vessel_by_state_cnt |>
  purrr::map(\(curr_df) {
    curr_df |>
      dplyr::select(
        vessel_official_number,
        year,
        permit_sa_gom_dual,
        year_permit_sa_gom_dual,
        compliant_,
        total_vsl_by_state_cnt,
        state_fixed
      ) |>
      dplyr::distinct() |>
      get_compl_by(group_by_for_compl)
  })

## check ----

names(vessels_permits_home_port_22_compliance_list_vessel_by_state_cnt_list_compl_wide$`2023 sa_only`) |> head()
# [1] "year"                    "permit_sa_gom_dual"      "year_permit_sa_gom_dual"
# [4] "total_vsl_by_state_cnt"  "state_fixed"             "1000164"                 
vessels_permits_home_port_22_compliance_list_vessel_by_state_cnt_list_compl_wide[["2023 sa_only"]] |>
  select("684541",
         "641822",
         "992615",
         "1169835",
         total_vsl_by_state_cnt,
         state_fixed) |>
  distinct() |>
  glimpse()

# $ `992615`               <chr> NA, NA, "NO_YES", NA, NA, NA, NA, NA, NA, NA, NA, …
# $ `1169835`              <chr> NA, "YES", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
# $ total_vsl_by_state_cnt <int> 1206, 47, 396, 85, 61, 191, 47, 45, 16, 11, 14, 4,…
# $ state_fixed            <chr> "FL", "NJ", "NC", "MD", "GA", "SC", "VA", "DE", "T…

  # filter(vessel_official_number %in% c("684541", "641822", "992615", "1169835")) |>

names(vessels_permits_home_port_22_compliance_list_vessel_by_state_cnt_list_compl_wide[[1]]) |> head()
# year, permit_sa_gom_dual, year_permit_sa_gom_dual, total_vsl_by_state_cnt, state_fixed,

# back_to_longer_format ----
cols_names <-
  c("year",
    "permit_sa_gom_dual",
    "year_permit_sa_gom_dual",
    "total_vsl_by_state_cnt", 
    "state_fixed"
    )

vessels_permits_home_port_22_compliance_list_vessel_by_state_cnt_list_compl_wide_long <-
  vessels_permits_home_port_22_compliance_list_vessel_by_state_cnt_list_compl_wide |>
  purrr::map(\(curr_df) {
    curr_df |>
      compl__back_to_longer_format(cols_names)
  })

vessels_permits_home_port_22_compliance_list_vessel_by_state_cnt_list_compl_wide_long[["2023 sa_only"]] |>
  filter(vessel_official_number %in% c("684541", "641822", "992615", "1169835")) |> 
  glimpse()
# $ total_vsl_by_state_cnt  <int> 1206, 1206, 1206, 1206, 47, 47, 47, 47, 396, 396,…
# $ state_fixed             <chr> "FL", "FL", "FL", "FL", "NJ", "NJ", "NJ", "NJ", "…
# $ vessel_official_number  <chr> "1169835", "641822", "684541", "992615", "1169835…
# $ is_compl_or_both        <chr> NA, NA, "YES", NA, "YES", NA, NA, NA, NA, NA, NA,…

## compl_or_not ----

vessels_permits_home_port_22_compliance_list_vessel_by_state_cnt_list_compl_wide_long__compl_or_not <-
  vessels_permits_home_port_22_compliance_list_vessel_by_state_cnt_list_compl_wide_long |>
  purrr::map(\(curr_df) {
    curr_df |>
      filter(stats::complete.cases(is_compl_or_both)) %>%
      mutate(compl_or_not =
               case_when(is_compl_or_both == "YES" ~
                           "compliant",
                         .default = "non_compliant")) |>
      select(-is_compl_or_both)
  })

dim(vessels_permits_home_port_22_compliance_list_vessel_by_state_cnt_list_compl_wide_long__compl_or_not$`2023 sa_only`)
# [1] 2145    7

# cnt vessel by state and compliance ----

group_by_col <-
  c(
    "year",
    "permit_sa_gom_dual",
    "year_permit_sa_gom_dual",
    "total_vsl_by_state_cnt",
    "state_fixed",
    # "vessel_official_number",
    "compl_or_not"
  )

vessels_permits_home_port_22_compliance_list_vessel_by_state_cnt_list_compl_wide_long__compl_or_not__compl_cnt <- 
vessels_permits_home_port_22_compliance_list_vessel_by_state_cnt_list_compl_wide_long__compl_or_not |> 
    purrr::map(\(curr_df) {
  add_cnt_in_gr(curr_df,
           group_by_col,
           cnt_col_name = "cnt_vsl_compl")
    })

vessels_permits_home_port_22_compliance_list_vessel_by_state_cnt_list_compl_wide_long__compl_or_not__compl_cnt$`2023 sa_only` |> 
  select(state_fixed,
         total_vsl_by_state_cnt,
         compl_or_not,
         cnt_vsl_compl) |> 
  distinct() |> 
  head()

# 1 FL                            1166 compliant               424
# 2 FL                            1166 non_compliant           742
# 3 VA                              45 compliant                13
# 4 VA                              45 non_compliant            32
# 5 NC                             374 non_compliant           235
# 6 NC                             374 compliant               139

# 424+742
# [1] 1166
# ok

# 1 FL                            1226 non_compliant           780
# 2 FL                            1226 compliant               446
# 3 NJ                              48 non_compliant            28
# 4 NJ                              48 compliant                20
# 5 NC                             399 compliant               154
# 6 NC                             399 non_compliant           245
# 780+446
# ok

# 1 FL                            1206 non_compliant           768
# 2 FL                            1206 compliant               438
# 3 NJ                              47 non_compliant            28
# 4 NJ                              47 compliant                19
# 5 NC                             396 compliant               154
# 6 NC                             396 non_compliant           242

# fewer columns ----
dim(vessels_permits_home_port_22_compliance_list_vessel_by_state_cnt_list_compl_wide_long__compl_or_not__compl_cnt$`2023 sa_only`)
# [1] 2178    8
# [1] 2145    8

vessels_permits_home_port_22_compliance_list_vessel_by_state_cnt_list_compl_wide_long__compl_or_not__compl_cnt__short <-
  vessels_permits_home_port_22_compliance_list_vessel_by_state_cnt_list_compl_wide_long__compl_or_not__compl_cnt |>
  purrr::map(\(curr_df) {
    curr_df |>
      select(state_fixed,
             total_vsl_by_state_cnt,
             compl_or_not,
             cnt_vsl_compl) |>
      distinct()
  })

dim(vessels_permits_home_port_22_compliance_list_vessel_by_state_cnt_list_compl_wide_long__compl_or_not__compl_cnt__short$`2023 sa_only`)
# [1] 37  4
# 35 4

# add proportions ----
vessels_permits_home_port_22_compliance_list_vessel_by_state_cnt_list_compl_wide_long__compl_or_not__compl_cnt__short__perc <-
  vessels_permits_home_port_22_compliance_list_vessel_by_state_cnt_list_compl_wide_long__compl_or_not__compl_cnt__short |>
  purrr::map(\(curr_df) {
    curr_df |>
      dplyr::group_by(state_fixed) |>
      dplyr::mutate(
        non_compl_proportion_per_st =
          cnt_vsl_compl /
          total_vsl_by_state_cnt,
        non_compl_percent_per_st =
          non_compl_proportion_per_st * 100
      ) |>
      dplyr::ungroup()
  })

# glimpse(vessels_permits_home_port_22_compliance_list_vessel_by_state_cnt_list_compl_wide_long__compl_or_not__compl_cnt__short__perc)
#   ..$ total_vsl_by_state_cnt     : int [1:37] 1166 1166 45 45 374 374 45 45 78 78 ...
#   ..$ compl_or_not               : chr [1:37] "compliant" "non_compliant" "compliant" "non_compliant" ...
#   ..$ cnt_vsl_compl              : int [1:37] 424 742 13 32 235 139 19 26 59 19 ...
#   ..$ non_compl_proportion_per_st: num [1:37] 0.364 0.636 0.289 0.711 0.628 ...
#   ..$ non_compl_percent_per_st   : num [1:37] 36.4 63.6 28.9 71.1 62.8 ...

# Maps ----
## map specific columns ----

vessels_permits_home_port_22_compliance_list_vessel_by_state_cnt_list_compl_wide_long__compl_or_not__compl_cnt__short__nc_perc_labels <-
  vessels_permits_home_port_22_compliance_list_vessel_by_state_cnt_list_compl_wide_long__compl_or_not__compl_cnt__short__perc |>
  purrr::map(\(curr_df) {
    curr_df |>
      filter(compl_or_not == "non_compliant") |>
      dplyr::select(
        state_fixed,
        total_vsl_by_state_cnt,
        cnt_vsl_compl,
        non_compl_percent_per_st,
        non_compl_proportion_per_st
      ) |>
      dplyr::distinct() |>
      dplyr::mutate(
        nc_round_perc = round(non_compl_percent_per_st),
        nc_round_proportion = round(non_compl_proportion_per_st, 2),
        my_label_perc =
          stringr::str_glue("{state_fixed}:
                             {nc_round_perc}% of {total_vsl_by_state_cnt}"),
        my_label_cnt =
          stringr::str_glue("{state_fixed}:
                             {cnt_vsl_compl} of {total_vsl_by_state_cnt}"),
        my_label_long =
          stringr::str_glue(
            "{state_fixed}:
                             {cnt_vsl_compl}/{total_vsl_by_state_cnt} = {nc_round_proportion}"
          )
      )
  })

# View(vessels_permits_home_port_22_compliance_list_vessel_by_state_cnt_list_compl_wide_long__compl_or_not__compl_cnt__short__nc_perc_labels)

### add to the shape file by state name ----

shp_file_with_cnts_list <-
  vessels_permits_home_port_22_compliance_list_vessel_by_state_cnt_list_compl_wide_long__compl_or_not__compl_cnt__short__nc_perc_labels |>
  purrr::map(\(curr_df) {
    # browser()
    south_east_coast_states_shp |>
      left_join(curr_df,
                join_by(STUSPS ==
                          state_fixed))
  })

# View(shp_file_with_cnts_list)

# shp_file_with_cnts_list$`2023 sa_only` |> print_df_names()
# [1] "STATEFP, STATENS, AFFGEOID, GEOID, STUSPS, NAME, LSAD, ALAND, AWATER, total_vsl_by_state_cnt, cnt_vsl_compl, non_compl_percent_per_st, non_compl_proportion_per_st, nc_round_perc, nc_round_proportion, my_label_perc, my_label_cnt, my_label_long, geometry"

## get south states map ----
# to add empty states to the map

# Explanations:
# The code creates an sf object 'states_sf' representing U.S. states:
# - Use maps::map() to generate a map of U.S. states without plotting it.
# - Convert the resulting map to an sf object using sf::st_as_sf().

states_sf <-
  sf::st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))

# add X and Y
# Explanations:
# The code adds coordinates of centroids to the 'states_sf' sf object:
# - Use sf::st_centroid() to compute the centroid of each state in 'states_sf'.
# - Use sf::st_coordinates() to extract the coordinates of centroids.
# - Use cbind() to add the coordinates to 'states_sf'.

states_sf <-
  cbind(states_sf,
        sf::st_coordinates(sf::st_centroid(states_sf)))

# mapview(states_sf)

## Keep only SA states for SA only plots ----
shp_file_with_cnts_list_sa_only_23 <-
  shp_file_with_cnts_list$`2023 sa_only` |>
  # filter(!is.na(state_fixed_full)) |>
  filter(tolower(NAME) %in% tolower(east_coast_states$sa))

# View(shp_file_with_cnts_list_sa_only_23)
# 4 19
# $ STUSPS                      <chr> "GA", "FL", "SC", "NC"

## variables for plot sizes ----

my_base_size = 18
label_text_size <- 5
# axis_text_size <- 4
# title_text_size <- 4

## get color palette ----
# Explanations:
# The code defines a color palette 'mypalette' using the viridis package:
# - Use the viridis::viridis() function to generate a color palette.
# - Set the number of colors with 'len_colors'.
# - Choose the "D" option for the color map.

get_color_palette <- 
  function(my_sf) {
    
    my_sf_proportion_indexes <-
      my_sf |>
      sf::st_drop_geometry() |>
      select(nc_round_proportion) |>
      distinct() |>
      drop_na() |>
      arrange(nc_round_proportion)
    
    len_colors = nrow(my_sf_proportion_indexes)
    
    my_palette = viridis(len_colors, option = "D")
    
    names(my_palette) <-
      my_sf_proportion_indexes$nc_round_proportion
    
    return(my_palette)
  }

## get boundaries
# from south_east_coast_states_shp_bb
get_my_bb <- function(my_sf) {
  shp_file_with_cnts_list_bbox <-
    sf::st_bbox(my_sf)
  
  return(shp_file_with_cnts_list_bbox)
}

map_plot <- 
  function(curr_sf_for_map, 
           states_sf,
           smaller_size = 0) {
    
    # browser()
    
    shp_file_with_cnts_list_bbox <-
      get_my_bb(curr_sf_for_map)
    
    my_map  <-
      ggplot2::ggplot() +
      ggplot2::geom_sf(data = states_sf, fill = NA) +
      ggplot2::geom_sf(data = curr_sf_for_map,
                       aes(fill = factor(nc_round_proportion))) +
      ggplot2::geom_sf_label(
        data = curr_sf_for_map,
        aes(label = my_label_long),
        size = label_text_size - smaller_size,
        fill = "lightgrey"
      )
    
    # Set the coordinate limits for the plot, based on the provided bounding box
    my_map  <-
      my_map +
      ggplot2::coord_sf(
        xlim =
          c(
            floor(shp_file_with_cnts_list_bbox$xmin),
            ceiling(shp_file_with_cnts_list_bbox$xmax)
          ),
        ylim =
          c(
            floor(shp_file_with_cnts_list_bbox$ymin),
            ceiling(shp_file_with_cnts_list_bbox$ymax)
          ),
        # with expand = FALSE to prevent expansion beyond the specified limits.
        expand = FALSE
      )
    
    my_palette <- get_color_palette(curr_sf_for_map)
    shp_file_with_cnts_list_bbox <-
      get_my_bb(curr_sf_for_map)
    
    my_labels <-
      c("less",
        vector("character" , length(my_palette) - 2),
        "more")
    
    my_map  <-
      my_map +
      ggplot2::xlab("") +
      ggplot2::ylab("") +
      scale_fill_manual(labels = my_labels,
                        values = my_palette) +
      theme_bw(base_size = my_base_size) +
      theme(legend.position = c(0.53, 0.1)) +
      guides(fill = guide_legend(title = "Non-Compliance Color Scale",
                                 nrow = 1))
    
    return(my_map)
  }

## all_sa_permitted_map ----

all_sa_permitted_map <-
  map_plot(shp_file_with_cnts_list$`2023 sa_only`, 
           states_sf,
           smaller_size = 2)

# all_sa_permitted_map
# 	771 × 405 px

#         0.5        0.51        0.56        0.57        0.61        0.64 
# "#440154FF" "#46337EFF" "#365C8DFF" "#277F8EFF" "#1FA187FF" "#4AC16DFF" 
#        0.83           1 
# "#9FDA3AFF" "#FDE725FF" 

#       xmin       ymin       xmax       ymax 
# -106.64565   24.52310  -75.46062   36.58812 

# sa_states_only_sa_23_permitted_map ----
sa_states_only_sa_23_permitted_map <-
  map_plot(shp_file_with_cnts_list_sa_only_23, 
           states_sf)

## save plot to file function ----
write_png_to_file <- function(output_file_name,
                              map_plot,
                              png_width,
                              png_height,
                              my_units = "in"
                              ) {
  ggplot2::ggsave(
      file = output_file_name,
      plot = map_plot,
      device = "png",
      path = file.path(my_paths$outputs,
                       current_project_basename),
      width = png_width,
      height = png_height,
      units = my_units,
      dpi = 300
    )
}

# find the width and height in zoom / "inspect element"
# src="plot_zoom_png?width=776&amp;height=600"
write_png_to_file(
  "sa_states_only_sa_23_permitted_map.png",
  sa_states_only_sa_23_permitted_map,
  png_width = 5.50,
  png_height = 5.93
)

# all_sa_permitted_map
# 	771 × 405 px
 
write_png_to_file(
  "all_sa_permitted_map.png",
  all_sa_permitted_map,
  png_width = 7.71,
  png_height = 4.05
)

## SA only ----
permit_region <- "SA only"

# doesn't fit for SA
# sa_only_map_titled <-
# sa_only_map +
#   ggplot2::ggtitle(perc_plot_titles[["SA only"]])

output_file_name <-
  str_glue("sa_only_{my_year2}_perc_by_state_{today()}.png")

# Get numbers of all SA permitted vessels in 2023 ----

## 1) metrics tracking ----
# compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col_list
processed_metrics_tracking_permits_2023 <-
  processed_metrics_tracking_permits |>
  filter(end_date >= my_beginning2 &
           effective_date <= my_end2) 
min(processed_metrics_tracking_permits_2023$effective_date)
# [1] "2006-02-28"
max(processed_metrics_tracking_permits_2023$effective_date)
# [1] "2023-12-29"
min(processed_metrics_tracking_permits_2023$end_date)
# [1] "2023-01-03"
max(processed_metrics_tracking_permits_2023$end_date)
# [1] "2025-06-30"

processed_metrics_tracking_permits_2023_short <-
  processed_metrics_tracking_permits_2023 |>
  select(vessel_official_number,
         permit_sa_gom_dual) |>
  distinct()

processed_metrics_tracking_permits_2023_short |>
  group_by(permit_sa_gom_dual) |> 
  mutate(count_vessels_by_permit = n_distinct(vessel_official_number)) |> 
  select(-vessel_official_number) |> 
  distinct() |>
  arrange(permit_sa_gom_dual)

#   permit_sa_gom_dual count_vessels_by_permit
#   <chr>                                <int>
# 1 dual                                   310
# 2 gom_only                              1130
# 3 sa_only                               2167

vessls_in_metrics <- 
  processed_metrics_tracking_permits_2023_short |> 
  filter(permit_sa_gom_dual == "sa_only") |> 
  select(vessel_official_number) |> 
  distinct()

dim(vessls_in_metrics)
# 2167 1

## 2) in compliance + metrics after adding home ports ----

compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col_addr__fixed_2__list$`2023 sa_only` |> 
  mutate(vsl_num = n_distinct(vessel_official_number)) |> 
  select(vsl_num) |> 
  distinct()
# 2178  

vessls_in_compl__metr <- 
  compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col_addr__fixed_2__list$`2023 sa_only` |> 
  select(vessel_official_number) |> 
  distinct()

dim(vessls_in_compl__metr)
# 2178 1

setdiff(vessls_in_metrics$vessel_official_number, 
        vessls_in_compl__metr$vessel_official_number)

# [1] "NC1075EA" "644342"   "FL4491NW" "697962"   "1316517"  "1299900"  "1301119" 

setdiff(vessls_in_compl__metr$vessel_official_number,
        vessls_in_metrics$vessel_official_number)
#  [1] "517238"   "FL8312PA" "936388"   "FL4650HK" "1064042"  "FL9242GM" "982351"  
#  [8] "906483"   "1293629"  "1209015"  "1207188"  "FL9793RU" "1320533"  "1078789" 
# [15] "1061382"  "974323"   "523112"   "FL3860SK"

## 3) before plotting ----
vessels_permits_home_port_22_compliance_list_vessel_by_state_cnt_list_compl_wide_long__compl_or_not__compl_cnt__short__perc$`2023 sa_only` |> 
  select(state_fixed, total_vsl_by_state_cnt) |> 
  distinct() |> 
  count(wt = total_vsl_by_state_cnt)
  # mutate(sum(total_vsl_by_state_cnt)) same result
# 2178

# 4) join with south_east_coast_states_shp
shp_file_with_cnts_list$`2023 sa_only` |> 
  sf::st_drop_geometry() |>
  count(wt = total_vsl_by_state_cnt)
# 1911
# 2178-1911
# lost 267

# Where is the loss?
shp_file_with_cnts_list <-
  vessels_permits_home_port_22_compliance_list_vessel_by_state_cnt_list_compl_wide_long__compl_or_not__compl_cnt__short__nc_perc_labels |>
  purrr::map(\(curr_df) {
    # browser()
    south_east_coast_states_shp |>
      left_join(curr_df,
                join_by(STUSPS ==
                          state_fixed))
  })


# shp_file_with_cnts_list_sa_only_23$total_vsl_by_state_cnt |> sum()
# # [1] 1788
# 
# shp_file_with_cnts_list$`2023 sa_only`$total_vsl_by_state_cnt |> sum()
# # 1818

# Total Vessels With SA Only
# 2,207

# 1) In Metric tracking on FHIER:
# Total Vessels With SA Only
# 2207
# 
# 2) in Processed metric tracking: 
# 2167
# 3            sa_only 2149
# processed_metrics_permit_info |>
#   count(permit_sa_gom_dual)

# compl_metric_permit_sa_2023 <- 
# compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col_list$`2023 sa_only` |>
#   filter(permit_sa_gom_dual == "sa_only",
#          year == "2023")

# compl_metric_permit_sa_2023 |>
#   mutate(vsl_cnt = n_distinct(vessel_official_number)) |>
#   select(vsl_cnt) |>
#   distinct() |> 
#   glimpse()
# # 2145

# no port
# compl_metric_permit_sa_2023 |>
#      filter(is.na(hailing_port)) |>
# glimpse()
# [1] 158   6

# all_sa_23_states_cnts <-
#   vessel_by_state_cnt$`2023 sa_only` |>
#   select(state_fixed, total_vsl_by_state_cnt) |>
#   distinct() |>
#   mutate(tot_cnts = sum(total_vsl_by_state_cnt))
# # # 2178
# 
# # write_csv(
# #   all_sa_23_states_cnts,
# #   file.path(current_output_dir,
# #             "all_sa_23_states_cnts.csv")
# # )
# 
# # 3) Vessels having trips with correct (or fixed) coordinates, including SA permitted vessels with trips in states other than the 4 SA states:
# # 1818
# vessels_permits_home_port_22_compliance_list_vessel_by_state_cnt$`2023 sa_only` |>
#   select(state_fixed, total_vsl_by_state_cnt) |>
#   distinct() |>
#   mutate(all_csls = sum(total_vsl_by_state_cnt)) |>
#   glimpse()
# # 2178
# 
# # 172
# 
# # 4) Vessels with trips in 4 SA states only:
# vessels_permits_home_port_22_compliance_list_vessel_by_state_cnt$`2023 sa_only` |>
#     select(state_fixed, total_vsl_by_state_cnt) |>
#     distinct() |> 
#     filter(state_fixed %in% c("FL", "GA", "NC", "SC")) |> 
#       mutate(tot_cnts = sum(total_vsl_by_state_cnt)) |> 
#     glimpse()
# # $ state_fixed            <chr> "GA", "FL", "NC", "SC"
# # $ total_vsl_by_state_cnt <int> 59, 1166, 374, 189
# # $ tot_cnts               <int> 1788, 1788, 1788, 1788
# # 1788
# # 1879
# 
# # About 88% of all SA only permitted vessels in 2023 are on this map.
# # 1879/2145*100
# # 87.59907
# 
# # 12% lost
# # 2145-1879
# # 266
# 
# # *) check where are they home ports
# # 266