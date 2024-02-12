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

# compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col_addr__fixed_2

## split by permit and year ----
compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col_addr__fixed_2__list <-
  compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col_addr__fixed_2 |>
  split(
    as.factor(
      compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col_addr__fixed_2$year_permit_sa_gom_dual
    )
  )

# print_df_names(compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col_addr__fixed_2__list)
# [1] "2022 dual, 2022 gom_only, 2022 sa_only, 2023 dual, 2023 gom_only, 2023 sa_only"

# vessel_by_state_cnt ----
vessel_by_state_cnt <-
  compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col_addr__fixed_2__list |>
  purrr::map(\(curr_df) {
    curr_df |>
      dplyr::select(vessel_official_number, state_fixed) |>
      dplyr::distinct() |>
      dplyr::add_count(state_fixed)
  })

# vessel_by_state_cnt |> glimpse()
 # $ 2023 sa_only : tibble [2,178 × 3] (S3: tbl_df/tbl/data.frame)
 #  ..$ vessel_official_number: chr [1:2178] "684541" "641822" "992615" "1169835" ...
 #  ..$ state_fixed           : chr [1:2178] "FL" "VA" "NC" "NJ" ...
 #  ..$ n                     : int [1:2178] 1166 45 374 45 1166 1166 45 1166 1166 1166 ...

vessels_permits_home_port_22_compliance_list_vessel_by_state_cnt <-
  compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col_addr__fixed_2__list |>
  purrr::map(\(curr_df) {
    curr_df |>
      group_by(state_fixed) |>
      mutate(total_vsl_by_state_cnt =
               n_distinct(vessel_official_number)) |> 
      ungroup()
  })

## check ----
vessels_permits_home_port_22_compliance_list_vessel_by_state_cnt[["2023 sa_only"]] |>
  filter(state_fixed == "FL") |>
  select(vessel_official_number) |>
  distinct() |>
  nrow()
# [1] 1166

# vessels_permits_home_port_22_compliance_list_vessel_by_state_cnt[["2023 sa_only"]] |>
#   filter(vessel_official_number %in% c("684541", "641822", "992615", "1169835")) |> View()
  
vessels_permits_home_port_22_compliance_list_vessel_by_state_cnt[["2023 sa_only"]] |>
  select(state_fixed, total_vsl_by_state_cnt) |>
  glimpse()
# Rows: 22
# Columns: 2
# $ state_fixed            <chr> "FL", "VA", "NC", "NJ", "MD", "SC", "DE", "GA", "N…
# $ total_vsl_by_state_cnt <int> 1166, 45, 374, 45, 78, 189, 41, 59, 112, 14, 8, 16…

# vessels_permits_home_port_22_compliance_list_vessel_by_state_cnt[["2023 sa_only"]]$state_fixed |> unique() |> length()
# 22  

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
#  names() |> head()
# [1] "year"                    "permit_sa_gom_dual"      "year_permit_sa_gom_dual"
# [4] "total_vsl_by_state_cnt"  "state_fixed"             "684541"                 
vessels_permits_home_port_22_compliance_list_vessel_by_state_cnt_list_compl_wide[["2023 sa_only"]] |>
  select("684541",
         "641822",
         "992615",
         "1169835",
         total_vsl_by_state_cnt,
         state_fixed) |>
  distinct() |>
  glimpse()

  # filter(vessel_official_number %in% c("684541", "641822", "992615", "1169835")) |>

# print_df_names(vessels_permits_home_port_22_compliance_list_vessel_by_state_cnt_list_compl_wide[[1]])
# year, permit_sa_gom_dual, year_permit_sa_gom_dual, total_vsl_by_state_cnt, state_fixed, 
# back_to_longer_format
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

# View(vessels_permits_home_port_22_compliance_list_vessel_by_state_cnt_list_compl_wide_long__compl_or_not)

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

# fewer columns ----
dim(vessels_permits_home_port_22_compliance_list_vessel_by_state_cnt_list_compl_wide_long__compl_or_not__compl_cnt$`2023 sa_only`)
# [1] 2178    8

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

# map specific columns ----

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

## add to the shape file by state name ----

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

# get south states map ----
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

# Keep only SA states for SA only plots ----
shp_file_with_cnts_list_sa_only_23 <-
  shp_file_with_cnts_list$`2023 sa_only` |>
  # filter(!is.na(state_fixed_full)) |>
  filter(tolower(NAME) %in% tolower(east_coast_states$sa))

# View(shp_file_with_cnts_list_sa_only_23)
# 4 19
# $ STUSPS                      <chr> "GA", "FL", "SC", "NC"

# combine static map ----
# get boundaries from south_east_coast_states_shp_bb

base_size = 20
label_text_size <- 5
# axis_text_size <- 4
# title_text_size <- 4

sa_state_proportion_indexes <-
  shp_file_with_cnts_list_sa_only_23 |>
  sf::st_drop_geometry() |>
  select(nc_round_proportion) |>
  distinct() |>
  drop_na() |>
  arrange(nc_round_proportion)

len_colors_sa_states = nrow(sa_state_proportion_indexes)

# ---
# Explanations:
# The code defines a color palette 'mypalette' using the viridis package:
# - Use the viridis::viridis() function to generate a color palette.
# - Set the number of colors with 'len_colors_sa_states'.
# - Choose the "D" option for the color map.

mypalette = viridis(len_colors_sa_states, option = "D")
# mypalette <- rainbow(length(gom_all_cnt_indexes))
names(mypalette) <- sa_state_proportion_indexes$nc_round_proportion

# mypalette
#        0.51        0.58        0.63        0.64 
# "#440154FF" "#31688EFF" "#35B779FF" "#FDE725FF" 

# Creates a plot using the ggplot2

shp_file_with_cnts_list_sa_only_23_bbox <-
  sf::st_bbox(shp_file_with_cnts_list_sa_only_23)

# shp_file_with_cnts_list_maps_sa_23 <-
  # shp_file_with_cnts_list$`2023 sa_only` |>
  # purrr::map(\(curr_sf) {
  curr_sf_for_map <- shp_file_with_cnts_list_sa_only_23
  # shp_file_with_cnts_list_sa_only_23 |> 
  # curr_sf |>
      # filter(!is.na(total_vsl_by_state_cnt)) |> dim()
      # mutate(
        # my_nudge_y =
          # ifelse(grepl("MS:", my_label_long), 2, 0))
    
    sa_only_map  <-
      ggplot2::ggplot() +
      ggplot2::geom_sf(data = states_sf, fill = NA) +
      ggplot2::geom_sf(data = curr_sf_for_map,
                       aes(fill = factor(nc_round_proportion))) +
      ggplot2::geom_sf_label(
        data = curr_sf_for_map,
        aes(label = my_label_long),
        size = label_text_size,
        fill = "lightgrey",
        # nudge_x = curr_sf_for_map$my_nudge_x,
        # nudge_y = curr_sf_for_map$my_nudge_y
      ) +
      
      # Set the coordinate limits for the plot, based on the bounding box of southeast coast states,
      ggplot2::coord_sf(
        xlim =
          c(
            floor(shp_file_with_cnts_list_sa_only_23_bbox$xmin),
            ceiling(shp_file_with_cnts_list_sa_only_23_bbox$xmax)
          ),
        ylim =
          c(
            floor(shp_file_with_cnts_list_sa_only_23_bbox$ymin),
            ceiling(shp_file_with_cnts_list_sa_only_23_bbox$ymax)
          ),
        # with expand = FALSE to prevent expansion beyond the specified limits.
        expand = FALSE
      ) +
      ggplot2::xlab("") +
      ggplot2::ylab("") +
      scale_fill_manual(labels =
                          c("less", "", "", "more"),
                        values = mypalette) +
      theme_bw(base_size = 18) +
      # ggplot2::scale_fill_continuous(name = "",
      #                                # breaks = c(min(nc_round_perc), 'Num of weeks'),
      #                                                                  breaks = c("0.14", "0.29"),
      theme(legend.position = c(0.53, 0.1)) +
      guides(fill = guide_legend(title = "Non-Compliance Color Scale",
                                 nrow = 1))
  # })

    sa_only_map
# individual plots ----

## make map titles ----
permit_regions <-
  c("SA only",
    "GOM and Dual",
    "Gulf"
    )

# Generate plot titles using 'str_glue' to include the 'permit_region'.
perc_plot_titles <-
  permit_regions |>
  purrr::map(\(permit_region) {
    stringr::str_glue(
      "Proportion of Non-compliant, {permit_region} Permitted SEFHIER Vessels by Homeport State"
    )
  })

# Set the names of the 'perc_plot_titles' list to be the same as the 'permit_regions'.
names(perc_plot_titles) <- permit_regions

## save plot to file function ----
write_png_to_file <- function(output_file_name,
                              map_plot) {

  png_width  <- 31
  # png_height <- 25
  # png_width <- 800
  # png_height <- 600

  ggplot2::ggsave(
      file = output_file_name,
      plot = map_plot,
      device = "png",
      path = file.path(my_paths$outputs,
                       current_project_basename),
      width = png_width,
      # height = png_height,
      units = "cm" # "px"
    )
}

## SA only ----
permit_region <- "SA only"
# TODO: make mypalette dynamic by states number and names

# sa_only_map <-
#   shp_file_with_cnts_list_maps$SA +
#   ggplot2::ggtitle(perc_plot_titles[[permit_region]])

output_file_name <-
  str_glue("sa_only_perc_by_state_{today()}.png")

write_png_to_file(output_file_name,
                  sa_only_map)

# Check no home port vessels ----
## GOM ----
vessels_no_home_port <-
  vessels_permits_home_port_22_compliance_list_vessel_by_state_compl_cnt$GOM |>
  filter(state_fixed %in% c("NA", "UN") | is.na(state_fixed)) |>
  select(vessel_official_number) |>
  distinct()

nrow(vessels_no_home_port)
# 230

vessels_permits_home_port_22 |>
  select(starts_with("SERO")) |>
  distinct() |>
  filter(trimws(tolower(SERO_OFFICIAL_NUMBER)) %in% trimws(tolower(vessels_no_home_port$vessel_official_number))) |>
  nrow()
# 2

all_get_db_data_result_l$vessels_permits |>
  select(starts_with("SERO")) |>
  distinct() |>
  filter(trimws(tolower(SERO_OFFICIAL_NUMBER)) %in% trimws(tolower(vessels_no_home_port$vessel_official_number))) |>
  glimpse()


compl_err_db_data_metrics_2022_clean_list_short$GOM |>
  filter(trimws(tolower(vessel_official_number)) %in% trimws(tolower(
    vessels_no_home_port$vessel_official_number
  ))) |>
  select(vessel_official_number) |>
  distinct() |>
  nrow()
# 230

compl_err_db_data_metrics_2022_clean_list_short_uniq$GOM |>
  filter(trimws(tolower(vessel_official_number)) %in% trimws(tolower(
    vessels_no_home_port$vessel_official_number
  ))) |>
  nrow()
# 230

# vessels_permits_home_port_22_compliance_list_vessel_by_state_cnt$GOM |>
#   filter(trimws(tolower(vessel_official_number)) %in% trimws(tolower(
#     vessels_no_home_port$vessel_official_number
#   ))) |>
#   View()
