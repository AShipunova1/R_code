# Colored terminal output
library(crayon)

# get db data ----
db_data_path <-
  file.path(my_paths$git_r,
            r"(get_data\get_db_data\get_db_data.R)")

source(db_data_path)

tic("run_all_get_db_data()")
all_get_db_data_result_l <- run_all_get_db_data()
toc()
# run_all_get_db_data(): 8.86 sec elapsed

# prepare non compliant vessels 2022 info ----
compl_err_db_data <- 
  all_get_db_data_result_l$compl_err_db_data

## use metricks only vessels ----
source(r"(~\R_code_github\get_data\get_data_from_fhier\metric_tracking_no_srhs.R)")

# fhier_reports_metrics_tracking_not_srhs_ids

# remove ids not in fhier_reports_metrics_tracking_not_srhs_ids
compl_err_db_data_metrics <-
  compl_err_db_data |>
  filter(
    vessel_official_nbr %in% fhier_reports_metrics_tracking_not_srhs_ids$vessel_official_number
  )

## 2022: divide by permit region ----
compl_err_db_data_metrics_permit_reg <-
  compl_err_db_data_metrics |> 
  filter(comp_week_start_dt < '2023-01-01' &
           comp_week_end_dt >= '2022-01-01') |> 
  separate_permits_into_3_groups(permit_group_field_name = "permit_group") |>
  # [1] 26391    39
  remove_empty_cols() |> 
  distinct()

dim(compl_err_db_data_metrics_permit_reg)
# [1] 26391    29

## split into separate dfs by permit region ----
compl_err_db_data_metrics_permit_reg_list <- 
  compl_err_db_data_metrics_permit_reg |> 
  split(as.factor(compl_err_db_data_metrics_permit_reg$permit_sa_gom))

map(compl_err_db_data_metrics_permit_reg_list, dim)
# $dual
# [1] 1317   29
# 
# $gom_only
# [1] 1358   29
# 
# $sa_only
# [1] 23716    29

### SA only: remove vessels not in Jeannette's SA list ----

# Build the path to the R script 'vessel_permit_corrected_list.R' by
# combining the base path 'my_paths$git_r' and the script name.
script_path <-
  file.path(my_paths$git_r,
            "vessel_permit_list/vessel_permit_corrected_list.R")

# Source (run) the R script using the constructed script path.
source(script_path)

# Rows are filtered to exclude vessels whose 'VESSEL_OFFICIAL_NBR' is in the
# 'vessels_to_remove_from_ours' vector.
compl_err_db_data_metrics_permit_reg_sa_only <-
  compl_err_db_data_metrics_permit_reg_list$sa_only |>
  filter(!vessel_official_nbr %in% vessels_to_remove_from_ours)

dim(compl_err_db_data_metrics_permit_reg_sa_only)
# [1] 22228    29

## Remove columns not use in this analysis ----
### check if all are not compliant ----
compl_err_db_data_metrics_permit_reg_sa_only |>
  select(is_comp) |>
  distinct()

# if use compliant only when permit is active add
# comp_week_start_dt and comp_week_end_dt to select()

# if override is taken in the account, add it

compl_err_db_data_metrics_permit_reg_sa_only_vsl <- 
  compl_err_db_data_metrics_permit_reg_sa_only |>
  select(vessel_official_nbr) |> 
  distinct()

"srh_vessel_comp_id, srh_vessel_comp_err_id, table_pk, comp_error_type_cd, is_override, is_send_to_vesl, send_to_vesl_dt, send_to_vesl_user_id, is_pa_review_needed, is_pa_reviewed, val_tr_res_id, vms_table_pk, safis_vessel_id, vessel_official_nbr, permit_group, prm_grp_exp_date, comp_year, comp_week, comp_week_start_dt, comp_week_end_dt, is_created_period, is_comp, is_comp_override, comp_override_dt, comp_override_user_id, srfh_for_hire_type_id, comp_override_cmt, is_pmt_on_hold, permit_sa_gom"

# non compliant only, 2022 results to use: 
# compl_err_db_data_metrics_permit_reg_sa_only
# vessels only: compl_err_db_data_metrics_permit_reg_sa_only_vsl

# prepare vessel_permit_data ----
## 2022 permits ----
vessels_permits_home_port_22 <-
  all_get_db_data_result_l$vessels_permits |>
  filter(
    LAST_EXPIRATION_DATE > "2021-12-31" |
      END_DATE > "2021-12-31" |
      EXPIRATION_DATE > "2021-12-31"
  ) |> 
  filter(EFFECTIVE_DATE < "2021-12-31") |> 
  remove_empty_cols()
  
## add permit region ----
vessels_permits_home_port_22_reg <-
  vessels_permits_home_port_22 |>
  group_by(SERO_OFFICIAL_NUMBER) |> 
  mutate(all_permits = toString(unique(sort(TOP)))) |>
  separate_permits_into_3_groups(permit_group_field_name = "all_permits") |>
  ungroup()

vessels_permits_home_port_22_reg |>
  select(permit_sa_gom) |> 
  distinct()
# 1 sa_only      
# 2 gom_only     
# 3 dual         

# vessels_permits_home_port_22_reg |> dim()
# [1] 36784    52

## shorten permit_vessel ----
vessels_permits_home_port_22_reg_short <-
  vessels_permits_home_port_22_reg |>
  select(SERO_OFFICIAL_NUMBER,
         permit_sa_gom,
         starts_with("SERO_HOME")) |>
  remove_empty_cols() |>
  distinct()

# glimpse(vessels_permits_home_port_22_reg_short)
# [1] 4729    5

cat("Result to use for vessels home port and its permit region:",
"vessels_permits_home_port_22_reg_short",
sep = "\n")

# Map 'us_s_shp' using the 'tigris' package to obtain U.S. state shapes. ----
# The 'cb = TRUE' parameter specifies that you want the U.S. state boundaries.
us_s_shp <-
  tigris::states(cb = TRUE, progress_bar = FALSE)

# Load additional dictionaries ----
misc_info_path <- file.path(my_paths$git_r,
               r"(get_data\misc_info.R)")

source(misc_info_path)

## Rows are retained if the 'NAME' column (state name) matches any of the values in 'south_east_coast_states'.
south_east_coast_states_shp <-
  us_s_shp |>
  filter(NAME %in% south_east_coast_states)

south_east_coast_states_shp_bb <- 
  sf::st_bbox(south_east_coast_states_shp)

# south_east_coast_states_shp_bb
#       xmin       ymin       xmax       ymax 
# -106.64565   24.52310  -75.46062   36.58812 

tigris_crs <- sf::st_crs(south_east_coast_states_shp)
# User input: NAD83 
# ID["EPSG",4269]]

crs4326 <- 4326

# Prepare home port coordinates ----
## Fix port addresses ----
# run once, gives vessels_permits_home_port_c_st_fixed

fix_ports_file_path <-
  file.path(my_paths$git_r,
            current_project_dir_name,
            "non_compliant_areas_fix_lat_lon.R")

source(fix_ports_file_path)

dim(vessels_permits_home_port_c_st_fixed)
# [1] 4729    8

## shorten fixed
vessels_permits_home_port_c_st_fixed_short <-
  vessels_permits_home_port_c_st_fixed |>
  select(SERO_OFFICIAL_NUMBER,
         permit_sa_gom,
         city_fixed,
         state_fixed) |>
  distinct() |>
  remove_empty_cols()
  
glimpse(vessels_permits_home_port_c_st_fixed)
# [1] 4729    8
dim(vessels_permits_home_port_c_st_fixed_short)
# [1] 4729    4

## Add lat long to fixed ports ----

my_file_path_lat_lon <- 
  file.path(my_paths$outputs, 
            current_project_dir_name,
            paste0(current_project_dir_name, "_no_county_fixed.rds"))

file.exists(my_file_path_lat_lon)

get_lat_lon_no_county <-
  function(vessels_permits_home_port_c_st_fixed_short) {
    vessels_permits_home_port_c_st_fixed_lat_longs <-
      vessels_permits_home_port_c_st_fixed_short |>
      tidygeocoder::geocode(city = "city_fixed",
                            state = "state_fixed")
    return(vessels_permits_home_port_c_st_fixed_lat_longs)
  }

vessels_permits_home_port_lat_longs_city_state <-
  read_rds_or_run(
    my_file_path_lat_lon,
    my_data =
      as.data.frame(vessels_permits_home_port_c_st_fixed_short),
    get_lat_lon_no_county
  )
# 2023-11-27 run for non_compliant_areas_no_county_fixed.rds: 632.64 sec elapsed

dim(vessels_permits_home_port_lat_longs_city_state)
# [1] 4729    6

vessels_permits_home_port_lat_longs_city_state |> 
  # dim()
# [1] 5029    5
# [1] 4729    6
data_overview()
# SERO_OFFICIAL_NUMBER 4729
# permit_sa_gom           3
# city_fixed            592
# lat                   547

# print out get_data results ----

cat(
  blue("All DB data:"),
  "all_get_db_data_result_l",
  blue("non compl 2022 sa:"),
  "compl_err_db_data_metrics_permit_reg_sa_only",
  blue("Vessels for non compl 2022 sa:"),
  "compl_err_db_data_metrics_permit_reg_sa_only_vsl",
  blue("vessel_permit 2022 with lat/long:"),
  "vessels_permits_home_port_lat_longs_city_state",
  blue("Maps:"),
  "us_s_shp",
  sep = "\n"
)
