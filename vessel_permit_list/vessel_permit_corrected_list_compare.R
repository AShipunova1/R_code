# start with "vessel_permit_corrected_list.R"
# compare with db_data ----
## prepare_db_data ----
use_df_names <- list("mv_sero_fh_permits_his",
                     "vessels_permits")
### add permit region groups ----

# Create a list of data frames 'db_df_reg_l' by applying a function to each element of 'use_df_names'.
db_df_reg_l <-

  # 'use_df_names' is a character vector containing names of data frames.
  use_df_names |>

  # Use the 'map' function to apply a custom function to each data frame in 'all_get_db_data_result_l'.
  purrr::map(function(use_df_name) {

    # Inside the 'map' function, apply the 'separate_permits_into_3_groups' function
    # to the corresponding data frame from 'all_get_db_data_result_l'.
    separate_permits_into_3_groups(all_get_db_data_result_l[[use_df_name]],
                                   permit_group_field_name = "TOP")
  }) |>

  # Set the names of the resulting list to match the names in 'use_df_names'.
  rlang::set_names(use_df_names)

# purrr::map(db_df_reg_l,
#     print_df_names)

### get 2022 sa_only ----
# Create a list of data frames 'db_df_reg_2022_sa_only_l' by applying a function to each element of 'use_df_names'.
db_df_reg_2022_sa_only_l <-

  # 'use_df_names' is a character vector containing names of data frames.
  use_df_names |>

  # Use the 'map' function to apply a custom function to each data frame in 'db_df_reg_l'.
  purrr::map(function(use_df_name) {

    # Inside the 'map' function, access the data frame 'db_df_reg_l[[use_df_name]]'.
    db_df_reg_l[[use_df_name]] |>

      # Use the 'filter' function to filter rows based on specific conditions.
      filter(
        permit_sa_gom == "sa_only" &   # Filter rows with 'permit_sa_gom' equal to "sa_only"
        EFFECTIVE_DATE >= '2022-01-01' &  # Filter rows with 'EFFECTIVE_DATE' on or after '2022-01-01'
        END_DATE > '2022-12-31'          # Filter rows with 'END_DATE' after '2022-12-31'
      )
  }) |>

  # Set the names of the resulting list to match the names in 'use_df_names'.
  rlang::set_names(use_df_names)

map(db_df_reg_2022_sa_only_l, dim)
# $mv_sero_fh_permits_his
# [1] 6189   23
#
# $vessels_permits
# [1] 12381    52

# compare Jeannettes's with mv_sero_fh_permits_his ----
mv_sero_fh_permits_his_u_v_ids_all <-
  unique(all_get_db_data_result_l$mv_sero_fh_permits_his$VESSEL_ID)

mv_sero_fh_permits_his_u_v_ids_22_sa <-
  unique(db_df_reg_2022_sa_only_l$mv_sero_fh_permits_his$VESSEL_ID)

## intersection ----
mv_sero_fh_permits_his_intersect <-
  mv_sero_fh_permits_his_u_v_ids_22_sa |>
  intersect(vessels_22_sa$permit_vessel_id)

length(mv_sero_fh_permits_his_intersect)
# [1] 1339

## in J only ----
### all ----
not_in_mv_sero_fh_permits_his_all <-
  setdiff(vessels_22_sa$permit_vessel_id,
          mv_sero_fh_permits_his_u_v_ids_all)
length(not_in_mv_sero_fh_permits_his_all)
# 18

# cat(not_in_mv_sero_fh_permits_his_all,
#     sep = ", ")

### 2022 SA ----
not_in_mv_sero_fh_permits_his <-
  setdiff(vessels_22_sa$permit_vessel_id,
          mv_sero_fh_permits_his_u_v_ids_22_sa)
length(not_in_mv_sero_fh_permits_his)
# 982

## in db only ----
### all ----
mv_sero_fh_permits_his_all__not_in_j <-
  setdiff(mv_sero_fh_permits_his_u_v_ids_all,
          vessels_22_sa$permit_vessel_id)
length(mv_sero_fh_permits_his_all__not_in_j)
# 11749

### 2022 SA ----
mv_sero_fh_permits_his__not_in_j <-
  setdiff(mv_sero_fh_permits_his_u_v_ids_22_sa,
          vessels_22_sa$permit_vessel_id)
length(mv_sero_fh_permits_his__not_in_j)
# 1357
head(mv_sero_fh_permits_his__not_in_j)

# compare Jeannettes's with vessels_permits ----
vessels_permits_u_v_ids_all <-
  unique(all_get_db_data_result_l$vessels_permits$PERMIT_VESSEL_ID)

vessels_permits_u_v_ids_22_sa <-
  unique(db_df_reg_2022_sa_only_l$vessels_permits$PERMIT_VESSEL_ID)

## intersection ----
### all ----
vessels_permits_intersect_all <-
  vessels_permits_u_v_ids_all |>
  intersect(vessels_22_sa$permit_vessel_id)

length(vessels_permits_intersect_all)
# 2301

### 2022 sa ----
vessels_permits_intersect <-
  vessels_permits_u_v_ids_22_sa |>
  intersect(vessels_22_sa$permit_vessel_id)

length(vessels_permits_intersect)
# [1] 1339

## in J only ----
### all ----
not_in_vessels_permits_all <-
  setdiff(vessels_22_sa$permit_vessel_id,
          vessels_permits_u_v_ids_all)
length(not_in_vessels_permits_all)
# 20

### 2022 SA ----
not_in_vessels_permits <-
  setdiff(vessels_22_sa$permit_vessel_id,
          vessels_permits_u_v_ids_22_sa)
length(not_in_vessels_permits)
# 982

## in db only ----
### all ----
vessels_permits_all__not_in_j <-
  setdiff(vessels_permits_u_v_ids_all,
          vessels_22_sa$permit_vessel_id)
length(vessels_permits_all__not_in_j)
# 4450

### 2022 SA ----
vessels_permits__not_in_j <-
  setdiff(vessels_permits_u_v_ids_22_sa,
          vessels_22_sa$permit_vessel_id)
length(vessels_permits__not_in_j)
# 1356
