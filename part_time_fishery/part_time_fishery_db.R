# Determine what level of the fishery is part time via declarations and VMS positions (e.g., from Jessica - number of fishing trips vs non-fishing trips). I would look at # of gulf + dual permitted vessels who declared charter/headboat fishing intended trips vs total # of permitted vessels, for 2022.

library(tictoc)
library(zoo)

source("~/R_code_github/useful_functions_module.r")
my_paths <- set_work_dir()
current_project_name <- "part_time_fishery"

# err msg if no connection, but keep running
try(con <- connect_to_secpr())
# con <- connect_to_secpr()

# get data ----
db_compliance_dir_name <- "db_compliance"

db_compliance_get_data_file_path <- file.path(
  my_paths$git_r,
  db_compliance_dir_name,
  paste0("get_data_",
         db_compliance_dir_name,
         ".R")
)

source(db_compliance_get_data_file_path)

source(file.path(my_paths$git_r,
                 db_compliance_dir_name,
                 "clean_data_db_compliance.R"))

# v_p_d_w_22_short
# t_d_w_short
# tne_d_w_short
# tn_d_w_short

# (e.g., from Jessica - number of fishing trips vs non-fishing trips). 
# total # of permitted vessels
# gulf + dual permitted vessels 
# who declared charter/headboat fishing intended trips

# print_df_names(v_p_d_w_22_short)

v_p_d_w_22_short_gom <-
  v_p_d_w_22_short |> 
  filter(permit_sa_gom_dual %in% c("dual", "gom_only"))

dim(v_p_d_w_22_short_gom)
# [1] 1348    5

# total # of gulf + dual permitted vessels 
v_p_d_w_22_short_gom_total_vsls <-
  v_p_d_w_22_short_gom |> 
  select(PERMIT_VESSEL_ID) |> 
  dplyr::distinct() |> 
  dim()

v_p_d_w_22_short_gom_total_vsls
# 1348

# gulf + dual permitted vessels who declared charter/headboat fishing intended trips
# View(tn_d_w_short)

tn_d_w_short_22 <-
  tn_d_w_short |>
  filter(YEAR == "2022")
# print_df_names(v_p_d_w_22_short_gom)

v_p__tn <-
  inner_join(tn_d_w_short_22,
            v_p_d_w_22_short_gom,
            join_by(VESSEL_ID == VESSEL_VESSEL_ID))

dim(v_p__tn)
# [1] 64568    25

v_p__tn |> 
  select(PERMIT_VESSEL_ID) |> 
  dplyr::distinct() |> 
  dim()
# 865   

# 1304 total permitted in the compliance from FHIER
