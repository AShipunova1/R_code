# Set up ----

install.packages("devtools")
library(devtools)
devtools::install_github("AShipunova1/R_code/auxfunctions@development")
library(auxfunctions)

my_paths <- set_work_dir()

# get this project name
current_project_dir_name <- this.path::this.dir()

# find its base name
current_project_name <-
  basename(current_project_dir_name)

# use current_project_name to create input and output path
curr_proj_input_path <- file.path(my_paths$inputs,
                         current_project_name)

auxfunctions::create_dir_if_not(curr_proj_input_path)

curr_proj_output_path <- file.path(my_paths$outputs,
                         current_project_name)

auxfunctions::create_dir_if_not(curr_proj_output_path)

# get data ----
get_data_file_path <-
  file.path(current_project_dir_name,
            paste0(current_project_name, "_", "get_data.R"))

file.exists(get_data_file_path)

source(get_data_file_path)

# Data are in:
# survey_data_l_2022
# processed_logbooks_2022
# db_logbooks_2022

survey_data_l_2022 |> 
  purrr::map(print_df_names)

# $aga
# [1] "asg_num, intcd1, intcd2, state, interval, ano_int, anosite, site1, reason1, site2, reason2, start1, stop1, tsite1, start2, stop2, tsite2, start3, stop3, tsite3, start4, stop4, tsite4, year, month, day, wave, asg_code, sitehrs, int12_1, int12_2, int12, site1_comments, site2_comments, all_site_comments, control7, cluster_id, cnty, date1"
# 
# $i1
# [1] "id_code, time, hrsf, year, wave, sub_reg, intsite, vessel_name, num_typ2, num_typ3, status, for_hire_permit, la_charter_license, prefix1, prefix2, la_charter_permit_number, operating_type, srhs_vessel, interviewee_f_name, interviewee_l_name, interviewee_m_name, interviewee_suffix, interviewee_role, fishing_distance, people_fishing, no_harvested_selected, permit_number1, permit_number2, vsl_num, cnty, date1, st, comments"
# 
# $i2
# [1] "year, wave, sub_reg, id_code, tsn, num_fish, num_typ2, st, date1"
# 
# $i3
# [1] "year, wave, sub_reg, id_code, tsn, fshinsp, disp3, lngth, wgt, num_typ3, st, date1"
# 
# $ref
# [1] "id_code, time, hrsf, year, wave, st, sub_reg, intsite, vessel_name, num_typ2, num_typ3, status, comments, for_hire_permit, la_charter_license, prefix1, prefix2, la_charter_permit_number, operating_type, srhs_vessel, interviewee_f_name, interviewee_l_name, interviewee_m_name, interviewee_suffix, interviewee_role, fishing_distance, people_fishing, no_harvested_selected, permit_number1, permit_number2, vsl_num, cnty, date1"
# 

survey_data_l_2022$aga |> 
  select(year, month, day, asg_code) |> 
  distinct() |> glimpse()
# 
# sst <- strsplit(text, "")[[1]]
# out <- paste0(sst[c(TRUE, FALSE)], sst[c(FALSE, TRUE)])

survey_data_l_2022$aga |> 
  select(year, month, day, asg_code) |> 
  distinct() |> 
    mutate(asg_sp = stringr::str_replace(asg_code, "(\\d+)2022(\\d\\d)(\\d\\d)",
                                            stringr::str_c("\\1 \\2 \\3"))
) |> 
  mutate(asg_dates = stringr::str_split(asg_sp, " ")) |> 
  rowwise() |> 
  # filter(!asg_dates[[2]] == month) |> 
# 0
  filter(!asg_dates[[3]] == day) |> 
# 0
  glimpse()

# asg_code is useless for us