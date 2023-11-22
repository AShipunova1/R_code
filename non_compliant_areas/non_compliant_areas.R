# identifying any particular areas of high non-compliance to help focus future outreach efforts. 
# do this as a map
# home port from the permit as an area
# source the usual setup 
# get data

source("~/R_code_github/useful_functions_module.r")
my_paths <- set_work_dir()

current_project_dir_name <- this.path::this.dir()

db_data_path <-
  file.path(my_paths$git_r,
            r"(get_data\get_db_data\get_db_data.R)")
source(db_data_path)
tic("run_all_get_db_data()")
all_get_db_data_result_l <- run_all_get_db_data()
toc()
# run_all_get_db_data(): 8.86 sec elapsed

all_get_db_data_result_l$compl_err_db_data |> 
  print_df_names()
