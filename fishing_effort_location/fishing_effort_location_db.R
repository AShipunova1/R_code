# setup ----
source("~/R_code_github/useful_functions_module.r")
my_paths <- set_work_dir()
current_project_dir_path <- get_current_file_directory()

# get data ----
# "C:\Users\anna.shipunova\Documents\R_code_github\get_db_data\get_db_data.R"
source(file.path(my_paths$git_r, r"(get_db_data\get_db_data.R)"))

tic("run_all_get_db_data()")
all_get_db_data_result_l <- run_all_get_db_data()
toc()
# run_all_get_db_data(): 2.27 sec elapsed (from csv)

# prepare data ----
# print_df_names(all_get_db_data_result_l)
# mv_sero_fh_permits_his, trips_info, trip_coord_info, trip_neg_2022, trips_notifications_2022, vessels_permits, dates_2022, compl_err_db_data

# trip_coord_info <-
  all_get_db_data_result_l[["trip_coord_info"]] |>
  head()
Filter(function(x)
  !all(is.na(x)),
  head(all_get_db_data_result_l[["trip_coord_info"]]))

  # filter(function(x)!all(is.na(x)), mydf)
  map_df(function(x) {
    # browser()
    if (!all(is.na(x))) {
      return(unique(x))
    }
  })

  select(-all_of(names(empty_cols)))

dim(trip_coord_info)
