source("~/R_code_github/useful_functions_module.r")
my_paths <- set_work_dir()
current_project_dir_path <-
  get_current_file_directory()
current_project_dir_name <- basename(current_project_dir_path)

source(file.path(my_paths$git_r,
                 r"(get_data\all_logbooks_db_data_2022_short_p_region_prep.R)"))
ls()
