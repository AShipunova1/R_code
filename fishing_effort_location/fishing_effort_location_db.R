# setup ----
source("~/R_code_github/useful_functions_module.r")
my_paths <- set_work_dir()
current_project_dir_path <- get_current_file_directory()

# get data ----
# "C:\Users\anna.shipunova\Documents\R_code_github\get_db_data\get_db_data.R"
source(file.path(my_paths$git_r, r"(get_db_data\get_db_data.R)"))


# prepare data ----
# fishing_data_from_db
