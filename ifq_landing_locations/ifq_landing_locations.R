# setup for ifq_landing_locations ----
source("~/R_code_github/useful_functions_module.r")
my_paths <- set_work_dir()
current_project_dir_path <-
  get_current_file_directory()
current_project_dir_name <- basename(current_project_dir_path)

# get data for ifq_landing_locations ----
# 1) convert addresses from the original csv to coordinates with ARCgis
# 2) upload the result to R
input_data_file_path <-
  file.path(my_paths$inputs,
            r"(ifq_landing_locations\IFQ_Landing_Location_Use_geocoded.csv)")

input_data <- readr::read_csv(input_data_file_path)

# > problems(input_data)
# # A tibble: 2 × 5
#     row   col expected actual file                                                      
#   <int> <int> <chr>    <chr>  <chr>                                                     
# 1  1264    68 a double TX     C:/Users/anna.shipunova/Documents/R_files_local/my_inputs…
# 2  1264    78 a double TX     C:/Users/anna.shipunova/Documents/R_files_local/my_inputs…
