# Determine what level of the fishery is part time via declarations and VMS positions (e.g., from Jessica - number of fishing trips vs non-fishing trips). I would look at # of gulf + dual permitted vessels who declared charter/headboat fishing intended trips vs total # of permitted vessels, for 2022.

library(tictoc)

source("~/R_code_github/useful_functions_module.r")
my_paths <- set_work_dir()
current_project_name <- "part_time_fishery"

# err msg if no connection, but keep running
try(con <- connect_to_secpr())
# con <- connect_to_secpr()

# get data ----
db_compliance_dir_name <- "db_compliance"

get_data_file_path <- file.path(
  my_paths$git_r,
  db_compliance_dir_name,
  paste0("get_data_",
         db_compliance_dir_name,
         ".R")
)
source(get_data_file_path)
