# quantify_compliance_start.R

# TODO: change
# need files:
# Detail Report - via Valid and Renewable Permits Filter (SERO_NEW Source)
# Detail_Report_12312021_12312022__08_23_2023.csv
# FHIER_Compliance_2023__01_24_2023.csv
# Compliance_Error_Types_03_29_2023.csv
# Permits_2023-03-29_1611_active.csv

# Quantify program compliance.

# setup ----
# 2022, 2023
# dual + SA
library(grid)  # Load the 'grid' library, which provides low-level graphics functions.
library(zoo)   # Load the 'zoo' library, which deals with time series data.
library(gridExtra)  # Load the 'gridExtra' library for arranging and combining grid graphics.
library(cowplot)  # Load the 'cowplot' library for creating publication-ready plots with ggplot2.

# Read R Code from a File
source("~/R_code_github/useful_functions_module.r")

my_year1 <- "2022"
my_beginning1 <- str_glue("{my_year1}-01-01")
my_end1 <- str_glue("{my_year1}-12-31")

my_year2 <- "2023"
my_beginning2 <- str_glue("{my_year2}-01-01")
my_end2 <- str_glue("{my_year2}-12-31")

# Use a function defined in "useful_functions_module.r". Use F2 to see a custom functions' definition.
my_paths <- set_work_dir()

current_project_dir_name <- this.path::this.dir()

current_project_basename <-
  basename(current_project_dir_name)

curr_proj_output_path <- file.path(my_paths$outputs,
                         current_project_basename)

curr_proj_input_path <- file.path(my_paths$inputs,
                         current_project_basename)

project_name <- current_project_basename
  # "quantify_compliance_2023"

# Read R Code from files
quantify_compliance_functions_path <- 
  file.path(current_project_dir_name,
            "quantify_compliance_functions.R")

source(quantify_compliance_functions_path)

# get data ----
quantify_compliance_get_data_path <- 
  file.path(current_project_dir_name,
            "quantify_compliance_get_data.R")

source(quantify_compliance_get_data_path)

# Uses the file.path function to construct a file path. The components used are:
# my_paths$outputs: A variable containing a directory path.
# "quantify_compliance": A directory name to be appended to the path.
# today(): Represents a function used to include the current date, creating a date-specific path.
plot_file_path <-
  file.path(my_paths$outputs, project_name, today())

# create dir if doesn't exists
create_dir_if_not(plot_file_path)

plot_colors <- list("compliant" = "skyblue1",
                    "non_compliant" = "#0570B0",
                    "nc_bucket" = "deepskyblue",
                    "non_compliant_by_month" = "blue")

title_permits <- data.frame(
  permit_sa_gom_dual = c("sa_only",
                         "sa_dual",
                         "gom_only",
                         "dual"),
  title = c("SA Only",
            "SA + Dual",
            "GOM only",
            "Dual only"),
  # "2022: GOM + Dual",
  # "2023: SA + Dual"),
  long_title = c("South Atlantic",
            "South Atlantic + Dual",
            "Gulf of Mexico",
            "Dual only"),

  second_part = c("Permitted Vessels")
)

ls(pattern = "metric")
# processed_metrics_tracking_permits

# ls(pattern = "process")
# processed_logbooks

# ls(pattern = "compl_clean_sa_vs_gom_m_int")

# Vessels which are in processed_logbooks + vessels which have no logbooks at all, but are in metrics tracking ----
compl_clean_sa_vs_gom_m_int <-
  compl_clean_sa_vs_gom_m_int_c |>
  dplyr::filter(
    vessel_official_number %in% processed_logbooks$vessel_official_number |
      vessel_official_number %in% vessels_no_logbooks$vessel_official_number
  ) |> 
  distinct()

dim(compl_clean_sa_vs_gom_m_int)
# [1] 146066     24
# [1] 143767     24 (2023)
# [1] 265533     23 both
# [1] 290408     22 use processed
# [1] 290402     22

# save vsl count for future checks ----
count_all_vessels <-
  n_distinct(compl_clean_sa_vs_gom_m_int$vessel_official_number)
# 4017 
# 3411 (2023)
# 3372 in metrics only
# 3382 both
# 4016

# add permit_region from processed metrics tracking ----
compl_clean_sa_vs_gom_m_int__join_metrics <-
  compl_clean_sa_vs_gom_m_int |>
  left_join(processed_metrics_tracking_permits,
            relationship = "many-to-many")

# Joining with `by = join_by(vessel_official_number)
#   Detected an unexpected many-to-many relationship between `x` and `y`.
# ℹ Row 1 of `x` matches multiple rows in `y`.
# ℹ Row 2531 of `y` matches multiple rows in `x`.
# ℹ If a many-to-many relationship is expected, set `relationship = "many-to-many"` to
#   silence this warning.

dim(compl_clean_sa_vs_gom_m_int__join_metrics)
# [1] 535295     30

vessels_compl_or_not_per_y_r_all <-
  compl_clean_sa_vs_gom_m_int__join_metrics %>%
  dplyr::select(vessel_official_number,
         compliant_,
         year,
         permit_sa_gom_dual) %>%
  unique() %>%
  dplyr::count(compliant_, year, permit_sa_gom_dual)

vessels_compl_or_not_per_y_r_all
#    compliant_ year  permit_sa_gom_dual     n
#  1 NO         2022  dual                  86
#  2 NO         2022  gom_only             197
#  3 NO         2022  sa_only             1142
#  4 NO         2023  dual                 235
#  5 NO         2023  gom_only              24
#  6 NO         2023  sa_only             1363
#  7 YES        2022  dual                 297
#  8 YES        2022  gom_only             977
#  9 YES        2022  sa_only             1770
# 10 YES        2023  dual                 301
# 11 YES        2023  gom_only            1142
# 12 YES        2023  sa_only             1817

# add sa + dual ----
# compl_clean_sa_vs_gom_m_int__join_metrics |> glimpse()
# if sa_dual:
# compl_clean_sa_vs_gom_m_int__join_metrics__both_p <-
#   compl_clean_sa_vs_gom_m_int__join_metrics |>
#   mutate(permit_sa_gom_dual_both =
#            case_when(
#              year == "2023" &
#                (permit_sa_gom_dual == "sa_only" |
#                   permit_sa_gom_dual == "dual") ~ "sa_dual",
#              .default = permit_sa_gom_dual
#            ))
# if_sa_only
compl_clean_sa_vs_gom_m_int__join_metrics__both_p <-
  compl_clean_sa_vs_gom_m_int__join_metrics |>
  mutate(permit_sa_gom_dual_both = permit_sa_gom_dual)

# View(compl_clean_sa_vs_gom_m_int__join_metrics)

# add a year_permit column ----
compl_clean_sa_vs_gom_m_int__join_metrics__both_p__comb <-
  compl_clean_sa_vs_gom_m_int__join_metrics__both_p |>
  rowwise() |>
  mutate(year_permit_sa_gom_dual = paste(year, permit_sa_gom_dual_both)) |>
  ungroup()

compl_clean_sa_vs_gom_m_int__join_metrics__both_p__comb$year_permit_sa_gom_dual |> 
  unique() |> 
  cat(sep = "\n")
# 2022 sa_only
# 2022 dual
# 2022 gom_only
# 2023 sa_dual
# 2023 gom_only

# year ----
quantify_compliance_from_fhier_year_path <- file.path(
  my_paths$git_r,
  project_name,
  "quantify_compliance_from_fhier_year.R"
)

source(quantify_compliance_from_fhier_year_path)

# year (100 % not reporting) ----
quantify_compliance_from_fhier_year_100_nc_path <-
  file.path(my_paths$git_r,
            project_name,
            "quantify_compliance_from_fhier_year_100_nc.R")
source(quantify_compliance_from_fhier_year_100_nc_path)

# month line plots ----
month_line_plots_path <-
  file.path(my_paths$git_r,
            project_name,
            "quantify_compliance_month_line_plots.R")

source(month_line_plots_path)

