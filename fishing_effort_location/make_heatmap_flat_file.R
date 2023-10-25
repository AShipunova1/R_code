# heatmap_make_flat_file.R
# Remove all "source" lines from the flat file when done.
# source( to #source(


source("~/R_code_github/useful_functions_module.r")
my_paths <- set_work_dir()

dir_to_comb <- "~/R_code_github/fishing_effort_location"

flat_file_name <-
  file.path(dir_to_comb, "flat_file_heatmap.R")

# ---
# Define a custom R function 'write_to_flat_file_with_header' that adds a header and a specified file's content to a flat file.
write_to_flat_file_with_header <-
  function(flat_file_name, add_file_name) {

    # Use 'cat' to write a header line with the current file name to 'flat_file_name'.
  # The header is generated with 'str_glue', including the current file's name extracted from 'add_file_name'.
  cat(
    stringr::str_glue("#' ## Current file: {basename(add_file_name)}"),  # Create the header using 'str_glue'.
    file = flat_file_name,  # Specify the output file.
    append = TRUE,          # Append to an existing file if it exists.
    sep = "\n"             # Specify a newline separator.
  )

  # Call the 'write_to_1_flat_file' function to write the contents of 'add_file_name' to 'flat_file_name'.
  write_to_1_flat_file(flat_file_name, add_file_name)
}

# ===
add_file_name <- "~/R_code_github/useful_functions_module.r"
write_to_flat_file_with_header(flat_file_name, add_file_name)

cat(
  'my_paths <- set_work_dir()',
  file = flat_file_name,
  append = TRUE,
  sep = "\n"
)

get_data_from_fhier_dir <- file.path("get_data",
                                     "get_data_from_fhier")

metrick_tracking_files <-
  list("get_srhs_vessels.R",
    "get_metrics_tracking.R",
    "metric_tracking_no_srhs.R")

map(metrick_tracking_files,
    function(file_name) {
      get_metrics_tracking_path <-
        file.path(my_paths$git_r,
                  get_data_from_fhier_dir,
                  file_name)

      # write_to_1_flat_file(flat_file_name,
      #                      get_metrics_tracking_path)
      write_to_flat_file_with_header(flat_file_name, get_metrics_tracking_path)
    }
)

get_db_data_path <-
  file.path(my_paths$git_r, r"(get_data\get_db_data\get_db_data.R)")

write_to_flat_file_with_header(
  flat_file_name,
  get_db_data_path
)

# Build the path to the R script 'vessel_permit_corrected_list.R' by
# combining the base path 'my_paths$git_r' and the script name.
script_path <-
  file.path(my_paths$git_r,
            "vessel_permit_list/vessel_permit_corrected_list.R")

# Source (run) the R script using the constructed script path.
# source(script_path)
# Remove "source(script_path)" from the flat file

write_to_flat_file_with_header(
  flat_file_name,
  script_path
)

write_to_flat_file_with_header(
  flat_file_name,
  file.path(
    my_paths$git_r,
    r"(get_data\all_logbooks_db_data_2022_short_p_region_prep.R)"
  )
)

write_to_flat_file_with_header(
  flat_file_name,
  file.path(
    my_paths$git_r,
    r"(fishing_effort_location\fishing_effort_locations_get_data.R)"
  )
)


write_to_flat_file_with_header(
  flat_file_name,
  file.path(
    my_paths$git_r,
    r"(fishing_effort_location\fishing_effort_location_by_permit.R)"
  )
)

cat(
  paste("library(ggplot2) # a visualization package",
  "library(ggmap) # extends 'ggplot2' for creating maps and working with spatial data.",
  sep = "\n"),
  file = flat_file_name,
  append = TRUE,
  sep = "\n"
)

write_to_flat_file_with_header(
  flat_file_name,
  file.path(
    my_paths$git_r,
    r"(fishing_effort_location\prepare_gom_heatmap_func.R)"
  )
)

write_to_flat_file_with_header(
  flat_file_name,
  file.path(
    my_paths$git_r,
    r"(fishing_effort_location\fishing_effort_location_heatmap.R)"
  )
)

# sink()

file.exists(flat_file_name)
# T
sink.number()
# 0 - right
# delete the file
# unlink(flat_file_name)
#
