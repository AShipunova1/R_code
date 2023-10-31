file_to_check_path <- "~/R_code_github/fishing_effort_location/flat_file_heatmap_clean.R"
# Get by rstudioapi::getSourceEditorContext()$path

current_file_text <-
  readLines(file_to_check_path)

# str(current_file_text)
#  chr [1:3760] "#' %%%%% Prepare data" "" "" ...

# as.character(lsf.str(".GlobalEnv")) |>
#   cat(file = "functions_in_fish_eff_location.txt", sep = ', ')
# functions_in_fish_eff_location <-
#   read_lines("functions_in_fish_eff_location.txt")

functions_in_fish_eff_location <-
  as.character(lsf.str(".GlobalEnv"))

functions_in_fish_eff_location_list <-
  as.list(strsplit(functions_in_fish_eff_location, ","))

# str(functions_in_fish_eff_location_list)
# List of 82
#  $ : chr "add_count_contacts"


functions_in_fish_eff_location_web <- foodweb()

map(functions_in_fish_eff_location_list, function(f_name) {
  browser()
  callers.of(f_name, functions_in_fish_eff_location_web)
})
