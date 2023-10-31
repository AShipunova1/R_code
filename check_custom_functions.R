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

# functions_in_fish_eff_location_web <- foodweb()

result_func_list <-
  data.frame(func_name = character(0),
             is_in_code = character(0))

res <-
  map(functions_in_fish_eff_location_list, function(f_name) {
    # browser()
    # callers.of(f_name, functions_in_fish_eff_location_web)
    cat(f_name, sep = "\n")
    cat(toString((unique(grepl(f_name, current_file_text)))), sep = "\n")
    toString(unique(grepl(f_name, current_file_text)))
    # res_temp <-
    #   result_func_list |>
    #   mutate(func_name = f_name,
    #          is_in_code = toString(unique(grepl(f_name, current_file_text))))
     # = toString(unique(grepl(f_name, current_file_text)))
    # cat(f_name, sep = "\n")
    # grep(f_name, current_file_text, value = T)
    # return(res_temp)
  })

rr <- as.data.frame(res)
names(rr)
names(rr) <- unlist(functions_in_fish_eff_location_list)
rr2 <-
  t(rr) |>
  as.data.frame() |>
  rownames_to_column()

names(rr2) <- c("func_name", "is_in_code")

View(rr2)
