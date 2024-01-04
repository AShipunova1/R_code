file_to_check_path <- "~/R_code_github/fishing_effort_location/flat_file_heatmap_clean.R"
# Get by rstudioapi::getSourceEditorContext()$path

current_file_text <-
  readr::read_lines(file_to_check_path)

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

# In this code, a list 'res' is created using purrr::map by applying a function to each element in 'functions_in_fish_eff_location_list'. The function prints the function name, checks if the function name appears in 'current_file_text', and returns a string representation of the unique results. The resulting list contains the information related to the function names and their presence in the 'current_file_text'.
# f_name: This is a variable representing the name of a function that is passed as an argument to the function.
#
# current_file_text: This is a variable representing a text or a document that is searched for the presence of the function name.
#
# grepl(f_name, current_file_text): The grepl function is used to search for the presence of the function name (f_name) in the text (current_file_text). It returns a logical vector, where each element indicates whether the function name was found in the text (TRUE) or not (FALSE).
#
# unique(grepl(f_name, current_file_text)): This part of the code applies the unique function to the logical vector returned by grepl. It removes duplicate values, so it ensures that the result contains only unique TRUE/FALSE values. It's used to find if the function name is present at least once in the text.
#
# toString(unique(grepl(f_name, current_file_text))): Finally, the toString function converts the unique logical vector into a string representation. This string will be either "TRUE" if the function name was found at least once in the text or "FALSE" if it wasn't found.
#
# In summary, the function checks if a specific function name (f_name) is present in a given text (current_file_text) and returns a string representation of whether it was found or not. This can be used to determine the occurrence of the function name in the text.
#
#

res <-
  purrr::map(functions_in_fish_eff_location_list, function(f_name) {
    # browser()
    # callers.of(f_name, functions_in_fish_eff_location_web)
    cat(f_name, sep = "\n")
    cat(toString((unique(
      grepl(f_name, current_file_text)
    ))), sep = "\n")
    # toString(unique(grepl(f_name, current_file_text)))
    # Find all occurrences of 'f_name' in 'current_file_text' and return them as a character vector
    # using the 'value' parameter set to TRUE in the 'grep' function.
    vals <-
      grep(f_name, current_file_text, value = TRUE) |>
      unique() |>
      # Convert the resulting character vector into a single comma-separated string.
      toString()
    cnt <- grepl(f_name, current_file_text) |> sum()
    return(c(vals = vals, cnt = cnt))
  })


# Convert 'res' to a data frame and store it in 'res_df'
res_df <- as.data.frame(res)
# View(res_df)
# Set the column names of 'res_df' to the elements of 'functions_in_fish_eff_location_list'
names(res_df) <- unlist(functions_in_fish_eff_location_list)

# View(res_df)

# Transpose 'res_df' and convert it to a data frame, effectively swapping rows and columns
res_df <- t(res_df) |>

  # Convert the transposed data frame to a regular data frame
  as.data.frame() |>

  # Add a new column with row names to the data frame
  rownames_to_column(var = "func_name")

# View(res_df)
# Set the column names of 'res_df' to "func_name" and "is_in_code"
# names(res_df) <- c("func_name", "is_in_code")

res_df |>
  dplyr::filter(!vals == "") |>
  dplyr::glimpse()
# Rows: 52
# Columns: 3
# $ func_name <chr> "add_vsl_and_trip_cnts", "clean_headers", "connect_to_secpr", "conver…
# $ vals      <chr> "# Define a function 'add_vsl_and_trip_cnts' that adds vessel and tri…
# $ cnt       <chr> "5", "4", "4", "3", "3", "4", "1", "2", "4", "12", "3", "2", "2", "3"…

res_df |>
  dplyr::filter(cnt == "1")
