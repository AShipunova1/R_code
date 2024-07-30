# This code cleans homeport city and state from PIMS
# 1 Setup
# 1.1 Install packages if needed
# 1.2 Define dates
# 1.3 Set up paths
# 1.4 Other setup

# 2 Prepare data
# 2.1 Load data
# 2.2 Data cleanup

# 3 Clean home ports
# 3.1 

# Setup ----

## Install packages if needed ----
needed_packages <- c(
  "tidyverse",
  "devtools" # Collection of package development tools
)

# Explanations for the following code:
# - `needed_packages %in% rownames(installed.packages())` checks which packages from `needed_packages` are installed:
#   - `installed.packages()` returns a matrix of information about all installed packages.
#   - `rownames(installed.packages())` extracts the names of the installed packages.
#   - `needed_packages %in% ...` checks if each package in `needed_packages` is in the list of installed packages, returning a logical vector indicating the presence of each package.
# - `if (any(installed_packages == FALSE)) { ... }` checks if any package is not installed:
#   - `any(installed_packages == FALSE)` returns `TRUE` if at least one element in `installed_packages` is `FALSE`.
#   - `install.packages(packages[!installed_packages])` installs the packages that are not installed:
#     - `packages[!installed_packages]` selects the packages from `packages` that are not installed.
#     - `install.packages()` installs the selected packages.  

installed_packages <-
  needed_packages %in% rownames(installed.packages())

if (any(installed_packages == FALSE)) {
  install.packages(needed_packages[!installed_packages])
}

# Install helper functions for SEFHIER data analysis.
#
# Explanations for the following code:
# 
# The installation details depend on the username.
# 
# For most users, install from the main branch if not already installed.
# 
# One doesn't have to have a GitHub account to use it.
# 
# For the package developer, install from the development branch.
#
# - `if (!require("auxfunctions"))` checks if the `auxfunctions` package is installed and loaded:
#
#   - `require("auxfunctions")` attempts to load the `auxfunctions` package.
#
#   - The `!` operator negates the result, so the condition is true if the package is not installed or cannot be loaded.
#
# - `devtools::install_github("AShipunova1/R_code/auxfunctions")` installs the `auxfunctions` package from the specified GitHub repository:
#
#   - `devtools::install_github()` is a function from the `devtools` package that installs an R package directly from a GitHub repository.
#
#   - `"AShipunova1/R_code/auxfunctions"` specifies the repository and subdirectory containing the package.
# 
# This code checks if the `auxfunctions` package is available, and if not, it installs it from the GitHub repository `AShipunova1/R_code/auxfunctions`.
# 

install_helper_functions <- function() {
  if (!auxfunctions::get_username() == "anna.shipunova") {
    if (!require('auxfunctions')) {
      devtools::install_github("AShipunova1/R_code/auxfunctions")
    }
  } else {
    # For a developer, rebuild the package from the development branch. To force the installation change to 'force = TRUE'
    devtools::install_github("AShipunova1/R_code/auxfunctions@development", force = FALSE)
    # restart R session to pick up changes
    # .rs.restartR()
    library(auxfunctions)
  }
}

install_helper_functions()

## Define dates ----
# Variables for the current year(s)
my_years <- c("2022", "2023", "2024")

my_year_dates <-
  purrr::map(my_years, \(one_year) {
    my_beginning <- stringr::str_glue("{one_year}-01-01")
    my_end <- stringr::str_glue("{one_year}-12-31")
    
    res <- list(beg = my_beginning, end = my_end)
    return(res)
  })

names(my_year_dates) <- my_years

str(my_year_dates)

## Set up paths ----
#
# Different methods are used based on the user to accommodate different directory structure.
# 
# This allows the script to run correctly on multiple systems without manual path changes.
# 
# In the code in this section all user provided values have the word "manually" in the description. Everything else is created automatically.
#
# Manually: Change the following 2 lists (**my_paths** and **current_in_out_paths**) to your environment if needed. The variable _names_ are used throughout the code, so please change only the quoted _values_ inside the lists.
# 

# Check if the current username is not "anna.shipunova"
if (!auxfunctions::get_username() == "anna.shipunova") {
  auxfunctions::function_message_print(
    "Please CHANGE the following 2 lists values to your environment if needed. Use full path to your directories in quotes."
  )
  
  # 1) General directories (to look up additional files, e.g. processed data). It can be left as is if you don't have it. You can provide path to individual files later.
  my_paths <- list(inputs  = "~/my_inputs",
                   outputs = "~/my_outputs",
                   git_r   = "~/R_code")
  
  # 2) Current project code, input and output directories
  current_in_out_paths <-
    list(
      project_name = "home_port",
      code = "~/home_port/code",
      input = "~/home_port/input",
      output = "~/home_port/output"
    )
  
} else {
  # If the username is "anna.shipunova", use Anna's directory structure.
  my_paths <- auxfunctions::set_work_dir()
  project_name = "home_port"
  
  current_project_paths <- function(current_project_name = NULL) {
  
  my_paths <- auxfunctions::set_work_dir()
  
  # check
  'this.path' %in% rownames(installed.packages())

  # get this project name
  if (is.null(current_project_name)) {
    current_project_dir_name <- this.path::this.dir()
    
    # find its base name
    current_project_name <-
      basename(current_project_dir_name)
  } else {
    current_project_dir_name <- 
      file.path(my_paths$git_r, current_project_name)
  }
  
  #' use current_project_name to create input and output paths
  curr_proj_input_path <- 
    file.path(my_paths$inputs, current_project_name)
  
  auxfunctions::create_dir_if_not(curr_proj_input_path)
  
  curr_proj_output_path <- file.path(my_paths$outputs, current_project_name)
  
  auxfunctions::create_dir_if_not(curr_proj_output_path)
  
  current_proj_paths <-
    list(
      "project_name" = current_project_name,
      "code" = current_project_dir_name,
      "input" = curr_proj_input_path,
      "output" = curr_proj_output_path
    )
  
  return(current_proj_paths)
}

  
  # current_in_out_paths <- auxfunctions::current_project_paths(project_name)
}

current_project_dir_path <- this.path::this.dir()

current_project_basename <- basename(current_project_dir_path)

current_output_dir <-
  file.path(my_paths$outputs,
            current_project_basename)

#
# The following section uses provided directory names lists to automatically create separate variables for future use and create current input/output directories if they do not exists.
# 
# 
# Create variables to store shortcuts to project directories
# 
# This is usually the current directory name.
current_project_name <- current_in_out_paths$project_name

current_project_path <- current_in_out_paths$code
            
current_project_input_path <- current_in_out_paths$input

current_project_output_path <- current_in_out_paths$output

# Create input and output directories if they don't exist
auxfunctions::create_dir_if_not(current_project_input_path)

auxfunctions::create_dir_if_not(current_project_output_path)

### Additional individual paths to data files ----
# This section sets up paths for specific data files used in the project
# 
