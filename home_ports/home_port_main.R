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
  purrr::imap(my_years, \(one_year, idx) {
    browser()
    
    my_beginning <- stringr::str_glue("{one_year}-01-01")
    my_end <- stringr::str_glue("{one_year}-12-31")
    
    res <- c(my_beginning, my_end)
    
  })

names(my_year_dates) <- my_years

# str(my_year_dates)
