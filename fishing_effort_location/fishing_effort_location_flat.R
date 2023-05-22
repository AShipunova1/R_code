#### Current file: C:/Users/anna.shipunova/Documents/R_code_github/useful_functions_module.r ----

# nolint: commented_code_linter
# useful functions

##--- start functions ---
# How to use:
# my_paths <- set_work_dir()
# csv_names_list = list("report1.csv", "report2.csv")
# xls_names_list = list("report1a.xls", "report2a.xls")
# csv_content_1 <- load_csv_names(my_paths, csv_names_list)[[1]]
# xls_content_1 <- load_xls_names(my_paths, xls_names_list, sheet_num = 2)[[1]]

## get csv data into variables
# temp_var <- get_compl_and_corresp_data(my_paths, filenames = csv_names_list_22_23)
# compl_clean <- temp_var[[1]]
# corresp_contact_cnts_clean <- temp_var[[2]]

#---

#install.packages("tidyverse")
library(tidyverse)
library(magrittr)
library(readxl)  # reading in .xlsx
library(rbenchmark)
library(ROracle)
library(zoo) #date manipulations
library(sf) #Create sf object to work with coordinates
library(mapview) #View spatial objects interactively

# Do not show warnings about groups
options(dplyr.summarise.inform = FALSE)
# Turn off the scientific notation
options(scipen = 999)

# Use my function in case we want to change the case in all functions
my_headers_case_function <- tolower

set_work_dir <- function() {
  setwd("~/")
  base_dir <- getwd()
  main_r_dir <- "R_files_local"

  in_dir <- "my_inputs"
  full_path_to_in_dir <- file.path(base_dir, main_r_dir, in_dir)
  out_dir <- "my_outputs"
  full_path_to_out_dir <- file.path(base_dir, main_r_dir, out_dir)

  git_r_dir <- "R_code_github"
  full_path_to_r_git_dir <- file.path(base_dir, git_r_dir)

  setwd(file.path(base_dir, main_r_dir))

  my_paths <- list("inputs" = full_path_to_in_dir,
                   "outputs" = full_path_to_out_dir,
                   "git_r" = full_path_to_r_git_dir)
  return(my_paths)
}

load_csv_names <- function(my_paths, csv_names_list) {
  my_inputs <- my_paths$inputs
# add input directory path in front of each file name.
  myfiles <- lapply(csv_names_list, function(x) file.path(my_inputs, x))
  # read all csv files
  # contents <- lapply(myfiles, read.csv, skipNul = TRUE, header = TRUE)
  contents <- lapply(myfiles, read_csv, col_types = cols(.default = 'c'))

  return(contents)
}

load_xls_names <- function(my_paths, xls_names_list, sheet_n = 1) {
  my_inputs <- my_paths$inputs

  # add input directory path in front of each file name.
  myfiles <- lapply(xls_names_list, function(x) file.path(my_inputs, x))

  # browser()
  # print("map:")
  # start_time <- Sys.time()
  ## read all files
  contents <- map_df(myfiles,
         ~read_excel(.x,
                     sheet = sheet_n,
                     .name_repair = fix_names,
                     guess_max = 21474836,
                     col_types = "text"))
  # %>%
  # , col_types = "character"
  #   type_convert(guess_integer = TRUE)
  # end_time <- Sys.time()
  # print(end_time - start_time)
  return(contents)
}

clean_headers <- function(my_df) {
  colnames(my_df) %<>%
    fix_names()
  return(my_df)
}

# to use in a function,
# e.g. read_csv(name_repair = fix_names)
fix_names <- function(x) {
  x %>%
    # remove dots
    str_replace_all("\\.", "") %>%
    # all not letters and numbers to underscores
    str_replace_all("[^A-z0-9]", "_") %>%
    # letters only in the beginning
    str_replace_all("^(_*)(.+)", "\\2\\1") %>%
    # tolower
    my_headers_case_function()
}

## ---- functions to clean FHIER compliance and correspondense reports ----

# split week column ("52: 12/26/2022 - 01/01/2023") into 3 columns with proper classes, week_num (week order number), week_start and week_end
clean_weeks <- function(my_df) {
  my_df %>%
    separate_wider_delim(week, ":", names = c("week_num", "week_rest")) %>%
    separate_wider_delim(week_rest, " - ", names = c("week_start", "week_end")) ->
    temp_df

  my_df$week_num <- as.integer(trimws(temp_df$week_num))
  my_df$week_start <- as.Date(trimws(temp_df$week_start), "%m/%d/%Y")
  my_df$week_end <- as.Date(trimws(temp_df$week_end), "%m/%d/%Y")

  return(my_df)
}

# trim vesselofficialnumber, there are 273 white spaces in Feb 2023
trim_all_vessel_ids_simple <-
  function(csvs_clean_ws, col_name_to_trim = NA) {
    csvs_clean <- lapply(csvs_clean_ws, function(x) {
      if (is.na(col_name_to_trim)) {
        col_name_to_trim <- grep("vessel.*official.*number",
                                 tolower(names(x)),
                                 value = T)
      }
      col_name_to_trim_s <- sym(col_name_to_trim)
      # Hard code vessel_official_number as vessel id
      x %>%
        mutate(vessel_official_number = trimws(!!col_name_to_trim_s)) %>%
        # mutate({{col_name_to_trim_s}} := trimws(!!col_name_to_trim_s)) %>%
        return()
    })
    return(csvs_clean)
  }

# cleaning, regularly done for csvs downloaded from PHIER
clean_all_csvs <- function(csvs, vessel_id_field_name = NA) {
  # unify headers
  csvs_clean0 <- lapply(csvs, clean_headers)
  # trim vesselofficialnumber, just in case
  # browser()
  csvs_clean1 <- trim_all_vessel_ids_simple(csvs_clean0, vessel_id_field_name)
  return(csvs_clean1)
}

join_same_kind_csvs <- function(csvs_list_2_plus) {
  return(bind_rows(csvs_list_2_plus))
}

# Combine correspondence and compliance information into one dataframe by "vesselofficialnumber" only. Not by time!
join_all_csvs <- function(corresp_arr, compl_arr) {
  corresp <- corresp_arr
  if (!is.data.frame(corresp_arr)) {
    corresp <- join_same_kind_csvs(corresp_arr)
  }

  compl <- compl_arr
  if (!is.data.frame(compl_arr)) {
    compl <- join_same_kind_csvs(compl_arr)
  }

  compl %>%
    full_join(corresp,
              by = c("vesselofficialnumber"),
              multiple = "all") %>%
    return()
}

# Change a column class to POSIXct in the "my_df" for the field "field_name" using the "date_format"
change_to_dates <- function(my_df, field_name, date_format) {
  my_df %>%
    mutate({{field_name}} := as.POSIXct(pull(my_df[field_name]),
    format = date_format)) %>%
    return()
}

aux_fun_for_dates <- function(x, date_format) {
  out <- as.POSIXct(x,
                    format = date_format)
  out
}

# across(a:b, \(x) mean(x, na.rm = TRUE))

change_fields_arr_to_dates <- function(my_df, field_names_arr, date_format) {
  my_df %>%
    mutate(across(all_of(field_names_arr), aux_fun_for_dates, date_format)) %>%

    # mutate({{field_name}} := as.POSIXct(pull(my_df[field_name]),
                                        # format = date_format)) %>%
    return()
}

# Use for contacts in the setup function before combining with compliant dataframes
add_count_contacts <- function(all_data_df_clean) {
  # browser()
  contactdate_field_name <- find_col_name(all_data_df_clean, "contact", "date")[1]
  vessel_id_field_name <- find_col_name(all_data_df_clean, "vessel", "number")[1]

  # browser()
  all_data_df_clean %>%
    # add a new column with a "yes" if there is a contactdate (and a "no" if not)
    # TODO: as.factor
    mutate(was_contacted = if_else(is.na(contactdate_field_name), "no", "yes")) %>%
    # group by vesselofficialnumber and count how many "contacts" are there for each. Save in the "contact_freq" column.
    add_count(!!sym(vessel_id_field_name), was_contacted, name = "contact_freq") %>%
    return()
}

# Get frequencies for each column in the list
# usage:
# group_by_arr <- c("vesselofficialnumber", "contacttype")
# count_by_column_arr(my_df, group_by_arr)
count_by_column_arr <- function(my_df, group_by_arr) {
  my_df %>%
    arrange(group_by_arr[1]) %>%
    group_by_at(group_by_arr) %>%
    summarise(my_freq = n()) %>%
    return()
}

data_overview <- function(my_df) {
  summary(my_df) %>% print()
  cat("\nCount unique values in each column:")
  count_uniq_by_column(my_df)
}

count_uniq_by_column <- function(my_df) {
  sapply(my_df, function(x) length(unique(x))) %>%
    as.data.frame()
}

# from https://stackoverflow.com/questions/53781563/combine-rows-based-on-multiple-columns-and-keep-all-unique-values
# concat_unique <- function(x){paste(unique(x),  collapse=', ')}

concat_unique <- function(x){paste0(unique(x[!is.na(x)]), collapse= ", ")}
combine_rows_based_on_multiple_columns_and_keep_all_unique_values <- function(my_df, group_by_arr) {
  my_df %>%
    group_by_at(group_by_arr) %>%
    summarise_all(concat_unique) %>%
    return()
}

concat_unique_sorted <- function(x){paste0(unique(sort(x[!is.na(x)])), collapse= ", ")}

combine_rows_based_on_multiple_columns_and_keep_all_unique_sorted_values <- function(my_df, group_by_arr) {
  my_df %>%
    group_by_at(group_by_arr) %>%
    summarise_all(concat_unique_sorted) %>%
    return()
}

## usage:
# my_paths <- set_work_dir()
#
## get csv data into variables
# temp_var <- get_compl_and_corresp_data(my_paths)
# compl_clean <- temp_var[[1]]
# corresp_clean <- temp_var[[2]]

csv_names_list_22_23 = c("Correspondence.csv",
                         "FHIER_Compliance_22.csv",
                         "FHIER_Compliance_23.csv")

# add my additional folder names to each filename
prepare_csv_names <- function(filenames) {
  add_path_corresp <- "Correspondence"
  add_path_compl <- "FHIER Compliance"

  my_list <- sapply(filenames, function(x) {
    case_when(startsWith(my_headers_case_function(x), "correspond") ~
                file.path(add_path_corresp,  x),
              startsWith(my_headers_case_function(x), "fhier_compliance") ~
                file.path(add_path_compl,  x),
              .default = file.path(add_path_compl,  x)
    )
  } )
  paste(my_list) %>% as.list() %>% return()
}

get_compl_and_corresp_data <- function(my_paths, filenames = csv_names_list_22_23, vessel_id_field_name = NA) {
  # browser()
  # add my additional folder names
  csv_names_list <- prepare_csv_names(filenames)
  # read all csv files
  csv_contents <- load_csv_names(my_paths, csv_names_list)
# browser()
  # unify headers, trim vesselofficialnumber, just in case
  csvs_clean1 <- clean_all_csvs(csv_contents, vessel_id_field_name)

  # ---- specific correspondence manipulations ----
  corresp_arr_contact_cnts_clean <- corresp_cleaning(csvs_clean1)

  ## ---- specific compliance manipulations ----
  compl_arr <- csvs_clean1[2:length(csvs_clean1)]

  compl_clean <- compliance_cleaning(compl_arr)
  return(list(compl_clean, corresp_arr_contact_cnts_clean))
}

# ---- specific correspondence manipulations ----
corresp_cleaning <- function(csvs_clean1){
  corresp_arr <- csvs_clean1[[1]]
  # add a new column with a "yes" if there is a contactdate (and a "no" if not),
  # group by vesselofficialnumber and count how many "contacts" are there for each. Save in the "contact_freq" column.
  # browser()
  corresp_arr_contact_cnts <- add_count_contacts(corresp_arr)
  createdon_field_name <- find_col_name(corresp_arr, "created", "on")[1]
  contactdate_field_name <- find_col_name(corresp_arr, "contact", "date")[1]
  # change classes from char to POSIXct
  corresp_arr_contact_cnts %>%
    change_to_dates(createdon_field_name, "%m/%d/%Y %H:%M") %>%
    change_to_dates(contactdate_field_name, "%m/%d/%Y %I:%M %p") ->
    corresp_arr_contact_cnts_clean

  return(corresp_arr_contact_cnts_clean)
}

## ---- specific compliance manipulations ----
compliance_cleaning <- function(compl_arr){
  # if it is one df already, do nothing
  compl <- compl_arr
  # else combine separate dataframes for all years into one
  if (!length(compl_arr) == 1) {
    compl <- join_same_kind_csvs(compl_arr)
  }

  permitgroupexpiration <- grep("permit.*group.*expiration",
                           tolower(names(compl)),
                           value = T)

  compl %>%
    # split week column (52: 12/26/2022 - 01/01/2023) into 3 columns with proper classes, week_num (week order number), week_start and week_end
    clean_weeks() %>%
    # change dates classes from char to POSIXct
    change_to_dates(permitgroupexpiration, "%m/%d/%Y") %>%
    return()
}

# read csv file with EOF within quoted strings
read_csv_w_eofs <- function(my_paths, csv_names_list) {
  my_inputs <- my_paths$inputs
  # add input directory path in front of each file name.
  myfiles <- sapply(csv_names_list, function(x) file.path(my_inputs, add_csv_path, x))

  # read csv files
  contents <- sapply(myfiles, fread, header = TRUE)
  # convert the first one into a data frame
  # TODO change this function to deal with multiple files
  contents[, 1] %>%
    as.data.frame() %>%
    return()
}

# To use as a filter in FHIER
cat_filter_for_fhier <- function(my_characters) {
  cat(my_characters,
      sep = ', ',
      file = file.path(my_paths$outputs,
                       "cat_out.txt"))
}

#
# benchmarking to insert inside a function
# browser()
# time_for_appl <<- benchmark(replications=rep(10, 3),
                            # lapply(myfiles, read.csv, skipNul = TRUE, header = TRUE),
                            # sapply(myfiles, read.csv, skipNul = TRUE, header = TRUE, simplify = TRUE)
                            # ,
                            # columns = c('test', 'elapsed', 'relative')
# )

# write.csv(time_for_appl, "time_for_appl.csv")

# or
# browser()
# sappl_exp <- function(){
#   sapply(my_df, function(x) length(unique(x))) %>% as.data.frame()
# }
#
# map_exp <- function(){
#   my_fun <- function(x) length(unique(x))
#   map_df(my_df, my_fun)
# }
#
# time_for_appl <<- benchmark(replications=rep(10^7, 3),
#                             exp1,
#                             exp2,
#                             columns = c('test', 'elapsed', 'relative')
# )
#
# map_df(my_df, function(x) length(unique(x)))
# to compare:
# time_for_appl %>% group_by(test) %>% summarise(sum(elapsed))

connect_to_secpr <- function() {
  # usage:
  # con <- connect_to_secpr()
  my_username <- keyring::key_list("SECPR")[1, 2]
  con = dbConnect(
    dbDriver("Oracle"),
    username = my_username,
    password = keyring::key_get("SECPR",
                                my_username),
    dbname = "SECPR"
  )
  return(con)
}

# usage: complianceerrors_field_name <- find_col_name(compl_clean_sa, ".*xcompliance", "errors.*")[1]
# TODO what if two names?
find_col_name <- function(mydf, start_part, end_part) {
  to_search <- paste0(start_part, ".*", end_part)
  grep(to_search,
       tolower(names(mydf)),
       value = T)
}

# https://stackoverflow.com/questions/23986140/how-to-call-exists-without-quotation-marks
# usage: vexists(con_psql, bogus_variable_name)
vexists <- function(...) {
  vars <- as.character(substitute(...()))
  sapply(vars, exists)
}

# make a separate legend for grid.arrange
legend_for_grid_arrange <- function(legend_plot) {
  # legend_plot <-
  #   ggplot(data = legend_data, aes(x1, y1, colour = ll)) +
  #   geom_text(dat = legend_data,
  #             aes(label = ll),
  #             hjust = 0) +
  #   scale_color_manual(
  #     name = 'Lines',
  #     breaks = c('Mean', 'Num of weeks'),
  #     values = my_colors
  #   )
  #
  # legend_plot

  my_legend <-
    cowplot::get_legend(legend_plot)

  return(my_legend)
}

make_a_flat_file <- 
  function(flat_file_name,
           files_to_combine_list) {
    # browser()
    # write to file
    sink(flat_file_name)
    
    for (i in 1:length(files_to_combine_list)) {
      current_file = readLines(files_to_combine_list[i])
      cat("\n\n#### Current file:", files_to_combine_list[i], "\n\n")
      cat(current_file, sep = "\n")
    }
    
    sink()
  }


#### Current file: C:/Users/anna.shipunova/Documents/R_code_github/fishing_effort_location/read.me.R ----

# https://github.com/AShipunova1/R_code/tree/main/fishing_effort_location

# All of 2022 would work for us
# Trips filtered to charter trips only.
# fishing charter trips
# Trip start and end date
# Start Port: Code, Name, County and/or State, State Code, Postal Code (all separate fields):  all
# End Port: Code,
# Longitude and Latitude fields:  Lat and long as specific as possible
# Fishing Area Code, Sub Area Code, Distance Code and Distance Code Name 
# ---

# I was wondering if it would be possible to get access to the for hire reports.  We are working on a management strategy evaluation and need information on location of relative fishing effort.  The relative would be looking by depth, area, and seasonally. 
# 
# I got approval to send you the data, per your request. To ensure I get what you need, can you please respond to the following:
# 
# You are looking for just depth, area fished (lats/longs?) and dates of those trips (start and end fields), correct? 
# The lawyer deemed these non-confidential data fields, but just making sure you weren't expecting anything else.  No problem.  I do have access to confidential data through the SEFSC and ACCSP, and adrain has been approved by the SEFSC as well.  Are there additional steps I need to take for the future to be able to access landings data?  I don't think so, but we can cross that bridge when needed.  
# Do you want just for-hire intended fishing trips that have recorded an "area fished" on their trip report that falls within SA waters?
# Are there any lat/long or state limits to apply? 
# I think filter out beyond state waters for trips north of 28N.  All charter trips south of 28N to the SAFMC/GMFMC boundary.  ok
# 
# The other possibilities are instead sending any for-hire trip taken by a SA federal for-hire permitted vessel (either for just SA or dual Gulf/SA permitted vessels), and/or filtering by start and/or end port of the trip. 
# Is there a date range? 
# I would not suggest using 2021 data, given the program was so new. We tend to run any analyses from Jan 1, 2022+
# All of 2022 would work for us ok
# 
# What is the deadline that you would need this data request fulfilled by?
# June 1 ok
# 
# We collect 3 depth fields in the SEFHIER logbook: min and max depth, and primary fishing depth. 
# Do you want all 3 fields, or one in particular?
# Primary depth is fine.  However if the other fields are more commonly filled out than primary depth, please provide those fields instead.  ok
# 
# We also have lat/long data fields, collected in the logbook. A summary of logbook fields of potential interest are here:
# Trip type (commercial, charter, recreational, headboat) - these would be for-hire permitted vessels, telling us which trip type they intend to make:  Trips filtered to charter trips only.  We have access to commercial and headboat  ok (here headboat is a for-hire vessel operating as a headboat, but that is not in the SRHS program). So, I will include charter and headboat (which excludes SRHS data)
# Trip start and trip end date and time fields  :  Trip start and end date  ok
# Start Port: Code, Name, County and/or State, State Code, Postal Code (all separate fields):  all  ok
# End Port: Code, Name, County and/or State, State Code, Postal Code (all separate fields):  code only  ok
# Trip Activity Type: options are trips with effort; intended fishing but unable to fish (e.g. weather); trip with no intention of fishing (e.g. chartered sunset or dolphin cruise): fishing charter trips  ok
# Longitude and Latitude fields:  Lat and long as specific as possible  ok
# Fishing Area Code, Sub Area Code, Distance Code and Distance Code Name (these are ACCSP fields, so I or you would need to get the code definitions) all.  I have access in ACCSP  ok
#  
# 
# **Some data caveats to be aware of:
# 
# this is preliminary SEFHIER reported data, and the SEFHIER data is not yet calibrated or certified.  This information will only be used to consider relative effort.  Are there issues with potential bias by area?  This was more of a note, to just say please be careful when using it for relative effort as this may not be representative.  We have not looked into what the biases are, but based alone on the lack of reporting it just may not be representative
# to-date; we still have only about 60% compliance month to month in the SA, for vessels reporting as required. Therefore, interpretation of the data should take that into consideration
# SRHS data are not included in the SEFHIER data (I do not have access to that program's data).  I can access this data   ok
# SEFHIER logbooks don’t collect depth at the set level. So, a species (or complex level) analysis probably wouldn’t be very useful given how the fishery operates. We understand.  It is a hard one to define especially with some of the captains drifting instead of anchoring.  Average for the trip is fine.      ok
# We collect “average” gear depth (essentially fishing depth for the whole trip, where a trip could be across many depths - depending how and where they moved for that trip, and they could have targeted many species throughout the trip- depending on weather and what was biting). 
# We also collect min and max depth in the logbooks, but again those are trip level fields
#  
# 

# more setup ----
my_paths <- set_work_dir()

#### Current file: C:/Users/anna.shipunova/Documents/R_code_github/fishing_effort_location/fishing_effort_locations_get_data.R ----

con = dbConnect(
  dbDriver("Oracle"),
  username = keyring::key_list("SECPR")[1, 2],
  password = keyring::key_get("SECPR", keyring::key_list("SECPR")[1, 2]),
  dbname = "SECPR"
)

## From DB ====
# fishing charter trips only
# 2022
# sero_vessel_permit

request_query <- "SELECT
  trip_start_date,
  trip_end_date,
  start_port,
  start_port_name,
  start_port_county,
  start_port_state,
  end_port,
  vendor_app_name,
  area_code,
  sub_area_code,
  distance_code_name,
  local_area_code,
  latitude,
  longitude,
  minimum_bottom_depth,
  maximum_bottom_depth,
  fishing_gear_depth,
  depth
FROM
  srh.mv_safis_trip_download@secapxdv_dblk.sfsc.noaa.gov
WHERE
    trip_de >= TO_DATE('01-JAN-22', 'dd-mon-yy')
  AND TRIP_START_DATE >= TO_DATE('01-JAN-22', 'dd-mon-yy')
  AND TRIP_END_DATE <= TO_DATE('31-DEC-22', 'dd-mon-yy')
  AND trip_type_name = 'CHARTER'
  AND sero_vessel_permit IS NOT NULL"

db_data = dbGetQuery(con,
                     request_query)


# get area data ----
area_data_query <- 
  "select * from SAFIS.AREAS_FISHED@secapxdv_dblk.sfsc.noaa.gov 
  where state in ('FL', 'US')
"

db_area_data = dbGetQuery(con,
                     area_data_query)

dbDisconnect(con)

## ---- get other geographical data ----
read_shapefile <- function(filename) {
  shapefile_file_name <- file.path(my_paths$inputs, "shapefiles", filename)

  x <- sf::read_sf(shapefile_file_name)
  return(x)
}

# https://www.fisheries.noaa.gov/resource/map/defined-fishery-management-areas-south-atlantic-states-map-gis-data

sa_shp <- read_shapefile(r"(sa_eaz_off_states\shapefiles_sa_eez_off_states\SA_EEZ_off_states.shp)"
)

gom_reef_shp <- read_shapefile(r"(gom\ReefFish_EFH_GOM\ReefFish_EFH_GOM.shp)")

### atl_state_waters ----
# https://catalog.data.gov/dataset/outer-continental-shelf-submerged-lands-act-boundary-atlantic-region-nad83
# Outer Continental Shelf Submerged Lands Act Boundary - Atlantic Region NAD83

atl_state_waters <- read_shapefile("ATL_SLA/ATL_SLA.shp")
# mapview(atl_state_waters)

### fl_state_w_counties ----

fl_state_w_counties <- read_shapefile(r"(GOVTUNIT_Florida_State_Shape\Shape\GU_CountyOrEquivalent.shp)")

# mapview(fl_state_w_counties)

#### Current file: C:/Users/anna.shipunova/Documents/R_code_github/fishing_effort_location/fishing_effort_location.R ----

# information on location of relative fishing effort.  The relative would be looking by depth, area, and seasonally. 
# filter out beyond state waters for trips north of 28N.  All charter trips south of 28N to the SAFMC/GMFMC boundary. 
# --- OK boundaries
# lat 23 : 36
# lon -71 : -98
# SAFMC/GMFMC boundary
# see https://myfwc.com/fishing/saltwater/recreational/maps/
# 83 west (federal waters) / 24'35 N, 24'33 N (state waters)

# to get SA only:
# filter out beyond state waters for trips north of 28N.  All charter trips south of 28N to the SAFMC/GMFMC boundary. 
# ? how to get the boundary?

# fields to get 
# Trip start and end date
# Start Port: Code, Name, County and/or State, State Code, Postal Code - no
# End Port: Code,
# Longitude and Latitude fields:  Lat and long as specific as possible
# Fishing Area Code, Sub Area Code, Distance Code and Distance Code Name?
# depth

# OK boundaries ----
# lat 23 : 28
# lon -71 : -83

clean_lat_long <- function(my_lat_long_df, my_limit = 1000) {
  my_lat_long_df %>%
    unique() %>%
    head(my_limit) %>%
    # all LONG should be negative
    mutate(LONGITUDE = -abs(LONGITUDE)) %>%
    # remove wrong coords
    filter(between(LATITUDE, 23, 28) &
             between(LONGITUDE, -83, -71)) %>%
    return()
}

# combine with additional area data ----
db_data_w_area <- full_join(db_area_data, db_data)
# Joining with `by = join_by(AREA_CODE, SUB_AREA_CODE,
# LOCAL_AREA_CODE)`

all_points <- dim(db_data_w_area)[1]
# 254689

# View(db_data_w_area)

to_report <-
  db_data_w_area %>%
  select(
    TRIP_START_DATE,
    TRIP_END_DATE,
    START_PORT,
    START_PORT_NAME,
    START_PORT_COUNTY,
    START_PORT_STATE,
    END_PORT,
    LATITUDE,
    LONGITUDE,
    MINIMUM_BOTTOM_DEPTH,
    MAXIMUM_BOTTOM_DEPTH,
    FISHING_GEAR_DEPTH,
    DEPTH
  ) %>%
  unique() #?

dim(to_report)
# 75546

#### Current file: C:/Users/anna.shipunova/Documents/R_code_github/fishing_effort_location/fishing_effort_location_viz.R ----

# shape files maps ----
m_s <- mapview(sa_shp,
               layer.name = "South Altlantic",
               legend = FALSE)

# OK boundaries ----
# lat 23 : 28
# lon -71 : -83

clean_lat_long <- function(my_lat_long_df, my_limit = 1000) {
  my_lat_long_df %>%
    unique() %>%
    head(my_limit) %>%
    # all LONG should be negative
    mutate(LONGITUDE = -abs(LONGITUDE)) %>%
    # remove wrong coords
    filter(between(LATITUDE, 23, 28) &
             between(LONGITUDE, -83, -71)) %>%
    return()
}

to_sf <- function(my_df) {
  my_df %>%
    st_as_sf(coords = c("LONGITUDE",
                        "LATITUDE"),
             crs = st_crs(sa_shp)) %>%
    return()
}

## no gom by month ----

lat_long_month_depth <-
  db_data_w_area %>%
  # labels are a month only
  mutate(TRIP_START_M =
           format(TRIP_START_DATE, "%m")) %>%
  # compute on a data frame a row-at-a-time
  rowwise() %>%
  # get avg bottom depth
  mutate(AVG_DEPTH = mean(
    c(
      MINIMUM_BOTTOM_DEPTH,
      MAXIMUM_BOTTOM_DEPTH,
      FISHING_GEAR_DEPTH
    ),
    na.rm = T
  )) %>%
  ungroup() %>%
  mutate(
    POINT = paste(
      LATITUDE,
      LONGITUDE,
      TRIP_START_M,
      AVG_DEPTH,
      AREA_NAME,
      START_PORT_NAME,
      sep = ", "
    )) %>%
  filter(!REGION %in% c("GULF OF MEXICO"))

all_points <- dim(lat_long_month_depth)[1]
# 254503

lat_long_month_depth_clean <- clean_lat_long(lat_long_month_depth, all_points)
dim(lat_long_month_depth_clean)[1]
# 28032
# %>%
#   to_sf(lat_long_month_depth_clean)

names(lat_long_month_depth_minus_gom)

lat_long_month_depth_minus_gom_sf <- st_difference(to_sf(lat_long_month_depth_clean), gom_reef_shp)

mapviewOptions(fgb = FALSE)
lat_long_month_no_gom_map <-
  lat_long_month_depth_minus_gom_sf %>%
  mapview(
    # colors
    zcol = "TRIP_START_M",
    # color palette
    col.regions = viridisLite::turbo,
    layer.name = 'Month',
    # size
    cex = "AVG_DEPTH",
    # transparency
    alpha = 0.3,
    legend = TRUE
  )

res_map <- lat_long_month_no_gom_map + m_s

# install.packages("webshot2")
# library(webshot2)
# mapviewOptions(fgb = FALSE)
# remotes::install_github("r-spatial/mapview")
png_fl <- "res_map.png"
mapview::mapshot(res_map, file = png_fl)
# open the file
browseURL(png_fl)

## clusters ----
lat_long_area_for_leaflet <-
  clean_lat_long(lat_long_area, all_points) %>%
  mutate(
    POINT = paste(
      LATITUDE,
      LONGITUDE,
      TRIP_START_M,
      AREA_NAME,
      SUB_AREA_NAME,
      AREA_CODE,
      DISTANCE_CODE_NAME,
      sep = ", "
    )
  ) 

lat_long_area_leaflet_w_clusters <-
  lat_long_area_for_leaflet %>%
  leaflet() %>%
  addTiles() %>%
  addMarkers(
    label = paste(
      lat_long_area_for_leaflet$LATITUDE,
      lat_long_area_for_leaflet$LONGITUDE,
      lat_long_area_for_leaflet$AREA_NAME,
      sep = ", "
    ),
    # lat_long_area_clean$POINT,
    # labelOptions = labelOptions(noHide = T),
    clusterOptions = markerClusterOptions()
  )

# doesn't show clusters
# lat_long_area_for_leaflet %>%
#   to_sf() %>%
# mapview(clusterOptions = markerClusterOptions())
# 
# + 
#   m_s

lat_long_area_leaflet_w_clusters
## check different area options ----
lat_long_area_clean %>%
  select(AREA_NAME,
         SUB_AREA_NAME,
         AREA_CODE,
         DISTANCE_CODE_NAME) %>%
  unique() %>% 
  arrange(AREA_CODE) %>% 
  glimpse()
# Rows: 85

lat_long_area_clean %>%
  select(AREA_NAME,
         SUB_AREA_NAME,
         AREA_CODE) %>%
  unique() %>% 
  arrange(AREA_CODE) %>% 
  View()
# Rows: 47

lat_long_area_clean %>%
  select(AREA_NAME,
         AREA_CODE) %>%
  unique() %>% 
  arrange(AREA_CODE) %>% 
  View()
# 26

lat_long_area_clean %>%
  select(AREA_NAME,
         AREA_CODE) %>%
  filter(!grepl("GULF OF MEXICO", AREA_NAME)) %>% 
  filter(!grepl("TAMPA", AREA_NAME)) %>% 
  filter(!grepl("FORT MYERS", AREA_NAME)) %>% 
  unique() %>% 
  arrange(AREA_CODE) %>% 
  View()
  # write_csv("area_code_name.csv")

# View(db_data)

# separate SA only ----
# see v_safis_trip_download
"SELECT
  distinct region
FROM
safis.areas_fished@secapxdv_dblk.sfsc.noaa.gov"
# null, unknown?
# ===
# SOUTH ATLANTIC
# SOUTH ATLANIC
# MID ATLANTIC
# CARIBBEAN
#  
# 
# GULF OF MEXICO
# SOUTH ATLANTIC 
# NEW ENGLAND
# RHODE ISLAND
# UNKNOWN
