#' %%%%% Prepare data
#'
#' Questions:
#' How many SEFHIER vessels have a different start port county than end port county?
#'   Numbers, by quarter (1-4):
#' How many SEFHIER vessels have a different start port state than end port state?
#'   Numbers, by quarter (1-4):
#' How many SEFHIER vessels have a different start port region (Gulf) than end port region (South Atlantic)?
#' Numbers, by quarter (1-4):
#' How many Gulf permitted SEFHIER vessels fish in both the Gulf and South Atlantic?
#'   Numbers, by quarter (1-4):
#' For counties and states
#' GOM:
#' permit,
#' home_port,
#' end_port
#' retain Monroe
#'
#' For home port region to SA region:
#' GOM permit,
#' GOM home_port
#' exclude Monroe

#' For fishing region to region
#' GOM permit
#' retain Monroe
#' Create 2 dfs fished in GOM or in SA using lat and lon for area fished
#' grouping by vessel ID and quarter, check if unique vessel fishing in GOM and in SA

# setup current project ----
source("~/R_code_github/boats_number/boats_number_sources.R")

## processed_logbooks ----

# names(processed_logbooks_clean_names) |>
#   sort() |>
#   cat(sep = ", ")

# to use in lists
start_end_words <-
  c("start", "end")

port_fields_short <-
  c(
    "vessel_official_number",
    "end_port_name",
    "end_port_state",
    "end_port_county",
    "end_port",
    "permit_region",
    "start_port_name",
    "start_port_state",
    "start_port_county",
    "start_port",
    "trip_id",
    "trip_end_date",
    "trip_start_date",
    "latitude",
    "longitude"
  )

# grep("state", names(processed_logbooks_clean_names), value = T, ignore.case = T)

# Explanation:
# 1. The pipe operator (`|>`) is used to pass the data frame 'processed_logbooks_clean_names' to the next operation, making the code more readable.
# 2. The 'dplyr::select' function is employed to choose specific columns from the data frame. The columns to be selected are determined by the vector 'port_fields_short', which contains column names.
# 3. The 'remove_empty_cols()' function is a custom function (F2 - see the definition) that removes columns containing only missing values (NA).
# 4. The next pipe (`|>`) continues the data flow, passing the modified data frame to the 'dplyr::distinct()' function.
# 5. The 'dplyr::distinct()' function ensures that only unique rows are retained in the data frame, removing any duplicate rows based on all columns.
#
# In summary, this code snippet processes a data frame by selecting specific columns, removing empty columns, and keeping only unique rows. The result is stored in the 'processed_logbooks_short' data frame.

processed_logbooks_short <-
  processed_logbooks_clean_names |>
  dplyr::select(dplyr::all_of(port_fields_short)) |>
  remove_empty_cols() |>
  dplyr::distinct()

dim(processed_logbooks_short)
# [1] 3011    7
# [1] 2475    6 (no overridden)
# [1] 66641     8 with start and end dates
# [1] 72246    14 with lat/long
# [1] 73368    15 with trip_id

### add date related columns ----

# Explanation:
# 1. The 'processed_logbooks_short' data frame is piped into the 'dplyr::mutate' function, which is used to create new columns based on transformations.
# 2. The 'trip_start_week_num' and 'trip_end_week_num' columns are created using the 'strftime' function to extract the week number (1-7, Monday-Sunday) from the 'trip_start_date' and 'trip_end_date'.
# 3. 'trip_start_y' and 'trip_end_y' columns are created, representing the year of 'trip_start_date' and 'trip_end_date', respectively, using the 'lubridate::year' function.
# 4. 'trip_start_m' and 'trip_end_m' columns are created, representing the year and month (as a decimal) of 'trip_start_date' and 'trip_end_date' using the 'zoo::as.yearmon' function.
# 5. 'trip_start_year_quarter' and 'trip_end_year_quarter' columns are created, representing the year and quarter of 'trip_start_date' and 'trip_end_date' using the 'zoo::as.yearqtr' function.
# 6. 'trip_start_quarter_num' and 'trip_end_quarter_num' columns are created, representing the quarter number (1-4) of 'trip_start_date' and 'trip_end_date' using the 'format' function and the '%q' format specifier.

tic("processed_logbooks_short_dates")
processed_logbooks_short_dates <-
  processed_logbooks_short |>
  dplyr::mutate(
    trip_start_week_num =
      strftime(trip_start_date, format = "%u"),
    trip_end_week_num =
      strftime(trip_end_date, format = "%u"),
    trip_start_y =
      lubridate::year(trip_start_date),
    trip_end_y =
      lubridate::year(trip_end_date),
    trip_start_m =
      zoo::as.yearmon(trip_start_date),
    trip_end_m =
      zoo::as.yearmon(trip_end_date),
    trip_start_year_quarter = zoo::as.yearqtr(trip_start_date),
    trip_start_quarter_num =
      format(trip_start_year_quarter, "%q"),
    trip_end_year_quarter = zoo::as.yearqtr(trip_end_date),
    trip_end_quarter_num =
      format(trip_end_year_quarter, "%q")
  )
toc()
# processed_logbooks_short_dates: 2.94 sec elapsed
# processed_logbooks_short_dates: 4.28 sec elapsed

## Prepare home_port data ----
# all_get_db_data_result_l |>
#   print_df_names()

# all_get_db_data_result_l$vessels_permits |>
  # print_df_names()

vessel_permit_port_info <-
  all_get_db_data_result_l$vessels_permits |>
  # active permits in 2022
  dplyr::filter(
    LAST_EXPIRATION_DATE > "2022-12-31" |
      END_DATE > "2022-12-31" |
      EXPIRATION_DATE > "2022-12-31"
  )

dim(vessel_permit_port_info)
# [1] 68113    51
# SERO_OFFICIAL_NUMBER  5220
# SERO_HOME_PORT_CITY    809
# SERO_HOME_PORT_COUNTY   320
# SERO_HOME_PORT_STATE     28

### remove unused columns ----

vessel_permit_port_info_short <-
  vessel_permit_port_info |>
  select(
    PERMIT_VESSEL_ID,
    VESSEL_VESSEL_ID,
    # PORT_CODE, mostly empty
    SERO_HOME_PORT_CITY,
    SERO_HOME_PORT_COUNTY,
    SERO_HOME_PORT_STATE,
    SERO_OFFICIAL_NUMBER
  ) |>
  dplyr::ungroup() |>
  remove_empty_cols() |>
  dplyr::distinct()

n_distinct(vessel_permit_port_info_short$VESSEL_VESSEL_ID)
# VESSEL_VESSEL_ID      5220

vessel_permit_port_info_short_clean <-
  clean_headers(vessel_permit_port_info_short)

## add vessel_permit home port information to trip (logbook) information ----

# Explanation:
# 1. The 'dplyr::left_join' function is applied to combine two data frames, namely 'processed_logbooks_short_dates' and 'vessel_permit_port_info_short_clean', using a left join.
# 2. The third argument of the 'left_join' function, 'dplyr::join_by', specifies the columns used for the join operation. In this case, it indicates that the column 'vessel_official_number' from the left data frame should be matched with the column 'sero_official_number' from the right data frame.
# 3. The result of the join is stored in the 'join_trip_and_vessel' data frame, which includes columns from both input data frames, combining information about trips and vessel permits. The left join ensures that all rows from 'processed_logbooks_short_dates' are retained, and matching rows from 'vessel_permit_port_info_short_clean' are added based on the specified join condition.

join_trip_and_vessel <-
  dplyr::left_join(
    processed_logbooks_short_dates,
    vessel_permit_port_info_short_clean,
    dplyr::join_by(vessel_official_number ==
                     sero_official_number)
  )

dim(join_trip_and_vessel)
# [1] 3011   20
# [1] 2475   19 (processed logbooks)
# [1] 66641    35
# [1] 72246    29 with lat/long
# [1] 73368    30 with trip_id

# with overridden
# vessel_id             1876
# vessel_official_number   1876
# permit_sa_gom            4
# SERO_OFFICIAL_NUMBER  1785

# w/o overridden
# vessel_official_number 1629
# permit_region             2
# PERMIT_VESSEL_ID        1562
# VESSEL_VESSEL_ID        1562
# SERO_HOME_PORT_CITY      361
# SERO_HOME_PORT_COUNTY    145
# SERO_HOME_PORT_STATE      20
# latitude                52887
# longitude               53423

## remove trailing spaces ----

# Explanation:
# 1. The 'join_trip_and_vessel' data frame is piped into the 'mutate_if' function, which allows selective modification of columns based on a specified condition.
# 2. The 'mutate_if' function is used to apply the 'str_trim' function to all character columns of the 'join_trip_and_vessel' data frame.
# 3. The condition 'is.character' inside 'mutate_if' ensures that the trimming operation is only performed on columns containing character data.
# 4. The result is stored in the 'join_trip_and_vessel_trim' data frame, where leading and trailing whitespaces are removed from all character columns. This operation helps clean and standardize the textual data in those columns.

join_trip_and_vessel_trim <-
  join_trip_and_vessel |>
  mutate_if(is.character, str_trim)

diffdf::diffdf(join_trip_and_vessel,
               join_trip_and_vessel_trim)

## lower case of all data ----

# Explanation:
# 1. The 'join_trip_and_vessel_trim' data frame is piped into the 'mutate_if' function, which allows selective modification of columns based on a specified condition.
# 2. The 'mutate_if' function is used to apply the 'tolower' function to all character columns of the 'join_trip_and_vessel_trim' data frame.
# 3. The condition 'is.character' inside 'mutate_if' ensures that the conversion to lowercase is only performed on columns containing character data.
# 4. The result is stored in the 'join_trip_and_vessel_low' data frame, where all character columns are converted to lowercase. This operation helps standardize the case of text data for consistency and ease of analysis.
join_trip_and_vessel_low <-
  join_trip_and_vessel_trim |>
  mutate_if(is.character, tolower)

## remove not a-z in strings ----
# (e.g. "St. John" or "miami dade" vs. "miami-dade")

# Explanation:
# 1. The 'join_trip_and_vessel_low' data frame is piped into the 'mutate_if' function, allowing selective modification of columns based on a specified condition.
# 2. The 'mutate_if' function is used to apply the 'str_replace_all' function to all character columns of the 'join_trip_and_vessel_low' data frame.
# 3. The condition 'is.character' inside 'mutate_if' ensures that the replacement is only performed on columns containing character data.
# 4. The replacement pattern "[^a-z0-9]+" in 'str_replace_all' uses a regular expression to match any non-alphanumeric characters and replaces them with a single space.
# 5. The result is stored in the 'join_trip_and_vessel_clean' data frame, where non-alphanumeric characters in character columns are replaced with spaces. This operation helps clean and standardize the textual data for further analysis.
join_trip_and_vessel_clean <-
  join_trip_and_vessel_low |>
  mutate_if(is.character,
         ~str_replace_all(., "[^a-z0-9]+", " "))

# diffdf::diffdf(join_trip_and_vessel_low,
#                join_trip_and_vessel_clean)

# An aux function to add regions to states ----

# join_trip_and_vessel_clean |>
#   select(start_port_state) |>
#   distinct()

# Explanation:
# 1. The function takes a data frame (`my_df`) and a specifier (`start_or_end`) indicating whether to consider the start or end of a trip.
# 2. Dynamic column names are created using the 'str_glue' function for the result column and columns related to state and county based on the 'start_or_end' parameter.
# 3. Quosures (symbolic expressions) are created to check conditions related to Florida state, Gulf of Mexico region, and Gulf of Mexico counties in Florida.
# 4. The function then uses 'rowwise' and 'mutate' functions to add a new column to the data frame ('new_df'). The new column values are determined based on conditional logic using the 'case_when' function.
# 5. The conditions check if the state is not Florida and is in the Gulf of Mexico region, or if the state is Florida and the county is in the Gulf of Mexico region. The default case sets the result column to "sa" (South Atlantic) if the state is not Florida or the county is not in the Gulf of Mexico region.
# 6. The 'ungroup' function is used to remove the grouping introduced by 'rowwise'.
# 7. The modified data frame is returned.
add_region_to_state <-
  function(my_df, start_or_end) {
    result_column_name <-
      str_glue("{start_or_end}_state_region")

    port_state_column <-
      sym(str_glue("{start_or_end}_port_state"))

    port_county_column <-
      sym(str_glue("{start_or_end}_port_county"))

    is_st_florida <-
      rlang::quo(!!port_state_column == "fl")

    is_gom_state <-
      rlang::quo(my_state_name[[!!port_state_column]]
                 %in% east_coast_states$gom)

    is_gom_fl_county <-
      rlang::quo(
        !!port_county_column %in% tolower(fl_counties$gom) |
          !!port_county_column %in% tolower(fl_counties$gom_interior)
      )

    new_df <-
      my_df |>
      rowwise() |>
      mutate(
        !!result_column_name :=
          case_when(
            !(!!is_st_florida) & !!is_gom_state ~ "gom",

            !!is_st_florida & !!is_gom_fl_county ~ "gom",

            is.na(!!port_state_column) ~ NA,

            .default = "sa"
          )
      ) |>
      ungroup()

    return(new_df)
  }

# TODO: Why there is no home port?
join_trip_and_vessel_clean |>
  filter(is.na(sero_home_port_state)) |>
  select(contains("port")) |>
  distinct() |>
  glimpse()


no_home_port_vessels <-
  join_trip_and_vessel_clean |>
  filter(is.na(sero_home_port_state)) |>
  select(vessel_official_number) |>
  distinct()

n_distinct(no_home_port_vessels$vessel_official_number)
# 68

all_get_db_data_result_l$vessels_permits |>
  # active permits in 2022
  dplyr::filter(
    LAST_EXPIRATION_DATE > "2022-12-31" |
      END_DATE > "2022-12-31" |
      EXPIRATION_DATE > "2022-12-31"
  ) |>
  filter(SERO_OFFICIAL_NUMBER %in% no_home_port_vessels$vessel_official_number) |>
  nrow()
# 0

all_get_db_data_result_l$vessels_permits |>
  filter(SERO_OFFICIAL_NUMBER %in% no_home_port_vessels$vessel_official_number) |>
  select(SERO_OFFICIAL_NUMBER,
         END_DATE,
         EXPIRATION_DATE,
         LAST_EXPIRATION_DATE,
         SERO_HOME_PORT_STATE) |> distinct() |>
  arrange(SERO_OFFICIAL_NUMBER,
          END_DATE,
          EXPIRATION_DATE,
          LAST_EXPIRATION_DATE) |>
    dplyr::filter(
    LAST_EXPIRATION_DATE > "2021-12-31" |
      END_DATE > "2021-12-31" |
      EXPIRATION_DATE > "2021-12-31"
  ) |>
  group_by(SERO_OFFICIAL_NUMBER) |>
  count(name = 'date_by_vsl') |>
  ungroup() |>
  nrow()
# 25
# 0 with the dates as "2022-12-31"


# Add port state regions ----
# Don't use a start port state instead of filter(!is.na(sero_home_port_state)) for consistency.

# Explanation:
# 1. The 'join_trip_and_vessel_clean' data frame is piped into the 'filter' function to exclude rows where the 'sero_home_port_state' column is NA.
# 2. The 'add_region_to_state' function is applied twice using the pipe operator ('|>') to add region information for both the start and end of the trip.
# 3. The 'add_region_to_state("sero_home")' line adds region information for the start of the trip using the 'sero_home' prefix.
# 4. The 'add_region_to_state("end")' line adds region information for the end of the trip using the 'end' prefix.
# 5. The result is stored in the 'join_trip_and_vessel_clean_state_regions' data frame, which now includes additional columns indicating the regions for both the start and end of the trip.
# 6. The 'tic' and 'toc' functions are used to measure the time taken for the entire operation between the two calls. Timing is enclosed in the 'join_trip_and_vessel_clean_state_regions' operation to measure its execution time.
tic("join_trip_and_vessel_clean_state_regions")
join_trip_and_vessel_clean_state_regions <-
  join_trip_and_vessel_clean |>
  filter(!is.na(sero_home_port_state)) |>
  add_region_to_state("sero_home") |>
  add_region_to_state("end")
toc()
# join_trip_and_vessel_clean_state_regions: 33.63 sec elapsed

# Split by home port regions ----

# Explanation:
# 1. The 'join_trip_and_vessel_clean_state_regions' data frame is piped into the 'split' function.
# 2. The 'split' function is applied to create a list of data frames. The splitting is based on the levels of the 'sero_home_state_region' column.
# 3. The 'as.factor' function is used to ensure that the column is treated as a factor, and it provides the levels for splitting.
# 4. The result is stored in the 'join_trip_and_vessel_clean_state_regions_l' variable, which is a list of data frames. Each data frame in the list corresponds to a unique level of the 'sero_home_state_region' column.

join_trip_and_vessel_clean_state_regions_l <-
  join_trip_and_vessel_clean_state_regions |>
  split(as.factor(
    join_trip_and_vessel_clean_state_regions$sero_home_state_region
  ))

# check
map(join_trip_and_vessel_clean_state_regions_l,
    count_uniq_by_column)
# $gom
# vessel_official_number    944
# $sa
# vessel_official_number    617

# dim(join_trip_and_vessel_clean_state_regions_l$gom)
# [1] 53582    32

## Shorten GOM df ----
columns_to_keep <- c(
  "vessel_official_number",
  "trip_id",
  "sero_home_port_county",
  "sero_home_port_state",
  "end_port_county",
  "end_port_state"
)

short_port_gom <-
  join_trip_and_vessel_clean_state_regions_l$gom |>
  select(all_of(columns_to_keep),
         contains("region"),
         contains("quarter")) |>
  # can use distinct, because we are not interested in the number of such trips, just the number of vessel
  distinct()

#' %%%%% Boat movement numbers for GOM
#'

# How many GOM SEFHIER vessels have a different start port county than end port county? ----

## different counties ----

# Explanation:
# 1. The 'short_port_gom' data frame is piped into the 'group_by' function to group the data based on specified columns: 'vessel_official_number', 'trip_id', 'permit_region', and 'trip_end_year_quarter'.
# 2. The 'filter' function is applied to exclude rows where 'sero_home_port_county' is equal to 'end_port_county', effectively keeping rows where the start and end port counties are different.
# 3. The 'ungroup' function is used to remove the grouping introduced by 'group_by', ensuring that subsequent operations are applied to the entire data frame.
# 4. The 'select' function is used to exclude the 'trip_id' column from the resulting data frame.
# 5. The 'distinct' function is applied to keep only unique rows in the data frame, removing any duplicate rows based on all columns. The resulting data frame is stored in the 'start_end_county_diff_gom' variable.

start_end_county_diff_gom <-
  short_port_gom |>
  group_by(vessel_official_number,
           trip_id,
           permit_region,
           trip_end_year_quarter) |>
  filter(!sero_home_port_county == end_port_county) |>
  ungroup() |>
  select(-trip_id) |>
  distinct()

### check different start and end quarters ----
join_trip_and_vessel_clean |>
  select(trip_start_year_quarter, trip_end_year_quarter) |>
  distinct() |>
  filter(!trip_start_year_quarter == trip_end_year_quarter)
  # trip_start_year_quarter trip_end_year_quarter
# 1                 2022 Q1               2022 Q2
# 2                 2022 Q2               2022 Q3

start_end_county_diff_gom |>
  filter(vessel_official_number %in% c("al4295ak", "1270320")) |>
  glimpse()

join_trip_and_vessel_trim |>
  filter(grepl("monroe", sero_home_port_county, ignore.case = T)) |>
  filter(grepl("baldwin", end_port_county, ignore.case = T)) |>
  select(
    vessel_official_number,
    sero_home_port_county,
    sero_home_port_state,
    end_port_county,
    end_port_state,
    # trip_end_quarter_num,
    trip_end_year_quarter
  ) |>
  distinct() |>
  arrange(trip_end_year_quarter) |>
  glimpse()

## count different counties ----

# Explanation:
# 1. The 'start_end_county_diff_gom' data frame is piped into the 'add_count' function.
# 2. The 'add_count' function is applied to count the occurrences of each unique combination of values in the specified columns: 'permit_region', 'sero_home_port_county', 'end_port_county', and 'trip_end_year_quarter'.
# 3. The result is stored in a new column named 'cnt_diff_county', which represents the count of occurrences for each unique combination of the specified columns.
# 4. The resulting data frame is stored in the 'start_end_county_diff_gom_num' variable, which now includes the count information in the 'cnt_diff_county' column.

start_end_county_diff_gom_num <-
  start_end_county_diff_gom |>
  add_count(
    permit_region,
    sero_home_port_county,
    end_port_county,
    trip_end_year_quarter,
    name = "cnt_diff_county"
  )

dim(start_end_county_diff_gom_num)
# [1] 250   5
# [1] 575  11
# [1] 291  13 gom only

### spot check counts ----
join_trip_and_vessel_clean |>
  filter(sero_home_port_county == "collier" &
           end_port_county == "lee") |>
  select(vessel_official_number) |>
  distinct() |>
  nrow()
# [1] 2

join_trip_and_vessel_clean |>
  filter(sero_home_port_county == "collier" &
           end_port_county == "lee") |>
  select(vessel_official_number, trip_end_year_quarter) |>
  distinct() |>
  arrange(trip_end_year_quarter) |>
  head()
#   vessel_official_number trip_end_year_quarter
# 1                 958876               2022 Q1
# 2                 958876               2022 Q2
# 3               fl5321kd               2022 Q2
# 4               fl5321kd               2022 Q3

# join_trip_and_vessel_clean |>
#   filter(sero_home_port_county == "santa rosa" &
#            end_port_county == "escambia") |>
#   select(trip_end_year_quarter, vessel_official_number) |>
#   distinct() |>
#   arrange(trip_end_year_quarter, vessel_official_number) |>
#   View()

# vessels by quarter, correct
start_end_county_diff_gom_num |>
  filter(sero_home_port_county == "collier" &
           end_port_county == "lee") |>
  arrange(trip_end_quarter_num) |>
  glimpse()
# $ trip_end_quarter_num    <chr> "1", "2", "2", "3"
# $ cnt_diff_county         <int> 1, 2, 2, 1

## Result table for GOM permit region (and the state region) ----
# GOM for Metrics tracking permit region
start_end_county_diff_gom_num_gom_permit_only <-
  start_end_county_diff_gom_num |>
  filter(permit_region == "gom")

# check
dim(start_end_county_diff_gom_num_gom_permit_only)
# [1] 270  13

# state == gom, permit gom or sa
start_end_county_diff_gom_num |>
# select(sero_home_port_state) |>
# distinct() |> # 5 states
dim()
# [1] 291  13

#### make the result table ----

# Explanation:
# 1. The 'start_end_county_diff_gom_num_gom_permit_only' data frame is piped into the 'select' function to exclude the 'permit_region' column.
# 2. The 'rowwise' function is applied to ensure that subsequent mutations are applied row-wise.
# 3. Two 'mutate' functions are applied to create new columns 'home_port_state' and 'end_port_state' by looking up state names based on their abbreviations.
# 4. Two additional 'mutate' functions are applied to create new columns 'home_port_county' and 'end_port_county' by converting the port county names to title case.
# 5. The 'ungroup' function is used to remove the grouping introduced by 'rowwise'.
# 6. The 'select' function is applied to exclude several columns that are no longer needed for the final result.
# 7. The 'relocate' function is used to reorder columns for better readability, placing important columns at the beginning.
# 8. The 'distinct' function is applied to keep only unique rows in the data frame, removing any duplicate rows based on all columns.
# 9. The resulting data frame is stored in the 'start_end_county_diff_gom_num_gom_permit_only_res' variable.

start_end_county_diff_gom_num_gom_permit_only_res <-
  start_end_county_diff_gom_num_gom_permit_only |>
  select(-permit_region) |>
  rowwise() |>
  mutate(home_port_state =
           my_state_name[[sero_home_port_state]]) |>
  mutate(end_port_state =
           my_state_name[[end_port_state]]) |>
  mutate(
    home_port_county = str_to_title(sero_home_port_county),
    end_port_county = str_to_title(end_port_county)
  ) |>
  ungroup() |>
  select(
    -c(
      sero_home_port_county,
      sero_home_port_state,
      trip_start_year_quarter,
      trip_start_quarter_num,
      trip_end_quarter_num,
      vessel_official_number
    )
  ) |>
  relocate(
    trip_end_year_quarter,
    home_port_state,
    home_port_county,
    end_port_state,
    end_port_county,
    diff_county_num_of_vessels = cnt_diff_county
  ) |>
  distinct()
# Can use distinct here, because we only look at county to county vessels, not trips

# spot check
start_end_county_diff_gom_num_gom_permit_only_res |>
    filter(home_port_county == "Pasco" &
           end_port_county == "Pinellas" &
           trip_end_year_quarter == "2022 Q1" ) |>
  glimpse()

join_trip_and_vessel_clean |>
  filter(sero_home_port_county == "pasco" &
           end_port_county == "pinellas" &
           trip_end_year_quarter == "2022 Q1" &
           permit_region == "gom") |>
  select(vessel_official_number, trip_end_year_quarter) |>
  # arrange(vessel_official_number) |>
  count(vessel_official_number)
#   vessel_official_number  n
# 1                1070441 11
# 2               fl2045nb 59
# 2 vessels, ok

start_end_county_diff_gom_num_gom_permit_only_res |>
  filter(
    # home_port_county == "Brazoria" &
    #   end_port_county == "Galveston" &
      trip_end_year_quarter == "2022 Q4"
  ) |>
  count(wt = diff_county_num_of_vessels)
# 47 tot Q4
# 2 for Brazoria - Galveston

join_trip_and_vessel_clean |>
  filter(
    sero_home_port_county == "brazoria" &
      end_port_county == "galveston" &
      trip_end_year_quarter == "2022 Q4" &
      permit_region == "gom"
  ) |>
  select(vessel_official_number, trip_end_year_quarter) |>
  # distinct()
# 970060
# tx2118fj
  count(vessel_official_number) |>
  count()
# 2, correct


# join_trip_and_vessel_clean |>
#   filter(
#     sero_home_port_county == "pinellas" &
#       end_port_county == "brunswick"
#   ) |>
#   select(vessel_official_number, trip_end_year_quarter) |>
# View()
# 1 vessel, correct

join_trip_and_vessel_trim |>
  filter(grepl("monroe", sero_home_port_county, ignore.case = T)) |>
  filter(grepl("baldwin", end_port_county, ignore.case = T)) |>
  select(
    vessel_official_number,
    sero_home_port_county,
    sero_home_port_state,
    end_port_county,
    end_port_state,
    # trip_end_quarter_num,
    trip_end_year_quarter
  ) |>
  distinct() |>
  arrange(trip_end_year_quarter) |>
  glimpse()
# Q2 2
# Q3 1

start_end_county_diff_gom_num_gom_permit_only_res |>
  filter(
    home_port_county == "Monroe" &
      end_port_county == "Baldwin"
  ) |>
  count(trip_end_year_quarter,
        wt = diff_county_num_of_vessels)
# 1 2022 Q2                   2
# 2 2022 Q3                   1
# ok

##### Write out ----

write_csv(
  start_end_county_diff_gom_num_gom_permit_only_res,
  file.path(
    curr_proj_output_path,
    "start_end_county_diff_gom_num_gom_permit_only_res.csv"
  )
)

#### Diff county numbers, by quarter (1-4) ----

# Explanation:
# 1. The 'start_end_county_diff_gom_num_gom_permit_only_res' data frame is piped into the 'group_by' function, grouping the data by 'trip_end_year_quarter'.
# 2. The 'group_by' function is applied to group the data by the 'trip_end_year_quarter' column.
# 3. The 'count' function is applied to calculate the total count of 'diff_county_num_of_vessels' for each unique 'trip_end_year_quarter'. The result is stored in a new column named 'diff_county_num_of_vessels_tot'.
# 4. The resulting data frame is stored in the 'start_end_county_diff_gom_num_gom_permit_only_res_quarter' variable, which now includes the total count of vessels for each quarter based on the 'diff_county_num_of_vessels' column.

start_end_county_diff_gom_num_gom_permit_only_res_quarter <-
  start_end_county_diff_gom_num_gom_permit_only_res |>
  group_by(trip_end_year_quarter) |>
  count(wt = diff_county_num_of_vessels,
        name = "diff_county_num_of_vessels_tot")

head(start_end_county_diff_gom_num_gom_permit_only_res_quarter)
#   trip_end_year_quarter diff_county_num_of_vessels_tot
#   <yearqtr>                                      <int>
# 1 2022 Q1                                           24
# 2 2022 Q2                                          103
# 3 2022 Q3                                           96
# 4 2022 Q4                                           47

# How many SEFHIER vessels have a different start port state than end port state? ----

## different start and end states in one trip ----

# Explanation:
# 1. The 'short_port_gom' data frame is piped into the 'filter' function to include only rows where 'permit_region' is "gom".
# 2. The 'filter' function is applied to include only rows where 'permit_region' is "gom".
# 3. The 'select' function is applied to keep only selected columns: 'vessel_official_number', 'sero_home_port_state', 'end_port_state', and 'trip_end_year_quarter'.
# 4. The 'distinct' function is applied to keep only unique rows in the data frame, removing any duplicate rows based on all columns.
# 5. Another 'filter' function is applied to exclude rows where 'sero_home_port_state' is equal to 'end_port_state', effectively keeping rows where the start and end states are different.
# 6. The 'count' function is applied to calculate the total count of vessels for each unique combination of 'trip_end_year_quarter', 'sero_home_port_state', and 'end_port_state'. The result is stored in a new column named 'diff_states_num_of_vessels'.
# 7. The resulting data frame is stored in the 'start_end_state_diff_num_gom_only_res' variable, which now includes the total count of vessels for each quarter based on the 'diff_states_num_of_vessels' column.

# All functions are from dplyr

start_end_state_diff_num_gom_only_res <-
  short_port_gom |>
  filter(permit_region == "gom") |>
  select(
    vessel_official_number,
    sero_home_port_state,
    end_port_state,
    trip_end_year_quarter
  ) |>
  distinct() |>
  filter(!sero_home_port_state == end_port_state) |>
  count(trip_end_year_quarter,
        sero_home_port_state,
        end_port_state,
        name = "diff_states_num_of_vessels")

### spot check ----

start_end_state_diff_num_gom_only_res |>
  filter(trip_end_year_quarter == "2022 Q1") |>
  glimpse()

start_end_state_diff_num_gom_only_res |>
  filter(sero_home_port_state == "fl" &
           end_port_state == "al" &
           trip_end_year_quarter == "2022 Q2") |>
  glimpse()
# 2022 Q2
# Florida
# Alabama
# 5

join_trip_and_vessel_clean |>
  filter(sero_home_port_state == "fl" &
           end_port_state == "al" &
           trip_end_quarter_num == 2) |>
  select(vessel_official_number,
         trip_end_year_quarter,
         sero_home_port_state,
         end_port_state) |>
  distinct() |>
  nrow()
# 5
# OK

## Diff states numbers, by quarter (1-4) ----

# Explanation:
# 1. The 'start_end_state_diff_num_gom_only_res' data frame is piped into the 'group_by' function, grouping the data by 'trip_end_year_quarter'.
# 2. The 'group_by' function is applied to group the data by the 'trip_end_year_quarter' column.
# 3. The 'count' function is applied to calculate the total count of 'diff_states_num_of_vessels' for each unique 'trip_end_year_quarter'. The result is stored in a new column named 'diff_states_num_of_vessels_tot'.
# 4. The 'ungroup' function is used to remove the grouping introduced by 'group_by', ensuring that subsequent operations are applied to the entire data frame.
# 5. The resulting data frame is stored in the 'start_end_state_diff_num_gom_only_res_quarter' variable, which now includes the total count of vessels for each quarter based on the 'diff_states_num_of_vessels_tot' column.

# All functions are from dplyr

start_end_state_diff_num_gom_only_res_quarter <-
  start_end_state_diff_num_gom_only_res |>
  group_by(trip_end_year_quarter) |>
  count(wt = diff_states_num_of_vessels,
        name = "diff_states_num_of_vessels_tot") |>
  ungroup()

head(start_end_state_diff_num_gom_only_res_quarter)
#   trip_end_year_quarter diff_states_num_of_vessels_tot
#   <yearqtr>                                      <int>
# 1 2022 Q1                                            3
# 2 2022 Q2                                           30
# 3 2022 Q3                                           26
# 4 2022 Q4                                           10

### spot check ----
# 2022 Q4
join_trip_and_vessel_clean_state_regions |>
  filter(sero_home_state_region == "gom") |>
  filter(permit_region == "gom") |>
  filter(trip_end_quarter_num == 4) |>
  filter(!sero_home_port_state == end_port_state) |>
  select(
    vessel_official_number,
    sero_home_port_state,
    end_port_state
  ) |>
  distinct() |>
  arrange(sero_home_port_state,
          end_port_state) |>
  # View()
  count(sero_home_port_state,
        end_port_state) |>
  glimpse()
# ok

## save results to csv ----

# Explanation:
# 1. The 'start_end_state_diff_num_gom_only_res' data frame is piped into the 'rowwise' function to ensure that subsequent mutations are applied row-wise.
# 2. Two 'mutate' functions are applied to create new columns 'sero_home_port_state' and 'end_port_state' by looking up state names based on their abbreviations using the 'my_state_name' list.
# 3. The 'ungroup' function is used to remove the grouping introduced by 'rowwise'.
# 4. The 'write_csv' function is applied to write the data frame to a CSV file. The 'file.path' function is used to create the file path by combining the 'curr_proj_output_path' (current project output path) and the desired filename "start_end_state_diff_num_gom_only_res.csv".
# 5. The resulting CSV file is saved in the specified output path for further use or analysis.

start_end_state_diff_num_gom_only_res |>
  rowwise() |>
  mutate(sero_home_port_state =
           my_state_name[[sero_home_port_state]]) |>
  mutate(end_port_state =
           my_state_name[[end_port_state]]) |>
  ungroup() |>
  write_csv(file.path(curr_proj_output_path,
                      "start_end_state_diff_num_gom_only_res.csv"))


# Explanation:
# 1. The 'start_end_state_diff_num_gom_only_res' data frame is piped into the 'count' function to calculate counts for each unique combination of 'trip_end_year_quarter', 'sero_home_port_state', and 'end_port_state'.
# 2. The 'count' function is applied with the 'wt' parameter set to 'diff_states_num_of_vessels' for weighted counting. The resulting counts are stored in a new column named 'states_cnt'.
# 3. Two 'mutate' functions are applied to convert the state names in the 'sero_home_port_state' and 'end_port_state' columns to uppercase. This ensures consistency in the representation of state names.
# 4. The resulting data frame is stored in the 'start_end_state_diff_num_gom_only_res_cnts_by_home' variable, which includes counts for each unique combination of 'trip_end_year_quarter', 'sero_home_port_state', and 'end_port_state'.
start_end_state_diff_num_gom_only_res_cnts_by_home <-
  start_end_state_diff_num_gom_only_res |>
  count(
    trip_end_year_quarter,
    sero_home_port_state,
    end_port_state,
    wt = diff_states_num_of_vessels,
    name = "states_cnt"
  ) |>
  mutate(
    sero_home_port_state = toupper(sero_home_port_state),
    end_port_state = toupper(end_port_state)
  )

# ---
# Explanation:
# 1. The 'start_end_state_diff_num_gom_only_res_cnts_by_home' data frame is piped into the 'add_count' function to calculate the total count for each unique combination of 'trip_end_year_quarter' and 'sero_home_port_state'.
# 2. The 'add_count' function is applied with the 'wt' parameter set to 'states_cnt' for weighted counting. The resulting total counts are stored in a new column named 'sum_by_q_and_home'.
# 3. The resulting data frame is stored in the 'start_end_state_diff_num_gom_only_res_cnts_by_home_sum' variable, which includes the total counts for each unique combination of 'trip_end_year_quarter' and 'sero_home_port_state'.

start_end_state_diff_num_gom_only_res_cnts_by_home_sum <-
  start_end_state_diff_num_gom_only_res_cnts_by_home |>
  add_count(trip_end_year_quarter,
            sero_home_port_state,
            wt = states_cnt,
            name = "sum_by_q_and_home")

# Write to csv
start_end_state_diff_num_gom_only_res_cnts_by_home_sum |>
  write_csv(
    file.path(
      curr_proj_output_path,
      "start_end_state_diff_num_gom_only_res_cnts_by_home_sum.csv"
    )
  )

## State to state by state and quarter res table ----

# Explanation:
# 1. The 'start_end_state_diff_num_gom_only_res' data frame is piped into the 'count' function to calculate counts for each unique combination of 'trip_end_year_quarter' and 'sero_home_port_state'.
# 2. The 'count' function is applied with the 'wt' parameter set to 'diff_states_num_of_vessels' for weighted counting. The resulting counts are stored in a new column named 'diff_states_num_of_vessels_home'.
# 3. The 'rowwise' function is applied to ensure that subsequent mutations are applied row-wise.
# 4. The 'mutate' function is applied to create a new column 'sero_home_port_state' by looking up state names based on their abbreviations using the 'my_state_name' list.
# 5. The 'ungroup' function is used to remove the grouping introduced by 'rowwise'.
# 6. The resulting data frame is stored in the 'start_end_state_diff_num_gom_only_res_home' variable, which now includes counts for each unique combination of 'trip_end_year_quarter' and 'sero_home_port_state', with the state names updated for better readability.

start_end_state_diff_num_gom_only_res_home <-
  start_end_state_diff_num_gom_only_res |>
  count(trip_end_year_quarter,
        sero_home_port_state,
        wt = diff_states_num_of_vessels,
        name = "diff_states_num_of_vessels_home") |>
  rowwise() |>
  mutate(sero_home_port_state =
           my_state_name[[sero_home_port_state]]) |>
  ungroup()

# Write to a file
write_csv(
  start_end_state_diff_num_gom_only_res_home,
  file.path(
    curr_proj_output_path,
    "start_end_state_diff_num_gom_only_res_home.csv"
  )
)

# How many SEFHIER vessels have a different start port region (Gulf) than end port region (South Atlantic)? ----

# Explanation:
# 1. The 'start_end_county_diff_gom_num_gom_permit_only_res' data frame is piped into the 'select' function to keep only selected columns.
# 2. The 'select' function is applied to keep only selected columns: 'trip_end_year_quarter', 'sero_home_state_region', 'end_state_region', and 'diff_county_num_of_vessels'.
# 3. The 'distinct' function is applied to keep only unique rows in the data frame, removing any duplicate rows based on all columns.
# 4. Another 'filter' function is applied to exclude rows where 'sero_home_state_region' is equal to 'end_state_region', effectively keeping rows where the start and end state regions are different.
# 5. The 'group_by' function is applied to group the data by 'trip_end_year_quarter'.
# 6. The 'count' function is applied to calculate the total count of 'diff_county_num_of_vessels' for each unique 'trip_end_year_quarter'. The result is stored in a new column named 'diff_port_regions_num_of_vessels_tot'.
# 7. The resulting data frame is stored in the 'start_end_state_region_diff_num_gom_only_res_quarter' variable, which now includes the total count of vessels for each quarter based on the 'diff_port_regions_num_of_vessels_tot' column.

# All functions are from dplyr

start_end_state_region_diff_num_gom_only_res_quarter <-
  start_end_county_diff_gom_num_gom_permit_only_res |>
  select(
    trip_end_year_quarter,
    sero_home_state_region,
    end_state_region,
    diff_county_num_of_vessels
  ) |>
  distinct() |>
  filter(!sero_home_state_region == end_state_region) |>
  group_by(trip_end_year_quarter) |>
  count(wt = diff_county_num_of_vessels,
        name = "diff_port_regions_num_of_vessels_tot")

# result for state region to region
# 3 in the whole year (for GOM home port, and GOM permit)
#   trip_end_year_quarter diff_port_regions_num_of_vessels_tot
#   <yearqtr> <int>
# 1 2022 Q1   1
# 2 2022 Q2   1
# 3 2022 Q4   1

#' Nothing to show, only 1 vessel has trips starting in Fl and ending in North Carolina in Q2 and 1 vessel in Q4.
#' Plus 1 vessel in Q1 from Sarasota, Florida to Duval, Florida

# Quantify the # of vessels who fish in both the gulf and S Atl ----
# Notes:
# GOM permit
# retain Monroe
# Create 2 dfs fished in GOM or in SA using lat and lon for area fished
# grouping by vessel ID and quarter, check if unique vessel fishing in GOM and in SA

## prep fishing locations ----

### Get GOM permitted vessels with Lat and Long ----
# Explanation:
# 1. The 'join_trip_and_vessel_clean_state_regions_l$gom' data frame, representing the GOM (Gulf of Mexico) region from the joined data, is piped into the 'filter' function to include only rows where 'permit_region' is "gom".
# 2. The 'filter' function is applied to include only rows where 'permit_region' is "gom".
# 3. The 'select' function is applied to keep only selected columns: 'vessel_official_number', 'latitude', 'longitude', and 'trip_end_year_quarter'.
# 4. Another 'filter' function is applied to exclude rows where 'latitude' or 'longitude' is NA.
# 5. The 'distinct' function is applied to keep only unique rows in the data frame, removing any duplicate rows based on all columns.
# 6. The resulting data frame is stored in the 'lat_lon_gom_state' variable, which includes information about the latitude, longitude, and vessel details for the GOM region.
lat_lon_gom_state <-
  join_trip_and_vessel_clean_state_regions_l$gom |>
  filter(permit_region == "gom") |>
  select(vessel_official_number,
         latitude,
         longitude,
         trip_end_year_quarter) |>
  filter(!is.na(latitude) &
           !is.na(longitude)) |>
  distinct()

# dim(lat_lon_gom_state)
# [1] 46181     5

### Count points ----

# Explanation:
# 1. The 'lat_lon_gom_state' data frame is piped into the 'mutate' function to modify the 'latitude' and 'longitude' columns by taking their absolute values. This is done to handle potential discrepancies in latitude and longitude data.
# 2. The 'mutate' function is applied to modify the 'latitude' and 'longitude' columns by taking their absolute values. And then converting all longitude to negative values.
# 3. The 'add_count' function is applied to calculate the total count of vessels for each unique combination of 'latitude' and 'longitude'. The result is stored in a new column named 'cnt_v_coords_by_y'.
# 4. Another 'add_count' function is applied to calculate the total count of vessels for each unique combination of 'latitude', 'longitude', and 'trip_end_year_quarter'. The result is stored in a new column named 'cnt_v_coords_by_q'.
# 5. The resulting data frame is stored in the 'lat_lon_gom_state_cnt' variable, which includes counts for each unique combination of coordinates and quarter based on the 'cnt_v_coords_by_y' and 'cnt_v_coords_by_q' columns.

lat_lon_gom_state_cnt <-
  lat_lon_gom_state |>
  mutate(latitude = abs(latitude),
         longitude = -abs(longitude)) |>
  add_count(latitude, longitude,
            name = "cnt_v_coords_by_y") |>
  add_count(latitude,
            longitude,
            trip_end_year_quarter,
            name = "cnt_v_coords_by_q")

# View(lat_lon_gom_state_cnt)
## Define a common crs ----
my_crs <- 4326

# Create a new object 'lat_lon_gom_state_cnt_sf' by piping the data frame
# 'lat_lon_gom_state_cnt' into the st_as_sf function from the sf package

# The st_as_sf function is used to convert a data frame with latitude and
# longitude columns into an sf (simple feature) object
  # Specify the latitude and longitude columns for the sf object
    # Set the coordinate reference system (CRS) for the sf object
    # Keep the original columns in the resulting sf object

lat_lon_gom_state_cnt_sf <-
  lat_lon_gom_state_cnt |>
  st_as_sf(
    coords = c("longitude", "latitude"),
    crs = my_crs,
    remove = FALSE
  )

dim(lat_lon_gom_state_cnt_sf)
# [1] 36173     7

# all points
# mapview(lat_lon_gom_state_cnt_sf)

# lat_lon_gom_state_cnt_sf |>
#   mapview(
#     cex = "cnt_v_coords_by_y",
#     alpha = 0.3,
#     col.regions = viridisLite::turbo,
#     # legend = FALSE
#     layer.name = "GOM permit trips"
#   )

## List of loaded shapefiles ----
# GOMsf
# world_state_and_fed_waters_path
# fl_state_w_counties_shp
# GOM_s_fl_state_waters_only
# shp_4326_list:
# east_coast_sa_state_waters_shp
# gom_fl_state_w_counties_shp
# sa_fl_state_w_counties_shp
# sa_shp
# gom_states_shp
# sa_states_shp

## Split by region using shape files ----

### Aux function ----
# Explanation:
# 1. The 'intersect_waters_and_points' function is defined to take two spatial data frames, 'my_shp' (shapefile) and 'my_points'.
# 2. Inside the function, the 'st_intersection' function from the 'sf' package is applied to calculate the intersection of the two spatial data frames. This function identifies the common geometries between the shapefile and the points.
# 3. The resulting spatial data frame representing the intersection is stored in the 'intersect_result' variable.
# 4. The function returns the 'intersect_result', which contains the geometries that are common to both 'my_shp' and 'my_points'.
intersect_waters_and_points <-
  function(my_shp,
           my_points) {
    intersect_result <-
      st_intersection(my_shp,
                      my_points)
    return(intersect_result)
  }

### fishing in GOM  ----
# Read a file or run the function
gom_lat_lon_gom_state_cnt_sf_fed_w_file_path <-
  file.path(curr_proj_output_path,
            "fishing_regions_gom_permits",
            "gom_lat_lon_gom_state_cnt_sf_fed_w.rds")

# file.exists(gom_lat_lon_gom_state_cnt_sf_fed_w_file_path)

gom_lat_lon_gom_state_cnt_sf_fed_w <-
  read_rds_or_run_no_db(
    gom_lat_lon_gom_state_cnt_sf_fed_w_file_path,
    list(GOMsf,
         lat_lon_gom_state_cnt_sf),
    intersect_waters_and_points
  )
# run the function: 56.91 sec elapsed

# mapview(gom_lat_lon_gom_state_cnt_sf_fed_w)

### Fishing in SA ----
#### Federal waters ----

# Read a file or run the function for SA fed

sa_fed_waters_points_path <-
  file.path(curr_proj_output_path,
            "fishing_regions_gom_permits",
            "sa_fed_waters_points.rds")

# file.exists(gom_lat_lon_gom_state_cnt_sf_fed_w_file_path)

sa_fed_waters_points <-
  read_rds_or_run_no_db(
    sa_fed_waters_points_path,
    list(shp_4326_list$sa_shp,
         lat_lon_gom_state_cnt_sf),
    intersect_waters_and_points
  )
# run the function: 43.64 sec elapsed

# mapview(sa_fed_waters_points)

### fewer points by SA fed waters, to speed up ----
sa_bb <- st_bbox(shp_4326_list$sa_shp)
sa_bb_points <- st_crop(lat_lon_gom_state_cnt_sf, sa_bb)

# dim(sa_bb_points)
# [1] 10293     7

#### state waters, Monroe in both regions ----
# mapview(east_coast_sa_state_waters_shp)

# Read a file or run the function to subset points by SA state waters
sa_state_waters_points_path <-
  file.path(curr_proj_output_path,
            "fishing_regions_gom_permits",
            "sa_all_state_waters_points.rds")

# file.exists(sa_state_waters_points_path)

sa_state_waters_points <-
  read_rds_or_run_no_db(
    sa_state_waters_points_path,
    list(shp_4326_list$east_coast_sa_state_waters_shp,
         sa_bb_points),
    intersect_waters_and_points
  )
# run the function: 701.09 sec elapsed

# mapview(sa_state_waters_points)

#### Remove GOM Monroe points from all state waters ----
# Fewer fields
gom_lat_lon_gom_state_cnt_sf_fed_w_short <-
  gom_lat_lon_gom_state_cnt_sf_fed_w |>
  select(-c(StatZone,
            DepZone,
            Jurisdict,
            Activity,
            Shape_Area))

dim(gom_lat_lon_gom_state_cnt_sf_fed_w_short)
# [1] 31081     7

sa_state_waters_points_short <-
  sa_state_waters_points |>
  select(
    vessel_official_number,
    latitude,
    longitude,
    trip_end_year_quarter,
    cnt_v_coords_by_y,
    cnt_v_coords_by_q,
    geometry
  )

dim(sa_state_waters_points_short)
# [1] 6144    7

# Convert back to dfs
sa_state_waters_points_short_df <-
  st_drop_geometry(sa_state_waters_points_short)

gom_lat_lon_gom_state_cnt_sf_fed_w_short_df <-
  st_drop_geometry(gom_lat_lon_gom_state_cnt_sf_fed_w_short)

# Keep only SA state water points which are not in GOM
sa_state_waters_points_short_df_no_gom <-
  anti_join(sa_state_waters_points_short_df,
            gom_lat_lon_gom_state_cnt_sf_fed_w_short_df)

# mapview(sa_state_waters_points_short_df_no_gom,
#         xcol = "longitude",
#         ycol = "latitude",
#         crs = my_crs)

# dim(sa_state_waters_points_short_df_no_gom)
# 560 6

# Remove not sa counties ---

# Get the county column
logbooks_w_county <-
  join_trip_and_vessel_clean_state_regions_l$gom |>
  select(sero_home_port_county,
    any_of(names(sa_state_waters_points_short_df_no_gom))) |>
  distinct()

dim(logbooks_w_county)
# [1] 46424     5

# add the county column to the cropped df
sa_state_waters_points_short_df_no_gom_counties <-
  sa_state_waters_points_short_df_no_gom |>
  left_join(logbooks_w_county)
# Joining with `by = join_by(vessel_official_number, latitude, longitude,
# trip_end_year_quarter)`

# The 'filter' function is applied to include only rows where 'sero_home_port_county' is in the list of lowercased counties corresponding to the South Atlantic (SA) region in Florida.
sa_state_waters_points_short_df_no_gom_counties_sa <-
  sa_state_waters_points_short_df_no_gom_counties |>
  filter(sero_home_port_county %in% tolower(fl_counties$sa))

dim(sa_state_waters_points_short_df_no_gom_counties_sa)
# [1] 544   7
# 560 - 544 = 16 points removed

# Check counties
# sa_state_waters_points_short_df_no_gom_counties_sa |>
#   select(sero_home_port_county) |>
#   distinct()
# monroe

# mapview(sa_state_waters_points_short_df_no_gom_counties_sa,
#         xcol = "longitude",
#         ycol = "latitude",
#         crs = my_crs)

## Join all points with regions by vessel ----
### back to dfs for join ----
gom_lat_lon_gom_state_cnt_fed_w_df <-
  st_drop_geometry(gom_lat_lon_gom_state_cnt_sf_fed_w)
# str(gom_lat_lon_gom_state_cnt_fed_w_df)

sa_lat_lon_gom_state_cnt_sf_fed_w_df <-
  st_drop_geometry(sa_fed_waters_points)
# str(sa_lat_lon_gom_state_cnt_sf_fed_w_df)

### join point data frames ----
# get common names
keep_sa_fields <-
  intersect(
    names(sa_lat_lon_gom_state_cnt_sf_fed_w_df),
    names(sa_state_waters_points_short_df_no_gom_counties_sa)
  )

# Explanations:
#
# 1. **List Creation:**
#    - The `list` function creates a list containing two data frames: `sa_lat_lon_gom_state_cnt_sf_fed_w_df` and `sa_state_waters_points_short_df_no_gom_counties`.
#
# 2. **Mapping and Combining Data Frames:**
#    - The `map_df` function is applied to the list of data frames.
#    - The lambda function with a `curr_df` parameter selects specific columns from each data frame.
#    - The results are combined into a single data frame.
#
# 3. **Column Selection:**
#    - The pipe operator (`|>`) passes each data frame to the next operation.
#    - The `select` function is used to subset the columns of each data frame based on the column names specified in `keep_sa_fields`.
#
# 4. **Final Result:**
#    - The variable `all_points_sa` holds the resulting data frame with columns selected from the original data frames.

all_points_sa <-
  list(sa_lat_lon_gom_state_cnt_sf_fed_w_df,
       sa_state_waters_points_short_df_no_gom_counties) |>
  map_df(\(curr_df) {
    curr_df |>
      select(all_of(keep_sa_fields))
  })

dim(all_points_sa)
# [1] 1013    6
# check 994360 in state waters
# all_points_sa_l |>
#   filter(vessel_official_number == "994360") |>
#   View()

# Keep the same columns for gom
gom_lat_lon_gom_state_cnt_fed_w_df_short <-
  gom_lat_lon_gom_state_cnt_fed_w_df |>
  select(all_of(keep_sa_fields))

# join gom and sa points
all_fish_points <-
  full_join(
    gom_lat_lon_gom_state_cnt_fed_w_df_short,
    all_points_sa,
    join_by(vessel_official_number,
            trip_end_year_quarter),
    relationship = "many-to-many",
    suffix = c(".gom", ".sa")
  )

## add markers for having gom or sa fishing locations ----

# 1. **Grouping by Vessel Official Number:**
#    - The pipe operator (`|>`) passes the data frame 'all_fish_points' to the next operation.
#    - The `group_by` function is used to group the data frame by the 'vessel_official_number' column.
#
# 2. **Adding New Columns with Mutate:**
#    - The `mutate` function adds two new columns, 'has_gom_point_y' and 'has_sa_point_y', to the data frame.
#    - The 'has_gom_point_y' column is determined using the `any` function to check if any values in the 'latitude.gom' column are not NA.
#    - The 'has_sa_point_y' column is determined using the `any` function to check if any values in the 'latitude.sa' column are not NA.
#
# 3. **Ungrouping:**
#    - The `ungroup` function removes the grouping structure from the data frame, returning it to an ungrouped state.
#
# 4. **Final Result:**
#    - The variable 'all_fish_points_reg_y' holds the modified data frame with added columns indicating the presence of non-NA values in specific latitude columns for each vessel.

all_fish_points_reg_y <-
  all_fish_points |>
  group_by(vessel_official_number) |>
  mutate(has_gom_point_y =
           any(!is.na(latitude.gom), na.rm = TRUE),
         has_sa_point_y =
           any(!is.na(latitude.sa), na.rm = TRUE)) |>
  ungroup()

# Keep vessels, having both SA and GOM points
all_fish_points_reg_both_y <-
  all_fish_points_reg_y |>
  filter(has_gom_point_y & has_sa_point_y)

dim(all_fish_points_reg_both_y)
# [1] 8524   18
# [1] 4479   12 gom permit only
# vessel_official_number   76 all permits
# n_distinct(all_fish_points_reg_both_y$vessel_official_number)
# 30

### same by quarter ----
all_fish_points_reg_q <-
  all_fish_points |>
  group_by(vessel_official_number,
           trip_end_year_quarter) |>
  mutate(has_gom_point_q =
           any(!is.na(latitude.gom), na.rm = TRUE),
         has_sa_point_q =
           any(!is.na(latitude.sa), na.rm = TRUE)) |>
  ungroup()

all_fish_points_reg_both_q <-
  all_fish_points_reg_q |>
  filter(has_gom_point_q & has_sa_point_q)

dim(all_fish_points_reg_both_q)
# [1] 7145   18
# [1] 3940   12 gom permit only

n_distinct(all_fish_points_reg_both_q$vessel_official_number)
# [1] 30

# Count vessels fishing in both regions by quarter ---
# Explanations
# 1. **Column Selection:**
#    - The pipe operator (`|>`) passes the data frame 'all_fish_points_reg_both_q' to the next operation.
#    - The `select` function is used to subset the data frame to include only the columns 'trip_end_year_quarter' and 'vessel_official_number'.
#
# 2. **Distinct Rows:**
#    - The `distinct` function returns unique combinations of the specified columns, effectively removing duplicate rows.
#
# 3. **Counting Rows:**
#    - The `count` function is used to count the number of rows for each unique combination of 'trip_end_year_quarter'.
#    - The result is the count of unique combinations of 'trip_end_year_quarter', providing information about the number of vessels in each quarter.
#
# 4. **Final Result:**
#    - The output of the entire sequence is not assigned to a variable, but it represents the count of unique combinations of 'trip_end_year_quarter' in the original data frame 'all_fish_points_reg_both_q'.

all_fish_points_reg_both_q |>
  select(trip_end_year_quarter, vessel_official_number) |>
  distinct() |>
  count(trip_end_year_quarter)
#   trip_end_year_quarter     n
#   <yearqtr>             <int>
# 1 2022 Q1                  35
# 2 2022 Q2                  31
# 3 2022 Q3                  35
# 4 2022 Q4                  24

# no SA permit
# 1 2022 Q1                  13
# 2 2022 Q2                  14
# 3 2022 Q3                  15
# 4 2022 Q4                  13

## map all_fish_points_reg_both_q ----

# Explanations
#
# 1. **Conversion to Simple Feature (sf) Data Frame:**
#    - The pipe operator (`|>`) passes the data frame 'all_fish_points_reg_both_q' to the next operation.
#    - The `st_as_sf` function converts the data frame to a simple feature (sf) data frame, using the columns 'longitude.gom' and 'latitude.gom' as coordinates and setting the coordinate reference system (crs) to 'my_crs'. The 'remove' parameter is set to FALSE, preserving the original data frame.
#
# 2. **Column Subsetting:**
#    - The `select` function is used to subset the columns of the resulting sf data frame, excluding those ending with ".sa".
#
# 3. **Column Renaming:**
#    - The `rename` function is used to rename the 'geometry' column to 'geometry_gom'.
#
# 4. **Final Result:**
#    - The variable 'all_fish_points_reg_both_q_gom_sf' holds the modified sf data frame with coordinates from the Gulf of Mexico region, excluding columns related to South Atlantic coordinates and with the 'geometry' column renamed to 'geometry_gom'.

all_fish_points_reg_both_q_gom_sf <-
  all_fish_points_reg_both_q |>
  st_as_sf(
    coords = c("longitude.gom", "latitude.gom"),
    crs = my_crs,
    remove = FALSE
  ) |>
  select(-ends_with(".sa")) |>
  rename(geometry_gom = geometry)

# The same for SA
all_fish_points_reg_both_q_sa_sf <-
  all_fish_points_reg_both_q |>
  st_as_sf(
    coords = c("longitude.sa", "latitude.sa"),
    crs = my_crs,
    remove = FALSE
  ) |>
  select(-ends_with(".gom")) |>
  rename(geometry_sa = geometry)

# Create the map
all_sa_gom_map <-
  mapview(all_fish_points_reg_both_q_sa_sf,
          col.regions = "blue") +
  mapview(all_fish_points_reg_both_q_gom_sf,
          col.regions = "green")

# Uncomment to see the map
# all_sa_gom_map

### Fishing in SA and GOM map by quarter ----

# Explanations:
#
# 1. **List Creation:**
#    - The `list` function is used to create a list containing two sf data frames: `all_fish_points_reg_both_q_sa_sf` and `all_fish_points_reg_both_q_gom_sf`.
#
# 2. **Mapping and Splitting Data Frames:**
#    - The `map` function applies the following lambda function to each element of the list.
#    - For each sf data frame (`curr_df`), the data is split into a list of data frames (`list_by_reg_q`) based on the 'trip_end_year_quarter' column.
#
# 3. **Return of List of Data Frames:**
#    - The lambda function returns the list of data frames (`list_by_reg_q`) created by the `split` operation.
#
# 4. **Final Result:**
#    - The variable 'all_fish_points_reg_both_q_sf_quarters' holds a list of data frames, each representing a subset of the original sf data frames based on the 'trip_end_year_quarter' column. The list is created by splitting the data frames for both South Atlantic and Gulf of Mexico regions.

all_fish_points_reg_both_q_sf_quaters <-
  list(all_fish_points_reg_both_q_sa_sf,
       all_fish_points_reg_both_q_gom_sf) |>
  map(\(curr_df) {
    list_by_reg_q <-
      curr_df |>
      split(as.factor(curr_df$trip_end_year_quarter))

    return(list_by_reg_q)
  })

# Add names
names(all_fish_points_reg_both_q_sf_quaters) <-
  c("sa",
    "gom")

# View(all_fish_points_reg_both_q_sf_quaters)

# List of all quarters
all_quarters_list <-
  names(all_fish_points_reg_both_q_sf_quaters$sa)

# uncomment to see an example for the 1 quarter
# mapview(all_fish_points_reg_both_q_sf_quaters$sa$`2022 Q1`,
        # col.regions = "green") +
# mapview(all_fish_points_reg_both_q_sf_quaters$gom$`2022 Q1`,
        # col.regions = "blue")

# Explanations
#
# 1. **Mapping and Creating Views:**
#    - The pipe operator (`|>`) passes the 'all_quarters_list' to the next operation.
#    - The `map` function applies the following lambda function to each element of the list (`curr_quarter`).
#
# 2. **MapView for South Atlantic and Gulf of Mexico:**
#    - Inside the lambda function:
#      - The `mapview` function is used to create map views for both the South Atlantic and Gulf of Mexico regions.
#      - Data frames for each region and quarter are accessed from the list 'all_fish_points_reg_both_q_sf_quarters'.
#      - For South Atlantic, 'col.regions' is set to "green", and for Gulf of Mexico, it's set to "blue".
#
# 3. **Final Result:**
#    - The variable 'all_maps_by_q' holds a list of map views, each representing data for a specific quarter. The maps include points from both the South Atlantic and Gulf of Mexico regions, with distinct colors for each region.
all_maps_by_q <-
  all_quarters_list |>
  map(\(curr_quarter) {
    mapview(all_fish_points_reg_both_q_sf_quaters$sa[[curr_quarter]],
            col.regions = "green") +
      mapview(all_fish_points_reg_both_q_sf_quaters$gom[[curr_quarter]],
              col.regions = "blue")
  })

names(all_maps_by_q) <- all_quarters_list


# View(all_fish_points_reg_both_q_sf_quaters$sa)
# same in plots ----
all_plots_by_q <-
  all_quarters_list |>
  map(\(curr_quarter) {
    my_title = curr_quarter
  ggplot() +
  geom_sf(data =
            all_fish_points_reg_both_q_sf_quaters$sa[[curr_quarter]],
          aes(
            geometry = geometry_sa,
            # fill = q_factors,
            colour = "sa"
          )) +
  geom_sf(data =
            all_fish_points_reg_both_q_sf_quaters$gom[[curr_quarter]],
          aes(
            geometry = geometry_gom,
            # fill = q_factors,
            colour = "gom"
          )) +
  geom_sf(data = sa_states_shp, fill = NA) +
  geom_sf(data = gom_states_shp, fill = NA) +
        labs(title = my_title) +
    theme_bw()
  })

# all_plots_by_q[[1]]

# combine plots
super_title <-
  "Vessels with GOM or dual permits fishing in both regions in 2022"

plots_by_q_arranged <-
  grid.arrange(grobs = all_plots_by_q,
             top = super_title,
             # left = my_legend,
             ncol = 2)

plots_by_q_arranged_dir <-
  file.path(curr_proj_output_path,
            "fishing_regions_gom_permits")

ggsave(
  file = "plots_by_q_arranged.png",
  plot = plots_by_q_arranged,
  device = "png",
  path = plots_by_q_arranged_dir,
  width = 30,
  height = 20,
  units = "cm"
)
