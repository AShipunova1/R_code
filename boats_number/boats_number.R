#' %%%%% Prepare data
#'

# setup current project ----
library(zoo)
# Determine the path of the executing script
library(this.path)

# Prints an R object in markdown, needed to print pretty table from list of dfs.
library(pander)

# Colorblind-Friendly Color Map for R
library(viridis)

source("~/R_code_github/useful_functions_module.r")
my_paths <- set_work_dir()

# Get the current project directory name using the 'this.path' package.
current_project_dir_name <- this.path::this.dir()
current_project_basename <-
  basename(current_project_dir_name)

## additional data ----
# for qmd use #' {{< include .qmd >}} instead of source()

misc_info_path <-
  file.path(my_paths$git_r,
            r"(get_data\misc_info.R)")
source(misc_info_path)

#' {{< include misc_info.qmd >}}
#'

boats_number_get_data_info_path <-
  file.path(current_project_dir_name,
            r"(boats_number_get_data.R)")

source(boats_number_get_data_info_path)

## remove unused fields from all_logbooks_db_data_2022_short_p_region or
# processed_logbooks ----

names(processed_logbooks_clean_names) |>
  sort() |>
  cat(sep = ", ")

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
    "trip_end_date",
    "trip_start_date"
  )

# grep("state", names(processed_logbooks_clean_names), value = T, ignore.case = T)
processed_logbooks_short <-
  processed_logbooks_clean_names |>
  dplyr::select(dplyr::all_of(port_fields_short)) |>
  remove_empty_cols() |>
  dplyr::distinct()

dim(processed_logbooks_short)
# [1] 3011    7
# [1] 2475    6 (no overridden)
# [1] 66641     8 with start and end dates

# check port info ----
## port codes are different, but port names are the same ----
processed_logbooks_short |>
  dplyr::filter(!start_port == end_port &
           start_port_name == end_port_name) |>
  select(-starts_with("trip")) |>
  distinct() |>
  dplyr::glimpse()
# 2
# $ vessel_id           <int> 326764, 254794
# $ vessel_official_number <chr> "NC8438DJ", "FL0291MX"
# $ permit_region       <chr> "sa_only", "sa_only"
# $ start_port_name     <chr> "WRIGHTSVILLE BEACH", "KEYS FISHERIES"
# $ end_port_name       <chr> "WRIGHTSVILLE BEACH", "KEYS FISHERIES"
# $ start_port          <chr> "361133", "118530"
# $ end_port            <chr> "362333", "111949"

# all_logbooks_db_data_2022_short_p_region_short |>
#   filter(start_port == end_port &
#            !start_port_name == end_port_name) |>
# str()
# 0

## save specific field names for future use ----
port_fields_all <-
  function() {
    # Combine specific fields with names containing "port" from the given 'variables'.
    c(dplyr::all_of(c(
      "vessel_official_number",
      "permit_region"
    )),
    dplyr::contains("port")
    # ,
    # -starts_with("notif")
    )
  }

  # Select specific columns using the 'port_fields_all' function, remove columns starting with "notif",
  # remove empty columns, and retain distinct rows.
processed_logbooks_short_port_fields_all <-
  processed_logbooks_short |>
  dplyr::select(port_fields_all(), -starts_with("notif")) |>
  remove_empty_cols() |>
  dplyr::distinct()

dim(processed_logbooks_short_port_fields_all)
# [1] 3011   11
# [1] 2475    6 (not overridden)

## add date related columns ----
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

## shorten processed_logbooks_short_dates ----
# Select specific columns using the 'port_fields_all' function,
# select columns starting with "trip_",
# remove columns starting with "notif",
# remove empty columns, and retain distinct rows.

processed_logbooks_short_dates_trip_port <-
  processed_logbooks_short_dates |>
  dplyr::select(port_fields_all(),
                dplyr::starts_with("trip_"),
                -starts_with("notif")) |>
  remove_empty_cols() |>
  dplyr::distinct()

dim(processed_logbooks_short_dates_trip_port)
# [1] 94366    29
# [1] 66641    18 not overridden

#' %%%%% Count boat numbers
#'

# How many SEFHIER vessels start at a different location than they end ----

processed_logbooks_short_start_end_diff <-
  processed_logbooks_short_dates_trip_port |>
  # filter(!start_port == end_port) |>
  dplyr::filter(!start_port_name == end_port_name) |>
  dplyr::select(
         vessel_official_number,
         permit_region) |>
  dplyr::distinct() |>
  # dim()
  # Rows: 397
  count(permit_region)

processed_logbooks_short_start_end_diff
#   permit_region   n
# 1           GOM 187
# 2            SA 172

# 1  gom_and_dual 198
# 2       sa_only 199
# by name:
# 1  gom_and_dual 198
# 2       sa_only 197

# if keep ports:
# dim()
# 499
# 1  gom_and_dual 273
# 2       sa_only 226

# How many SEFHIER vessels start at a different location than they end by quarter ----
processed_logbooks_short_dates_short <-
  processed_logbooks_short_dates |>
  dplyr::select(dplyr::all_of(port_fields_short),
                contains("quarter")) |>
  remove_empty_cols() |>
  dplyr::distinct()

dim(processed_logbooks_short_dates_short)
# [1] 6639   11
# [1] 66641    12

# to test on
# filter(vessel_official_number %in% c("FL0789TE",
#                                   "1189202")) |>

## by start quarter ----
processed_logbooks_short_start_end_diff_dates <-
  processed_logbooks_short_dates_short |>
  group_by(trip_start_year_quarter) |>
  mutate(diff_start_end_q_num =
           n_distinct(vessel_official_number)) |>
  select(trip_start_year_quarter, diff_start_end_q_num) |>
  distinct() |>
  ungroup()

processed_logbooks_short_start_end_diff_dates |>
    arrange(trip_start_year_quarter)
#   trip_start_year_quarter diff_start_end_q_num
#   <yearqtr>                              <int>
# 1 2022 Q1                                  716
# 2 2022 Q2                                 1264
# 3 2022 Q3                                 1325
# 4 2022 Q4                                  997

## by end quarter ----
names(processed_logbooks_short_dates_short)

end_processed_logbooks_short_dates_short_diff_start_from_end <-
  processed_logbooks_short_dates_short |>
  group_by(trip_end_year_quarter) |>
  mutate(diff_end_end_q_num =
           n_distinct(vessel_official_number)) |>
  select(trip_end_year_quarter, diff_end_end_q_num) |>
  distinct() |>
  ungroup()

end_processed_logbooks_short_dates_short_diff_start_from_end |>
  arrange(trip_end_year_quarter)

## count total vessels by quarter ----

processed_logbooks_short_dates_short_diff_start_from_end_tot <-
  processed_logbooks_short_dates_short |>
  group_by(trip_start_year_quarter) |>
  mutate(tot_vsl_by_start_q = n_distinct(vessel_official_number)) |>
  ungroup() |>
  group_by(trip_end_year_quarter) |>
  mutate(tot_vsl_by_end_q = n_distinct(vessel_official_number)) |>
  ungroup()

### see total vessels by 2022 start trip quarters ----
processed_logbooks_short_dates_short_diff_start_from_end_tot |>
  select(trip_start_year_quarter,
         tot_vsl_by_start_q) |>
  distinct() |>
  arrange(trip_start_year_quarter)
#   trip_start_year_quarter tot_vsl_by_start_q
#   <yearqtr>                            <int>
# 1 2022 Q1                                716
# 2 2022 Q2                               1264
# 3 2022 Q3                               1325
# 4 2022 Q4                                997

# ---


### see total vessels by 2022 end trip quarters ----
processed_logbooks_short_dates_short_diff_start_from_end_tot |>
  select(trip_end_year_quarter,
         tot_vsl_by_end_q) |>
  distinct() |>
  arrange(trip_end_year_quarter)
#   trip_start_year_quarter tot_vsl_by_start_q
#   <yearqtr>                            <int>
# 1 2022 Q1                                715
# 2 2022 Q2                               1264
# 3 2022 Q3                               1325
# 4 2022 Q4                                997

# ---

# How many vessels have variable landing locations ----
#'I.e., in the winter they are in one state while in the summer they fish in another)
#'

## multiple_start_ports ----
multiple_start_ports <-
  processed_logbooks_short |>
  dplyr::select(vessel_official_number,
                permit_region,
                start_port_name) |>
  dplyr::distinct() |>
  dplyr::add_count(vessel_official_number,
                   permit_region,
                   name = "start_port_name_cnt") |>
  dplyr::filter(start_port_name_cnt > 1) |>
  dplyr::arrange(vessel_official_number)

n_distinct(multiple_start_ports)
# vessel_official_number 675
# [1] 1146

head(multiple_start_ports)

### test multiple_start_ports ----
multiple_start_ports |>
  dplyr::filter(vessel_official_number %in% c('944064',
                                    '934665')) |>
  head()
# 2,2

## multiple_end_ports ----

multiple_end_ports <-
  processed_logbooks_short |>
  # Select specific columns, retain distinct rows,
  # add a count end_port_name column for each combination of 'vessel_official_number' and 'permit_region'.
  dplyr::select(vessel_official_number,
                permit_region,
                end_port_name) |>
  dplyr::distinct() |>
  dplyr::add_count(vessel_official_number,
                   permit_region,
                   name = "end_port_name_cnt") |>
  # Filter for rows where the count of 'end_port_name' is greater than 1,
  # and arrange the data by 'vessel_official_number'.
  dplyr::filter(end_port_name_cnt > 1) |>
  dplyr::arrange(vessel_official_number)

head(multiple_end_ports)

### test multiple_end_ports ----
multiple_end_ports |>
  dplyr::filter(vessel_official_number %in% c('944064',
                                    '934665')) |>
  head()
# 2 port names

count_uniq_by_column(multiple_end_ports)
# vessel_official_number 374
# vessel_official_number 288
# permit_region            2
# end_port_name          298
# end_port_name_cnt        6

### How many vessels have multiple end ports ----

multiple_end_ports |>
  select(vessel_official_number) |>
  distinct() |>
  count()
# 288

## multiple_end_port_states ----
# print_df_names(processed_logbooks_short_port_fields_all)

multiple_end_port_states <-
  processed_logbooks_short_port_fields_all |>
  # Select specific columns, retain distinct rows,
  # add a count end_port_state column for each combination of 'vessel_official_number' and 'permit_region'.
  dplyr::select(vessel_official_number,
                permit_region,
                end_port_state) |>
  dplyr::distinct() |>
  dplyr::add_count(vessel_official_number,
                   permit_region,
                   name = "end_port_state_cnt") |>
  # Filter for rows where the count of 'end_port_state' is greater than 1,
  # and arrange the data by 'vessel_official_number'.
  dplyr::filter(end_port_state_cnt > 1) |>
  dplyr::arrange(vessel_official_number)

n_distinct(multiple_end_port_states$vessel_official_number)
# count_uniq_by_column()
# vessel_official_number 76
# 56

head(multiple_end_port_states)

### How many vessels have multiple end port states ----
n_distinct(multiple_end_port_states$vessel_official_number)
# 56

## by quarter ----
#
# processed_logbooks_short_all_port_names_by_vsl |>
#   filter(vessel_official_number == 1000042) |>
#   glimpse()

### trips and quarter fields only ----

# Select specific columns using 'port_fields_all' function,
# remove columns starting with "notif",
# and retain columns related to trip start and end quarter information.

processed_logbooks_short_dates_trip_port_short <-
  processed_logbooks_short_dates_trip_port |>
  dplyr::select(
    port_fields_all(),
    -starts_with("notif"),
    c(trip_start_year_quarter,
      trip_start_quarter_num,
      trip_end_year_quarter,
      trip_end_quarter_num,)
  ) |>
  remove_empty_cols() |>
  dplyr::distinct()

dim(processed_logbooks_short_dates_trip_port_short)
# [1] 6639   15
# [1] 5303   14

count_uniq_by_column(processed_logbooks_short_dates_trip_port_short)
# vessel_official_number     1876
# permit_region              2
# start_port_name          531
# end_port_name            529

### a quarter is the same, a port - diff ----

# test
processed_logbooks_short_dates_trip_port_short |>
  dplyr::filter(vessel_official_number == "1057052") |>
  dplyr::arrange(trip_start_quarter_num) |>
  dplyr::glimpse()

# Select columns for trip start, retain distinct rows.
start_ports_q_short <-
  processed_logbooks_short_dates_trip_port_short |>
  dplyr::select(vessel_official_number,
         permit_region,
         trip_start_year_quarter,
         start_port_county) |>
  dplyr::distinct()

# Select columns for trip end, retain distinct rows.
# end_ports_q_short <-
#   processed_logbooks_short_dates_trip_port_short |>
#   dplyr::select(vessel_official_number,
#          permit_region,
#          trip_end_year_quarter,
#          end_port_name) |>
#   dplyr::distinct()

end_ports_q_short <-
  processed_logbooks_short_dates_trip_port_short |>
  dplyr::select(vessel_official_number,
         permit_region,
         trip_end_year_quarter,
         end_port_county) |>
  dplyr::distinct()


dim(start_ports_q_short)
# [1] 6317    4
# [1] 5092    4
dim(end_ports_q_short)
# [1] 5845    4
# [1] 4760    4
# [1] 4482    4 county

count_uniq_by_column(start_ports_q_short)
# vessel_official_number     1876
# start_port_name          531
# start_port_county         87

# vessel_official_number  1629
# start_port_name          511

n_distinct(end_ports_q_short$end_port_county)
# 529
# 492 port name
# [1] 92 county

### melt and decast the table ----

# Make one row per vessel
# In summary, the code transforms the data from a long to a wide format, spreading the values from the 'start_port_name' column across columns named by 'trip_start_year_quarter', and aggregating multiple values into a comma-separated string.

# The pivot_wider() function from the tidyr package is used to reshape the data.
# It takes the 'vessel_official_number' column as the identifier columns,
# and then it spreads the values from the 'trip_start_year_quarter' column into
# separate columns, with the corresponding values being taken from the
# 'start_port_name' column.
# The values_fn parameter is specified to define how to handle multiple
# values that may exist for a combination of 'vessel_official_number' and
# 'trip_start_year_quarter'. In this case, unique values are sorted and
# concatenated into a comma-separated string.

# Define a function 'each_quarter_a_col' that takes a data frame 'my_df' and an argument 'start_or_end' with a default value "start".
# TODO: port_county in port_field_name as an argument
each_quarter_a_col <-
  function(my_df,
           start_or_end = "start") {

    # Generate field names dynamically using 'stringr::str_glue'.
    quarter_field_name <-
      stringr::str_glue("trip_{start_or_end}_year_quarter")
    port_field_name <-
      stringr::str_glue("{start_or_end}_port_county")

    # Reshape the data frame 'my_df' by widening it based on unique combinations of vessel, region, and quarter.
    ports_q_short_wider <-
      my_df |>
      tidyr::pivot_wider(
        id_cols = c(vessel_official_number, permit_region),
        names_from = !!sym(quarter_field_name),
        values_from = !!sym(port_field_name),
        values_fn = ~ paste(unique(sort(.x)), collapse = ",")
      )

    # Return the widened data frame.
    return(ports_q_short_wider)
  }

#' Explanation:
#'
#' 1. The function takes a data frame (`my_df`) and an optional argument (`start_or_end`) indicating whether it's for trip start or end.
#'
#' 2. Dynamically generate field names for the quarter and port based on the input.
#'
#' 3. Use the `pivot_wider` function to reshape the data frame by creating new columns for each unique combination of vessel, region, and quarter.
#'
#' 4. The `values_fn` argument ensures that duplicate values are concatenated and sorted within each cell.
#'
#' 5. Return the resulting widened data frame.
#'

### use the function for trip start and end ports separately ----

# Create a new list 'ports_q_short_wider_list' by applying a series of operations to each element
# of the provided list, where each element is a pair of a data frame and a character indicating start or end.
ports_q_short_wider_list <-
  list(c(start_ports_q_short, "start"),
       c(end_ports_q_short, "end")) |>
  purrr::map(\(one_df_l) {
    # Extract the data frame and the character indicating start or end.
    df_len <- length(one_df_l)
    my_df <-
      one_df_l[1:df_len - 1] |>
      as.data.frame()
    my_col_name <- one_df_l[[df_len]]
    # Apply the 'each_quarter_a_col' function to reshape the data frame based on quarters.
    each_quarter_a_col(my_df, my_col_name)
  })

#' Explanation:
#'
#' 1. Create a list where each element is a pair containing a data frame ("start_ports_q_short" or "end_ports_q_short") and a character ("start" or "end").
#'
#' 2. Use the "map" function to apply a series of operations to each element of the list.
#'
#' 3. Extract the data frame and the character from each pair.
#'
#' 4. Convert the extracted data frame to a standard data frame using "as.data.frame()".
#'
#' 5. Apply the "each_quarter_a_col" function to reshape the data frame based on quarters, using the extracted character as an argument.
#'
#' 6. The resulting list, "ports_q_short_wider_list", contains data frames widened based on quarters for both start and end ports.
#'

# glimpse(ports_q_short_wider_list)


# start_ports_q_short_wider <-
#   start_ports_q_short |>
#   pivot_wider(
#     id_cols = c(vessel_official_number, permit_region),
#     names_from = trip_start_year_quarter,
#     values_from = start_port_name,
#     values_fn = ~ paste(unique(sort(.x)), collapse = ",")
#   )

head(ports_q_short_wider_list[[1]])

### add column for the same or diff ----
#### create an auxiliary function ----

#' It starts by using the rowwise() function to apply subsequent operations
#' to each row individually, ensuring that calculations are row-wise.
#'
#' The mutate() function is then used to create a new column 'same'.
#'
#' This column is assigned the result of a logical comparison:
#'
#' It checks if the number of distinct values in all columns that start with '2022'
#' is equal to 1, indicating that all these columns have the same value for a given row.
#'
#' The ungroup() function is then applied to remove the grouping structure introduced by rowwise().
#'
#' rlang::\`:=\` used inside mutate() to work with dynamic field names (!!ports_num_field_name), e.g. a variable name instead of a string
#'

make_ports_q_short_wider_diff <-
  function(my_df,
           start_or_end = "start") {

    ports_num_field_name <-
      stringr::str_glue("all_{start_or_end}_ports_num")

    ports_field_name <-
      stringr::str_glue("all_{start_or_end}_county")

    ports_q_short_wider_diff <-
      my_df |>
      dplyr::rowwise() |>
      dplyr::mutate(!!ports_num_field_name :=
               dplyr::n_distinct(unlist(dplyr::across(
                 dplyr::starts_with('2022')
               ))),
             !!ports_field_name :=
               list(paste(unique(sort(
                 unlist(dplyr::across(dplyr::starts_with('2022')))
               )),
               sep = ","))) |>
      dplyr::mutate(same =
                      dplyr::n_distinct(unlist(dplyr::across(
        dplyr::starts_with('2022'),
        ~ as.character(.x)
      ))) == 1) |>
      dplyr::ungroup()

    return(ports_q_short_wider_diff)
  }

# View(ports_q_short_wider_list)

#### use the auxiliary function ----
tic("ports_q_short_wider_list_diff")

# Create a new list 'ports_q_short_wider_list_diff' by applying a series of operations to each element
# of the provided list, where each element is a pair of a data frame and a character indicating start or end.
ports_q_short_wider_list_diff <-
  list(c(ports_q_short_wider_list[[1]], "start"),
       c(ports_q_short_wider_list[[2]], "end")) |>
  purrr::map(\(one_df_l) {
    # Extract the data frame and the character indicating start or end.
    df_len <- length(one_df_l)
    my_df_names <- names(one_df_l)[1:df_len - 1]
    my_df <- one_df_l[1:df_len - 1] |>
      as.data.frame()
    names(my_df) <- my_df_names
    my_col_name <- one_df_l[[df_len]]
    # Apply the 'make_ports_q_short_wider_diff' function to calculate the differences between quarters.
    make_ports_q_short_wider_diff(my_df, my_col_name)
  })

#' Explanation:
#'
#' 1. Create a list where each element is a pair containing a data frame from "ports_q_short_wider_list" and a character ("start" or "end").
#'
#' 2. Use the "map" function to apply a series of operations to each element of the list.
#'
#' 3. Extract the data frame and the character from each pair.
#'
#' 4. Extract the column names of the data frame and assign them to "my_df_names".
#'
#' 5. Convert the extracted data frame to a standard data frame using "as.data.frame()" and set its column names.
#'
#' 6. Apply the "make_ports_q_short_wider_diff" function to calculate the differences between quarters.
#'
#' 7. The resulting list, "ports_q_short_wider_list_diff", contains data frames with differences between quarters for both start and end ports.
#'

toc()
# ports_q_short_wider_list_diff: 9.93 sec elapsed

# diffdf::diffdf(start_ports_q_short_wider_diff, ports_q_short_wider_list_diff[[1]])

#### check the result ----
ports_q_short_wider_list_diff[[1]] |>
  dplyr::filter(vessel_official_number == "1171256") |>
  dplyr::glimpse()
# $ all_start_ports_num <int> 3
# $ all_start_ports     <list> <"BARNEGAT,MORRISON'S MARINA AND  SHIPS STORE", "M…
# $ same                <lgl> FALSE

names(ports_q_short_wider_list_diff) <-
  c("start", "end")

ports_q_short_wider_list_diff |>
  purrr::map(count_uniq_by_column)
# start:
# vessel_official_number 1876
# 2022 Q3              484
# 2022 Q4              356
# 2022 Q2              488
# 2022 Q1              290
# all_start_ports      697

# end:
# vessel_official_number 1876
# permit_region          2
# 2022 Q3              497
# 2022 Q4              360
# 2022 Q2              497
# 2022 Q1              290
# 2023 Q1                7
# 2023 Q2                3
# 2018 Q2                2
# 2021 Q2                3
# 2020 Q4                2
# all_end_ports        701

### count same or diff trip start or end ----

ports_q_short_wider_list_diff_cnts <-
  ports_q_short_wider_list_diff |>
  purrr::map(\(one_df) {
    one_df |>
      dplyr::select(vessel_official_number, same) |>
      dplyr::count(same)
  })
# start
# 1 FALSE  1421 (incl. NAs)
# 2 TRUE    455

# end
# 1 FALSE  1391 (incl. NAs)
# 2 TRUE    485

# use pander for .qmd
pander(ports_q_short_wider_list_diff_cnts)

### count same or diff by permit_region and trip start or end ----
ports_q_short_wider_list_diff_cnt_p_r <-
  ports_q_short_wider_list_diff |>
  purrr::map(\(one_df) {
    one_df |>
      dplyr::count(permit_region, same)
  })

# use pander for .qmd
pander(ports_q_short_wider_list_diff_cnt_p_r)

# start
#   permit_region same      n
#   <chr>         <lgl> <int>
# 1 gom_and_dual  FALSE   569
# 2 gom_and_dual  TRUE    222
# 3 sa_only       FALSE   852
# 4 sa_only       TRUE    233

# end
#   permit_region same      n
#   <chr>         <lgl> <int>
# 1 gom_and_dual  FALSE   561
# 2 gom_and_dual  TRUE    230
# 3 sa_only       FALSE   830
# 4 sa_only       TRUE    255

print_df_names(processed_logbooks_short_dates)

# Quantify the # of vessels who fish in both the gulf and S Atl.  ----
## add a gom vs sa marker to ports ----

### add full state name ----
processed_logbooks_short_dates |>
  dplyr::select(end_port_state) |>
  dplyr::distinct() |>
  head(2)
# "FL", "DE"
# "FL", NA

names(state.abb) <- state.name
names(state.name) <- state.abb

my_state_name[tolower("FL")]
# "Florida"

### add a start_ and end_ port_reg columns ----

processed_logbooks_short_port_states <-
  processed_logbooks_short_dates |>
  # Add columns for the state names corresponding to start and end ports.
  dplyr::mutate(start_port_state_name =
                  my_state_name[tolower(start_port_state)],
                end_port_state_name   =
                  my_state_name[tolower(end_port_state)]) |>
  # Add a column 'start_port_reg' based on conditions using 'case_when'.
  dplyr::mutate(
    start_port_reg =
      dplyr::case_when(
        tolower(start_port_state_name) %in% tolower(sa_council_states) ~
          "sa_council_state",
        tolower(end_port_state_name) %in% tolower(east_coast_states$gom) ~
          "gom_state",
        .default = "sa_state"
      )
  ) |>
  # the same for end ports
  dplyr::mutate(
    end_port_reg =
      dplyr::case_when(
        tolower(end_port_state_name) %in% tolower(sa_council_states) ~
          "sa_council_state",
        tolower(end_port_state_name) %in% tolower(east_coast_states$gom) ~
          "gom_state",
        .default = "sa_state"
      )
  )

#' Explanation:
#'
#' 1. Create a new data frame "processed_logbooks_short_port_states" by applying operations to "processed_logbooks_short_port_region".
#'
#' 2. Add columns for the state names corresponding to start and end ports.
#'
#' 3. Add a new column "start_port_reg" based on conditions using "case_when":
#'
#'    - If the start port state is in "sa_council_states", set it to "sa_council_state".
#'
#'    - If the end port state is in "east_coast_states$gom", set it to "gom_state".
#'
#'    - For other cases, set it to "sa_state".
#'
#'    - Repeat for the "end" port names
#'

print_df_names(processed_logbooks_short_port_states)
# [1] 3011   14
# [1] 66641    26

### if FL divide by county ----
#' Add a new column "end_port_fl_reg" based on conditions using "case_when":
#'
#' If the end port state name is "florida" and the end port county is in "fl_counties$gom", set it to "gom_county". The same for "ends"
#'
#' For other cases, set it to "sa_county".
#'

processed_logbooks_short_port_states_fl_reg <-
  processed_logbooks_short_port_states |>
  dplyr::mutate(
    start_port_fl_reg =
      dplyr::case_when(
        tolower(start_port_state_name) == "florida" &
          tolower(start_port_county) %in% tolower(fl_counties$gom) ~
          "gom_county",
        .default = "sa_county"
      )
  ) |>
  dplyr::mutate(
    end_port_fl_reg =
      dplyr::case_when(
        tolower(end_port_state_name) == "florida" &
          tolower(end_port_county) %in% tolower(fl_counties$gom) ~
          "gom_county",
        .default = "sa_county"
      )
  )

print_df_names(processed_logbooks_short_port_states_fl_reg)

processed_logbooks_short_port_states_fl_reg |>
  filter(start_port_fl_reg == "gom_county") |>
  dim()
# [1] 844  17
# [1] 28477    28

processed_logbooks_short_port_states_fl_reg |>
  filter(end_port_fl_reg == "gom_county") |>
  dim()
# 1381   17
# [1] 37868    28

dplyr::glimpse(processed_logbooks_short_port_states_fl_reg)

### create one_port_marker ----
#' Combine all previous region markers into one column
#'
#' NB. If Monroe, FL divide by vessel permit_region
#'

processed_logbooks_short_port_states_fl_reg_start <-
  processed_logbooks_short_port_states_fl_reg |>
  dplyr::mutate(
    one_start_port_marker =
      dplyr::case_when(
        start_port_county == "MONROE" &
          permit_region == "gom_and_dual" ~
          "gom",
        start_port_county == "MONROE" &
          permit_region == "sa_only" ~
          "sa",
        start_port_state == "FL" &
          start_port_fl_reg == "gom_county" ~
          "gom",
        start_port_state == "FL" &
          start_port_fl_reg == "sa_county" ~
          "sa",
        start_port_reg %in% c("gom_council_state",
                              "gom_state") ~
          "gom",
        start_port_reg %in% c("sa_council_state",
                              "sa_state") ~
          "sa",
        .default = NA
      )
  )

#' Explanation:
#'
#' 1. Create a new data frame "processed_logbooks_short_port_states_fl_reg_start" by applying operations to "processed_logbooks_short_port_states_fl_reg".
#'
#' 2. Add a new column "one_start_port_marker" based on conditions using "case_when":
#'
#'    - If the start port county is "MONROE" and the permit region is "gom_and_dual", set it to "gom".
#'
#'    - If the start port county is "MONROE" and the permit region is "sa_only", set it to "sa".
#'
#'    - If the start port state is "FL" and the start port FL region is "gom_county", set it to "gom".
#'
#'    - If the start port state is "FL" and the start port FL region is "sa_county", set it to "sa".
#'
#'    - If the start port region is "gom_council_state" or "gom_state", set it to "gom".
#'
#'    - If the start port region is "sa_council_state" or "sa_state", set it to "sa".
#'    - For other cases, set it to NA.
#'

# The same for end_ports
processed_logbooks_short_port_states_fl_reg_end <-
  processed_logbooks_short_port_states_fl_reg |>
  dplyr::mutate(
    one_end_port_marker =
      dplyr::case_when(
        end_port_county == "MONROE" &
          permit_region == "gom_and_dual" ~
          "gom",
        end_port_county == "MONROE" &
          permit_region == "sa_only" ~
          "sa",
        end_port_state == "FL" &
          end_port_fl_reg == "gom_county" ~
          "gom",
        end_port_state == "FL" &
          end_port_fl_reg == "sa_county" ~
          "sa",
        end_port_reg %in% c("gom_council_state",
                              "gom_state") ~
          "gom",
        end_port_reg %in% c("sa_council_state",
                              "sa_state") ~
          "sa",
        .default = NA
      )
  )

dim(processed_logbooks_short_port_states_fl_reg_start)
# [1] 3011   18
# [1] 66641    29

dim(processed_logbooks_short_port_states_fl_reg_end)
# [1] 3011   18
# [1] 66641    29

#### check if all start ports have a permit region ----
processed_logbooks_short_port_states_fl_reg_start |>
  dplyr::filter(is.na(one_start_port_marker)) |>
  select(start_port_reg) |>
  distinct() |>
  dim()
# 0 OK

processed_logbooks_short_port_states_fl_reg_end |>
  select(end_port_reg) |>
  distinct()
# 1 sa_council_state
# 2         sa_state
# 3        gom_state

processed_logbooks_short_port_states_fl_reg_end |>
  dplyr::filter(is.na(one_end_port_marker)) |>
  distinct() |>
  dim()
# 0 29 OK

processed_logbooks_short_port_states_fl_reg_start |>
    dplyr::filter(one_start_port_marker == "gom") |>
    dplyr::select(vessel_official_number, contains("start")) |>
    dplyr::distinct() |>
    dplyr::glimpse()

### Count vessels having both sa and gom port_markers to find the num of vessels who fish in both the Gulf and S Atl ----

# #### shorten processed_logbooks_short_port_states_fl_reg_start
# keep_port_reg_column_names <- c(
#     "vessel_official_number",
#     "start_port",
#     "start_port_name",
#     "start_port_county",
#     "start_port_state",
#     "end_port",
#     "end_port_name",
#     "end_port_county",
#     "end_port_state",
#     "permit_region",
#     "start_port_state_name",
#     "end_port_state_name",
#     "one_start_port_marker",
#     "one_end_port_marker"
# )


# setdiff(names(processed_logbooks_short_port_states_fl_reg_start),
#         keep_port_reg_column_names)
# # Using any_of here to choose "one_start_port_marker" or "one_end_port_marker"
# processed_logbooks_short_port_states_fl_reg_start_short <-
#   processed_logbooks_short_port_states_fl_reg_start |>
#   dplyr::select(any_of(keep_port_reg_column_names)) |>
#   dplyr::distinct()

# processed_logbooks_short_port_states_fl_reg_end_short <-
#   processed_logbooks_short_port_states_fl_reg_end |>
#   dplyr::select(any_of(keep_port_reg_column_names)) |>
#   dplyr::distinct()

# count_uniq_by_column(processed_logbooks_short_port_states_fl_reg_start_short)
# [1] 3011   14
# vessel_official_number   1876
# start_port             536
# start_port_name        531
# processed logbooks
# vessel_official_number 1629
# start_port              516
# start_port_name         511

# count_uniq_by_column(processed_logbooks_short_port_states_fl_reg_end_short)
# end_port               534
# end_port_name          529
# processed logbooks
# end_port                497
# end_port_name           492

count_uniq_by_column(processed_logbooks_short_port_states_fl_reg_start)
# vessel_official_number  1629
# start_port_name          511
# end_port_name            492

count_uniq_by_column(processed_logbooks_short_port_states_fl_reg_end)
# vessel_official_number  1629
# end_port_name            492
# start_port_name          511

#### Count vessels with each GOM or SA trip start and end port region marker ----
#' (the occurrences of each unique value in the "one_start_port_marker" and "one_end_port_marker" columns).
#'

processed_logbooks_short_port_states_fl_reg_start |>
  dplyr::select(vessel_official_number, one_start_port_marker) |>
  dplyr::distinct() |>
  dplyr::count(one_start_port_marker)
# 1                   gom 1000
# 2                    sa 1029

# no overridden, not exclude Jeannett's, use metrics tracking regions
# 1                   gom 1023
# 2                    sa  655

processed_logbooks_short_port_states_fl_reg_end |>
  dplyr::select(vessel_official_number, one_end_port_marker) |>
  dplyr::distinct() |>
  dplyr::count(one_end_port_marker)
#   one_end_port_marker    n
# 1                 gom  819
# 2                  sa 1069

# 1                 gom 983
# 2                  sa 653

#### Count vessels with each GOM or SA trip start and end port region marker per vessel permit_region ----

processed_logbooks_short_port_states_fl_reg_start |>
  dplyr::select(vessel_official_number,
         permit_region,
         one_start_port_marker) |>
  dplyr::distinct() |>
  dplyr::count(permit_region, one_start_port_marker)
# 1  gom_and_dual                   gom  784
# 2  gom_and_dual                    sa   16
# 3       sa_only                   gom  216
# 4       sa_only                    sa 1013

#   permit_region one_start_port_marker   n
# 1           GOM                   gom 749
# 2           GOM                    sa  13
# 3            SA                   gom 274
# 4            SA                    sa 642

processed_logbooks_short_port_states_fl_reg_end |>
  dplyr::select(vessel_official_number,
         permit_region,
         one_end_port_marker) |>
  dplyr::distinct() |>
  dplyr::count(permit_region, one_end_port_marker)
# 2  gom_and_dual                  sa   15
# 3       sa_only                 gom   35
# 4       sa_only                  sa 1054

#   permit_region one_end_port_marker   n
# 1           GOM                 gom 749
# 2           GOM                  sa   8
# 3            SA                 gom 234
# 4            SA                  sa 645

## Trip start ports are in both regions ----
select_vessel_mark_start <-
  c("vessel_official_number",
    "one_start_port_marker")

start_ports_region_cnt <-
  processed_logbooks_short_port_states_fl_reg_start |>
  dplyr::select(dplyr::all_of(select_vessel_mark_start)) |>
  dplyr::distinct() |>
  dplyr::group_by(vessel_official_number) |>
  dplyr::mutate(
    vessel_one_start_port_marker_num =
      dplyr::n_distinct(one_start_port_marker,
                        na.rm = TRUE)
  ) |>
  ungroup()

#' Explanation:
#'
#' 1. Create a new data frame "start_ports_region_cnt" by applying operations to "processed_logbooks_short_port_states_fl_reg_start".
#'
#' 2. Select columns based on "select_vessel_mark_start".
#'
#' 3. Retain distinct rows.
#'
#' 4. Group the data by "vessel_official_number".
#'
#' 5. Add a new column "vessel_one_start_port_marker_num" counting the distinct values of "one_start_port_marker" for each vessel.
#'
#' 6. Ungroup the data.
#'

##### check start_ports_region_cnt ----
# [1] 2029    3

count_uniq_by_column(start_ports_region_cnt)
# vessel_official_number              1876
# one_start_port_marker               2
# vessel_one_start_port_marker_num    2

# vessel_official_number           1629
# one_start_port_marker               2
# vessel_one_start_port_marker_num    2

start_ports_region_cnt |>
  dplyr::filter(vessel_official_number == "1021879")
# 1 1021879                sa                1

#### How many vessels have start port in one or in both regions ----
start_ports_region_cnt |>
  count(vessel_one_start_port_marker_num)
#   vessel_one_start_port_marker_num     n
#                              <int> <int>
# 1                                1  1723
# 2                                2   306

# 1                               1580
# 2                                98

#### rename to NO/YES ----
start_ports_region_cnt |>
  mutate(
    start_ports_in_both_GOM_and_SA =
      case_when(vessel_one_start_port_marker_num == 1 ~ "NO",
                .default = "YES")
  ) |>
  count(start_ports_in_both_GOM_and_SA)
# 1 NO                              1580
# 2 YES                               98


## Trip end ports are in both regions ----
select_vessel_mark_end <-
  c("vessel_official_number",
    "one_end_port_marker")

end_ports_region_cnt <-
  processed_logbooks_short_port_states_fl_reg_end |>
  dplyr::select(dplyr::all_of(select_vessel_mark_end)) |>
  dplyr::distinct() |>
  dplyr::group_by(vessel_official_number) |>
  dplyr::mutate(
    vessel_one_end_port_marker_num =
      dplyr::n_distinct(one_end_port_marker,
                        na.rm = TRUE)
  ) |>
  ungroup()

#### check end_ports_region_cnt ----
end_ports_region_cnt |>
  filter(vessel_official_number == "FL8905LP")
# 1 FL8905LP               gom
1

### How many vessels have end port in one or in both regions ----
end_ports_region_cnt |>
  mutate(
    end_ports_in_both_GOM_and_SA =
      case_when(vessel_one_end_port_marker_num == 1 ~ "NO",
                .default = "YES")
  ) |>
  count(end_ports_in_both_GOM_and_SA)
# 1                              1  1864
# 2                              2    24
# 1 NO                            1622
# 2 YES                             14

## Trip start and end ports are in both regions count by vessel permit ----
### Start ports ----
start_ports_region_cnt_by_permit_r <-
  processed_logbooks_short_port_states_fl_reg_start |>
  dplyr::select(dplyr::all_of(select_vessel_mark_start),
                permit_region) |>
  dplyr::distinct() |>
  dplyr::group_by(vessel_official_number, permit_region) |>
  dplyr::mutate(vessel_one_start_port_marker_num =
           dplyr::n_distinct(one_start_port_marker,
                      na.rm = TRUE)) |>
  dplyr::ungroup()

#' Explanation:
#'
#' 1. Create a new data frame "start_ports_region_cnt_by_permit_r" by applying operations to "processed_logbooks_short_port_states_fl_reg_start".
#'
#' 2. Select columns based on "select_vessel_mark_start" and "permit_region".
#'
#' 3. Retain distinct rows.
#'
#' 4. Group the data by "vessel_official_number" and "permit_region".
#'
#' 5. Add a new column "vessel_one_start_port_marker_num" counting the distinct values of "one_start_port_marker" for each vessel within each permit region.
#'
#' 6. Ungroup the data.
#'

### The same for end ports ----
end_ports_region_cnt_by_permit_r <-
  processed_logbooks_short_port_states_fl_reg_end |>
  dplyr::select(dplyr::all_of(select_vessel_mark_end),
                permit_region) |>
  dplyr::distinct() |>
  dplyr::group_by(vessel_official_number, permit_region) |>
  dplyr::mutate(vessel_one_end_port_marker_num =
           dplyr::n_distinct(one_end_port_marker,
                      na.rm = TRUE)) |>
  dplyr::ungroup()

#### check start_  and end_ ports_region_cnt_by_permit_r ----
start_ports_region_cnt_by_permit_r |>
  # filter(vessel_one_start_port_marker_num > 1) |>
  dplyr::filter(vessel_official_number == "FL6069PT") |>
  dplyr::glimpse()

end_ports_region_cnt_by_permit_r |>
  # filter(vessel_one_end_port_marker_num > 1) |>
  dplyr::filter(vessel_official_number == "FL8905LP") |>
  dplyr::glimpse()
# $ vessel_official_number            <chr> "FL8905LP", "FL8905LP"
# $ one_end_port_marker            <chr> "gom", "sa"
# $ permit_region                  <chr> "sa_only", "sa_only"
# $ vessel_one_end_port_marker_num <int> 2, 2

#### Count multiple start regions by permit_region ----
start_ports_region_cnt_by_permit_r |>
  # glimpse()
  dplyr::select(
    vessel_official_number,
    permit_region,
    vessel_one_start_port_marker_num
  ) |>
  dplyr::distinct() |>
# vessel_official_number              1876
  dplyr::count(permit_region,
        start_in_both_regions =
          vessel_one_start_port_marker_num > 1)
  # count(wt = n)
# 1876
# 1629

#   permit_region start_in_both_regions     n
#   <chr>         <lgl>       <int>
# 1 gom_and_dual  FALSE         782
# 2 gom_and_dual  TRUE            9
# 3 sa_only       FALSE         941
# 4 sa_only       TRUE          144

  # permit_region start_in_both_regions     n
# 1 GOM           FALSE                   746
# 2 GOM           TRUE                      8
# 3 SA            FALSE                   834
# 4 SA            TRUE                     41


#### Count multiple end regions by permit_region ----
end_ports_region_cnt_by_permit_r |>
  # glimpse()
  dplyr::select(
    vessel_official_number,
    permit_region,
    vessel_one_end_port_marker_num
  ) |>
  dplyr::distinct() |>
  # count_uniq_by_column()
  # vessel_official_number              1876
  dplyr::count(permit_region,
        end_in_both_regions = vessel_one_end_port_marker_num > 1)
  # |>
  # count(wt = n)
  # 1  1876

#   permit_region end_in_both_regions     n
#   <chr>         <lgl>       <int>
# 1 gom_and_dual  FALSE       783
# 2 gom_and_dual  TRUE          8
# 3 sa_only       FALSE      1081
# 4 sa_only       TRUE          4

# 1 GOM           FALSE                 751
# 2 GOM           TRUE                    3
# 3 SA            FALSE                 871
# 4 SA            TRUE                    4

# Look at permit home port vs where they take trip ----

## prepare home_port data ----
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

count_uniq_by_column(vessel_permit_port_info)
# with exp_date:
# [1] 12238     8
# SERO_OFFICIAL_NUMBER  5220
# SERO_HOME_PORT_CITY    809

### add permit and vessel info ----
# should do here, before the join, bc if there are empty rows after merge sa_only is wrongly assigned

vessel_permit_port_info_perm_reg <-
  vessel_permit_port_info |>
  dplyr::group_by(VESSEL_VESSEL_ID) |>
  dplyr::mutate(all_permits = toString(unique(sort(TOP)))) |>
  separate_permits_into_3_groups(permit_group_field_name = "all_permits") |>
  select(
    PERMIT_VESSEL_ID,
    VESSEL_VESSEL_ID,
    # PORT_CODE, mostly empty
    permit_sa_gom,
    SERO_HOME_PORT_CITY,
    SERO_HOME_PORT_COUNTY,
    SERO_HOME_PORT_STATE,
    SERO_OFFICIAL_NUMBER
  ) |>
  dplyr::ungroup() |>
  remove_empty_cols() |>
  dplyr::distinct()

n_distinct(vessel_permit_port_info_perm_reg$VESSEL_VESSEL_ID)
# [1] 5220    7
# VESSEL_VESSEL_ID      5220

vessel_permit_port_info_perm_reg |>
  dplyr::filter(permit_sa_gom == "sa_only") |>
  dplyr::distinct() |>
  dplyr::glimpse()

#' NB. 1. There is incorrect home port info (city/county/state).
#'
#' 2. permit information is some times different from that in logbooks (trips)
#'
#' 3. In logbooks there are trip end dates in weird years
#'
#' 2018 Q2                2
#' 2021 Q2                3
#' 2020 Q4                2


## add vessel_permit information to trip (logbook) information ----
# print_df_names(processed_logbooks_short_port_states_fl_reg_start)
# print_df_names(vessel_permit_port_info_perm_reg)

join_vessel_and_trip_start <-
  dplyr::left_join(
    processed_logbooks_short_port_states_fl_reg_start,
    vessel_permit_port_info_perm_reg,
    dplyr::join_by(vessel_official_number ==
                     SERO_OFFICIAL_NUMBER)
  )

join_vessel_and_trip_end <-
  dplyr::left_join(
    processed_logbooks_short_port_states_fl_reg_end,
    vessel_permit_port_info_perm_reg,
    dplyr::join_by(vessel_official_number ==
                     SERO_OFFICIAL_NUMBER)
  )

data_overview(join_vessel_and_trip_end)
# [1] 3011   20
# [1] 2475   19 (processed logbooks)
# [1] 66641    35

# vessel_id             1876
# vessel_official_number   1876
# permit_sa_gom            4
# SERO_OFFICIAL_NUMBER  1785

# vessel_official_number 1629
# permit_region             2
# PERMIT_VESSEL_ID        1562

dim(processed_logbooks_short_port_states_fl_reg_end)
# [1] 3011   14
# [1] 2475   13
# [1] 66641    29

### check permit_regions ----
join_vessel_and_trip_end |>
  dplyr::filter(!permit_region == permit_sa_gom) |>
  dplyr::select(permit_region, permit_sa_gom) |>
  dplyr::distinct() |>
  dplyr::arrange(permit_region)
#   permit_region permit_sa_gom
# 1  gom_and_dual          dual
# 2  gom_and_dual      gom_only

# 3  gom_and_dual       sa_only
# 4       sa_only          dual
# 5       sa_only      gom_only

#   permit_region permit_sa_gom
# 1           GOM      gom_only
# 2           GOM          dual
# 3           GOM       sa_only
# 4            SA       sa_only
# 5            SA          dual

#' TODO: compare regions, why diff. Permits in logbooks (trips) and in permit data are different
#'

join_vessel_and_trip_end |>
  dplyr::filter(permit_region == "gom_and_dual" &
                  permit_sa_gom == "sa_only") |>
  dim()
# 6
# 0 35

join_vessel_and_trip_end |>
  dplyr::filter(permit_region == "sa_only" &
                  permit_sa_gom == "dual") |>
  dim()
# 40
# 0 19

join_vessel_and_trip_end |>
  dplyr::filter(permit_region == "sa_only" &
           permit_sa_gom == "gom_only") |>
  dim()
# FL1921PM
# PIMS:
# Home port
# PENSACOLA, FL
# dual

# 0 35 processed data

# vessel_id, vessel_official_number, start_port, start_port_name, start_port_county, start_port_state, end_port, end_port_name, end_port_county, end_port_state, permit_region, start_port_state_name, end_port_state_name, one_start_port_marker, PERMIT_VESSEL_ID, permit_sa_gom, SERO_HOME_PORT_CITY, SERO_HOME_PORT_COUNTY, SERO_HOME_PORT_STATE, SERO_OFFICIAL_NUMBER

### By start port: add columns for different start and home port names, counties and states ----
tic("join_vessel_and_trip_start_port_diff")
join_vessel_and_trip_start_port_diff <-
  join_vessel_and_trip_start |>
  dplyr::group_by(vessel_official_number) |>
  dplyr::mutate(
    diff_start_port_state =
      dplyr::case_when(
        !tolower(start_port_state) == tolower(SERO_HOME_PORT_STATE) ~
          "yes",
        .default = "no"
      ),
    diff_start_port_county =
      dplyr::case_when(
        !tolower(start_port_county) == tolower(SERO_HOME_PORT_COUNTY) ~
          "yes",
        .default = "no"
      ),
    diff_start_port_name_or_city =
      dplyr::case_when(
        !tolower(start_port_name) == tolower(SERO_HOME_PORT_CITY) ~
          "yes",
        .default = "no"
      )
  ) |>
  mutate(
    diff_start_port_state =
      dplyr::case_when(
        !tolower(start_port_state) == tolower(SERO_HOME_PORT_STATE) ~
          "yes",
        .default = "no"
      ),
    diff_start_port_county =
      dplyr::case_when(
        !tolower(start_port_county) == tolower(SERO_HOME_PORT_COUNTY) ~
          "yes",
        .default = "no"
      ),
    diff_start_port_name_or_city =
      dplyr::case_when(
        !tolower(start_port_name) == tolower(SERO_HOME_PORT_CITY) ~
          "yes",
        .default = "no"
      )
  ) |>
  dplyr::ungroup()
toc()
# join_vessel_and_trip_start_port_diff: 1.74 sec elapsed

#' Explanation:
#'
#' 1. Start measuring the execution time using the `tic()` function.
#'
#' 2. Create a new data frame "join_vessel_and_trip_start_port_diff" by applying operations to "join_vessel_and_trip_start".
#'
#' 3. Group the data by "vessel_official_number".
#'
#' 4. Add columns indicating differences between home port and trip start/end ports for both start and end ports.
#'
#' 5. Ungroup the data.
#'
#' 6. Stop measuring the execution time using the `toc()` function and display the elapsed time.
#'

join_vessel_and_trip_start_port_diff |>
  dplyr::select(vessel_official_number,
         dplyr::starts_with("diff")) |>
  dplyr::distinct() |>
  dim()
# [1] 2591    7
# [1] 2145    7
# [1] 2015    4

### By end port: add columns for different end and home port names, counties and states ----
tic("join_vessel_and_trip_end_port_diff")
join_vessel_and_trip_end_port_diff <-
  join_vessel_and_trip_end |>
  dplyr::group_by(vessel_official_number) |>
  dplyr::mutate(
    diff_end_port_state =
      dplyr::case_when(
        !tolower(end_port_state) == tolower(SERO_HOME_PORT_STATE) ~
          "yes",
        .default = "no"
      ),
    diff_end_port_county =
      dplyr::case_when(
        !tolower(end_port_county) == tolower(SERO_HOME_PORT_COUNTY) ~
          "yes",
        .default = "no"
      ),
    diff_end_port_name_or_city =
      dplyr::case_when(
        !tolower(end_port_name) == tolower(SERO_HOME_PORT_CITY) ~
          "yes",
        .default = "no"
      )
  ) |>
  mutate(
    diff_end_port_state =
      dplyr::case_when(
        !tolower(end_port_state) == tolower(SERO_HOME_PORT_STATE) ~
          "yes",
        .default = "no"
      ),
    diff_end_port_county =
      dplyr::case_when(
        !tolower(end_port_county) == tolower(SERO_HOME_PORT_COUNTY) ~
          "yes",
        .default = "no"
      ),
    diff_end_port_name_or_city =
      dplyr::case_when(
        !tolower(end_port_name) == tolower(SERO_HOME_PORT_CITY) ~
          "yes",
        .default = "no"
      )
  ) |>
  dplyr::ungroup()
toc()
# join_vessel_and_trip_port_diff: 1.74 sec elapsed

#' Explanation:
#'
#' 1. Start measuring the execution time using the `tic()` function.
#'
#' 2. Create a new data frame "join_vessel_and_trip_port_diff" by applying operations to "join_vessel_and_trip".
#'
#' 3. Group the data by "vessel_official_number".
#'
#' 4. Add columns indicating differences between home port and trip start/end ports for both start and end ports.
#'
#' 5. Ungroup the data.
#'
#' 6. Stop measuring the execution time using the `toc()` function and display the elapsed time.
#'

join_vessel_and_trip_end_port_diff |>
  dplyr::select(vessel_official_number,
         dplyr::starts_with("diff")) |>
  dplyr::distinct() |>
  dim()
# [1] 2591    7
# [1] 2145    7
# [1] 1804    4

#### shorten join_vessel_and_trip_port_diff ----
join_vessel_and_trip_port_diff_short <-
  join_vessel_and_trip_end_port_diff |>
  dplyr::select(vessel_official_number,
                permit_region,
                dplyr::starts_with("diff")) |>
  dplyr::distinct()

glimpse(join_vessel_and_trip_port_diff_short)

## count vessel number with different start_port_state and home_port_state ----
### check for one column ----
join_vessel_and_trip_port_diff_short |>
  dplyr::count(diff_start_port_state)
# 1 no                     2501
# 2 yes                      90

# 1 no                     2075
# 2 yes                      70

### make combinations of column names ----
my_col_names <- names(join_vessel_and_trip_port_diff_short)

my_col_names

combs1 <-
  utils::combn(my_col_names, 2) |>
  as.data.frame()

str(combs1)
# 'data.frame':	2 obs. of  28 variables:
#  $ V1 : chr  "vessel_official_number" "permit_region"
#  $ V2 : chr  "vessel_official_number" "diff_start_port_state"
#  $ V3 : chr  "vessel_official_number" "diff_start_port_county"

### keep only combinations of vessel_official_number with another column ----
combs1_short <-
  combs1[1:ncol(join_vessel_and_trip_port_diff_short) - 1]

combs1_short

### count vessels with different trip and home port info, using each pair of names ----
combs1_short_cnts <-
  combs1_short |>
  purrr::map(\(curr_col_names) {
    # browser()
    join_vessel_and_trip_port_diff_short |>
      dplyr::select(paste(curr_col_names, sep = ",")) |>
      dplyr::count(!!sym(curr_col_names[[2]]))
  })

#' This code uses the "map" function from the purrr package to iterate over each element (curr_col_names) in the combs1_short list. Within the map function:
#'
#' 1. The "join_vessel_and_trip_port_diff_short" data frame is piped into the "select" function, where columns specified by "curr_col_names" are selected using "paste" and separated by commas.
#'
#' 2. The "count" function is then applied to the selected data frame. The column to count is specified using "!!sym(curr_col_names[[2]])", where "sym" is used to convert the column name to a symbol for evaluation.
#'
#' The result is a list of data frames, where each data frame contains the counts of occurrences of vessel ids for the second column in "curr_col_names".
#'

#### Show vessel num with different trip and home port info, using each pair of names ----
# |> results="hide"

# use pander for .qmd
pander(combs1_short_cnts)

## Repeat the same with permit region ----
join_vessel_and_trip_port_diff_short_perm <-
  join_vessel_and_trip_port_diff |>
  dplyr::select(vessel_official_number,
         permit_region,
         dplyr::starts_with("diff")) |>
  dplyr::distinct()

join_vessel_and_trip_port_diff_short_perm |>
  dplyr::count(diff_start_port_state)
# 1 no                     2501
# 2 yes                      90

# 1 no                     2075
# 2 yes                      70

my_col_names <- names(join_vessel_and_trip_port_diff_short_perm)

combs2 <-
  utils::combn(my_col_names, 3) |>
  as.data.frame()

#' How many column names have "diff" in it?
#'

dif_cols_num <-
  grep("diff", my_col_names) |>  length()

combs2_short <-
  combs2[1:dif_cols_num]

# View(combs2_short)

### count vessels with different trip and home port info per permit_region, using each set of names ----
combs2_short_cnts <-
  combs2_short |>

  # Use 'map' to apply a function to each element of 'combs2_short'
  purrr::map(\(curr_col_names) {

    # Use 'join_vessel_and_trip_port_diff_short' as the data source
    join_vessel_and_trip_port_diff_short |>

      # Select columns specified by 'curr_col_names' and separate them with ","
      dplyr::select(paste(curr_col_names, sep = ",")) |>

      # Count occurrences of unique combinations of the second and third columns
      dplyr::count(!!sym(curr_col_names[[2]]),
            !!sym(curr_col_names[[3]]))
  })


#### Show vessel num with different trip and home port info per permit_region using each set of names ----
# use pander for .qmd
pander(combs2_short_cnts)

#### Show Number of vessels with different end trip port and home port per permit region ----

home_port_diff_from_end_port <-
  combs2_short_cnts$V6 |>
  dplyr::rename(is_home_port_diff_from_end_port =
                  "diff_end_port_name_or_city")

pander(home_port_diff_from_end_port)

## Different port states by quarter ----
# Q. But specifically do it by state. E.g. If start port state is different than end port state, and would be good to know what those states are. So perhaps the output is a table looks something like this and then maybe we can plot that in a bar chart, like attached (but as #s, not %s).
# print_df_names(processed_logbooks_short_port_states)
# print_df_names(processed_logbooks_short_dates)
processed_logbooks_short_dates_quarters <-
  processed_logbooks_short_dates |>
  select(vessel_official_number,
         permit_region,
         contains("quarter"),
         contains("state")) |>
  distinct()

pander(head(processed_logbooks_short_dates_quarters))
# [1] 4772    7

processed_logbooks_short_dates_quarters__p_l <-
  processed_logbooks_short_dates_quarters |>
  split(as.factor(processed_logbooks_short_dates_quarters$permit_region))

### check if permit_region don't intersect ----
intersect(processed_logbooks_short_dates_quarters__p_l$GOM$vessel_official_number,
          processed_logbooks_short_dates_quarters__p_l$SA$vessel_official_number)
# 0 correct

### start quarter is diff from the end quarter ----
processed_logbooks_short_dates_quarters |>
  filter(!trip_start_quarter_num == trip_end_quarter_num) |>
  nrow()
# 7

### count vessels by end quarter and ports ----
processed_logbooks_short_dates_quarters__p_l__st_cnt <-
  processed_logbooks_short_dates_quarters__p_l |>
  map(\(permit_df) {
    permit_df |>
      group_by(trip_end_quarter_num) |>
      rowwise() |>
      count(start_port_state, end_port_state,
            name = "number_of_vessels") |>
      ungroup()
  })

View(processed_logbooks_short_dates_quarters__p_l__st_cnt__q$GOM[[1]])
### plot start/end ports ----
# print_df_names(processed_logbooks_short_dates_quarters__p_l__st_cnt$GOM)

axis_text_size <- 12

plot_start_end_ports_matrix <-
  function(my_df, quarter_name, permit_region_name) {
    ggplot(my_df,
           aes(x = end_port_state,
               y = start_port_state)) +
      # Rectangles
      geom_raster(aes(fill = number_of_vessels)) +
      scale_fill_viridis(name = "Log of Number of Vessels",
                         labels = scales::comma,
                         trans = "log") +
      labs(x = "End Port State",
           y = "Start Port State",
           title =
             str_glue("Number of Vessels with 2022 permit region in {permit_region_name} by Start / End Ports for Quarter {quarter_name}")) +
      theme_bw() +
      theme(
        axis.text.x = element_text(
          size = axis_text_size
          # ,
          # angle = 0,
          # vjust = 0.3
        ),
        axis.text.y = element_text(size = axis_text_size),
        plot.title = element_text(size = axis_text_size)
      )
  }

# pander(processed_logbooks_short_dates_quarters__p_l__st_cnt__q)

processed_logbooks_short_dates_quarters__p_l__st_cnt__q <-
  processed_logbooks_short_dates_quarters__p_l__st_cnt |>
  map(\(permit_region_df) {
    permit_region_df |>
      split(as.factor(permit_region_df$trip_end_quarter_num))
  })

permit_regions <-
  names(processed_logbooks_short_dates_quarters__p_l__st_cnt__q)

tic("save plots")
processed_logbooks_short_dates_quarters__p_l__st_cnt__q_plots <-
  permit_regions |>
  map(\(curr_permit_region) {
    processed_logbooks_short_dates_quarters__p_l__st_cnt__q[[curr_permit_region]] |>
      map(\(curr_quarter) {
        # browser()
        curr_end_quarter <-
          unique(curr_quarter$trip_end_quarter_num)

        one_plot <-
        plot_start_end_ports_matrix(curr_quarter,
                                    curr_end_quarter,
                                    curr_permit_region)

        plot_file_name <-
          str_glue("vessel_num_{curr_permit_region}_{curr_end_quarter}.png") |>
          tolower()

        plot_file_path <-
         file.path(my_paths$outputs,
                   current_project_basename,
                   plot_file_name)

        save_plot_to_file(plot_file_path,
                          one_plot)
      })
  })
toc()

#'
#' %%%%% Results
#'
#' Analyzing movement patterns of vessels questions:
#'
#' 1. How many SEFHIER vessels land at a different location than they home ports?
#'
#' 2. How many vessels have variable landing locations?
#'
#' (i.e., in the winter they are in one state while in the summer they fish in another)
#'
#' 3. Quantify the # of vessels who fish in both the gulf and S Atl.
#'
#' 4. How many SEFHIER vessels start at a different location than they end, count by quarter
#'

#' ## How many SEFHIER vessels land at a different location than they home ports
#'
#+ Show Number of vessels with different end trip port and home port per permit region

#'
#' ## How many vessels have variable landing locations
#'  (i.e., in the winter they are in one state while in the summer they fish in another)
#'

#' ### How many vessels have multiple end ports
#'
#+ How many vessels have multiple end ports

#'
#' ### How many vessels have multiple end port states
#'
#+ How many vessels have multiple end port states
#'

#' ### How many vessels have same (or diff) trip start or end in all quarters of 2022
#+ count same or diff trip start or end
#'

#' ### How many vessels have same (or diff) trip start or end in all quarters of 2022 by permit region
#+ count same or diff by permit_region and trip start or end
#'

#' ## Quantify the number of vessels who fish in both the Gulf and S Atl
#' ### By counting start ports.
#'
#+ How many vessels have start port in one or in both regions

#' ### By counting end ports.
#'
#+ How many vessels have end port in one or in both regions


#' ### The same (start or end in both Gulf and S Atl) by a vessel permit region
#' #### By counting start ports.
#'
#+ Count multiple start regions by permit_region

#' #### By counting end ports.
#'
#+ Count multiple end regions by permit_region

#' ## How many SEFHIER vessels start at a different location than they end, count by quarter
#'
#+ by start quarter

#' ### Total number of vessels by 2022 quarters
#'
#+ see total vessels by 2022 start trip quarters

