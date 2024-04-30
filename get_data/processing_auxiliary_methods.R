# Auxiliary methods ----

# Define a function named 'connect_to_secpr'.
# It returns the established database connection (con), which can be used to interact with the "SECPR" database in R.
# Usage:
# con <- connect_to_secpr()
# or
# try(con <- connect_to_secpr())

connect_to_secpr <- function() {
  # Retrieve the username associated with the "SECPR" database from the keyring.
  my_username <- keyring::key_list("SECPR")[1, 2]

  # Use 'dbConnect' to establish a database connection with the specified credentials.
  con <- dbConnect(
    dbDriver("Oracle"),
    # Use the Oracle database driver.
    username = my_username,
    # Use the retrieved username.
    password = keyring::key_get("SECPR", my_username),
    # Retrieve the password from the keyring.
    dbname = "SECPR"  # Specify the name of the database as "SECPR."
  )

  # Return the established database connection.
  return(con)
}

# ---
# Pretty message print
function_message_print <- function(text_msg) {
  cat(crayon::bgCyan$bold(text_msg),
      sep = "\n")
}

# Define a helper function 'title_message_print' to print the title message in blue.
title_message_print <- function(title_msg) {
  cat(crayon::blue(title_msg), sep = "\n")
}

# Define a helper function 'my_tee' to print the message to the console and a file.
my_tee <- function(my_text,
                   my_title = NA,
                   stat_log_file_path = NA,
                   date_range = my_year) {

  the_end = "---"

  # Print out to console
  title_message_print(my_title)
  cat(c(my_text, the_end),
      sep = "\n")

  # Create a new file every day
  if (is.na(stat_log_file_path)) {
    stat_log_file_path <-
      file.path(Path,
                Outputs,
                str_glue("processing_stats_{date_range}_run_{today()}.log"))
  }

  # Write to a log file
  cat(c(my_title, my_text, the_end),
      file = stat_log_file_path,
      sep = "\n",
      append = TRUE)
}

# ---
# A function to print out stats.
# Usage: my_stats(Logbooks)

# Explanation:
#
# 1. **Define Function with Optional Parameter:**
#    - `my_stats <- function(my_df, title_msg = NA) { ... }`: Define a function named 'my_stats' that takes a dataframe 'my_df' as input and an optional 'title_msg' parameter with a default value of NA.
#
# 2. **Check and Assign Default Title Message:**
#    - `if (is.na(title_msg))  { ... }`: Check if 'title_msg' is NA, and if so, assign the dataframe name as the default title message using 'deparse(substitute(my_df))'.
#
# 3. **Extract Statistics:**
#    - `rows_n_columns <- dim(my_df)`: Extract the number of rows and columns in the dataframe.
#    - `uniq_vessels_num <- n_distinct(my_df[["VESSEL_OFFICIAL_NUMBER"]])`: Count the number of distinct vessel numbers.
#    - `uniq_trips_num <- n_distinct(my_df[["TRIP_ID"]])`: Count the number of distinct trip IDs.
#
# 4. **Create Formatted Text with Statistics:**
#    - `stat_text <- str_glue("rows: {rows_n_columns[[1]]} ... Unique trips (logbooks): {uniq_trips_num}")`: Use 'str_glue' to format the statistics into a text string.
#
# 5. **Print Title Message and Statistics to Console:**
#    - `title_message_print(title_msg)`: Use the helper function 'title_message_print' to print the title message in blue.
#    - `print(stat_text)`: Print the formatted statistics to the console.
#
# 6. **Write Statistics to Log File:**
#    - `stat_log_file_path <- file.path(Path, Outputs, str_glue("stat_info_{today()}.log"))`: Define the file path for the log file, including the date.
#    - `cat(c(title_msg, stat_text), file = stat_log_file_path, sep = "\n", append = TRUE)`: Write the title message and formatted statistics to the log file, appending to the file if it already exists.

my_stats <- function(my_df, title_msg = NA) {

  # A title
  if (is.na(title_msg))  {
    df_name = deparse(substitute(my_df))
    title_msg <- df_name
  }

  # Extract statistics
  rows_n_columns <- dim(my_df)
  uniq_vessels_num <- n_distinct(my_df[["VESSEL_OFFICIAL_NUMBER"]])
  uniq_trips_num <- n_distinct(my_df[["TRIP_ID"]])

  # Create a formatted text with statistics
  stat_text <- str_glue(
    "
rows: {rows_n_columns[[1]]}
columns: {rows_n_columns[[2]]}
Unique vessels: {uniq_vessels_num}
Unique trips (logbooks): {uniq_trips_num}
"
  )

  # Print out to console and to the log file
  my_tee(stat_text,
         my_title = title_msg,
         stat_log_file_path = NA)
}

# ---
# to use to drop empty columns, like select(where(not_all_na))
not_all_na <- function(x) any(!is.na(x))

# ---
# A function to use every time we want to read a ready file or query the database if no files exist.

# The read_rds_or_run_query function is designed to read data from an RDS file if it exists or run an SQL query to pull the data from Oracle db if the file doesn't exist.
# See usage below at the `Grab compliance file from Oracle` section
read_rds_or_run_query <- function(my_file_path,
                                  my_query,
                                  force_from_db = NULL) {

  if (file.exists(my_file_path)) {
    modif_time <- file.info(my_file_path)$mtime
  }

    # Check if the file specified by 'my_file_path' exists and 'force_from_db' is not set.
    if (file.exists(my_file_path) &
        is.null(force_from_db)) {
        # If the file exists and 'force_from_db' is not set, read the data from the RDS file.

        function_message_print("File already exists, reading.")

        my_result <- readr::read_rds(my_file_path)

    } else {

      # If the file doesn't exist or 'force_from_db' is set, perform the following steps:

      # 0. Print this message.
      function_message_print(c(
        "File",
        my_file_path,
        "doesn't exists, pulling data from database.",
        "Must be on VPN."
      ))

      # 1. Generate a message indicating the date and the purpose of the run for "tic".
      msg_text <-
        paste(today(), "run for", basename(my_file_path))
      tictoc::tic(msg_text)  # Start timing the operation.

      # 2. Run the specified function 'my_function' on the provided 'my_data' to generate the result. I.e. download data from the Oracle database. Must be on VPN.

      my_result <- dbGetQuery(con, my_query)
      # my_result <- my_function(my_data)

      tictoc::toc()  # Stop timing the operation.

      # 3. Save the result as an RDS binary file to 'my_file_path' for future use.
      # try is a wrapper to run an expression that might fail and allow the user's code to handle error-recovery.

      # 4. Print this message.
      function_message_print(c("Saving new data into a file here: ",
                       my_file_path))

      try(readr::write_rds(my_result,
                           my_file_path))

      modif_time <- date()
    }

  # Print out the formatted string with the file name ('my_file_name') and the modification time ('modif_time') to keep track of when the data were downloaded.
  my_file_name <- basename(my_file_path)
  function_message_print(
    str_glue("File: {my_file_name} modified {modif_time}"))

    # Return the generated or read data.
    return(my_result)
}

#---
