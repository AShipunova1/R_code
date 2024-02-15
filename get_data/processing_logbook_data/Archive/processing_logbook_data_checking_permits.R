# check missing/added boats ----
not_all_na <- function(x) any(!is.na(x))

my_check <- function(my_df, my_vsl) {
  my_df |>
    filter(VESSEL_OFFICIAL_NUMBER == my_vsl) |>
    select(VESSEL_OFFICIAL_NUMBER,
           contains("perm"),
           contains("effect"),
           contains("exp")) |>
    distinct() |>
    select(where(not_all_na))
}

vsls_to_check <-
  list(
    "FL3610NF",
    "FL4980LH",
    "TX8671AM",
    "1262695",
    "FL0404LF",
    "934563",
    "1075595",
    "1168377",
    "676198",
    "900347",
    "612531",
    "1097071",
    "981337",
    "1056956",
    "FL5345ML",
    "572241",
    "521095",
    "TX6348EK",
    "680069",
    "1325496",
    "621251",
    "FL4004NG",
    "FL0526TJ",
    "946290",
    "1045140",
    "FL8519NA",
    "979631",
    "968261",
    "FL0223KJ",
    "FL6731ST",
    "FL9124ST",
    "1276869",
    "1275799",
    "GA0824VK",
    "FL6313GU",
    "1213107",
    "1292808",
    "FL6016NY",
    "FL7085PL",
    "FL9370JC",
    "1192239",
    "1233666",
    "996948",
    "1078789",
    "944179",
    "FL5386PZ",
    "FL6786PB",
    "FL4862MW",
    "AL0220VC",
    "1227341",
    "FL1815PL",
    "1061390",
    "FL3510LY",
    "FL1202MW",
    "1300336"
  )

dfs_to_check <-
  list(SEFHIER_logbooks_usable,
  SEFHIER_metrics_tracking)

dfs_to_check_names <-
  c("SEFHIER_logbooks_usable",
  "SEFHIER_metrics_tracking")

# View(SEFHIER_logbooks_usable)
# grep("perm|end|exp", names(SEFHIER_logbooks_usable), value = T, ignore.case = T)

test_res <-
  map(vsls_to_check,
      \(vessel_official_num) {
        map(dfs_to_check,
            \(my_df) {
              my_check(my_df,
                       vessel_official_num)
            })
      })
names(test_res) <- vsls_to_check

test_res <-
  map(test_res, {
    \(curr_vessel_official_num_l) {
      names(curr_vessel_official_num_l) <-
        dfs_to_check_names
      return(curr_vessel_official_num_l)
    }
  })

# View(test_res1)

current_vessel <- ""
# unlink(temp_res_file)
temp_res_file <- tempfile()

has_empty_list <- function(x) {

  if (is.list(x)) {
    current_vessel <- x$VESSEL_OFFICIAL_NUMBER
    if (length(current_vessel) > 0) {
      cat(current_vessel,
          file = temp_res_file,
          append = T,
          sep = "\n")
    }

    if (length(x) == 0) {
      cat("Empty Metrics",
          file = temp_res_file,
          append = T,
          "\n")
      return(TRUE)
    } else {
      return(any(vapply(x, has_empty_list, logical(1))))
    }
  } else {
    return(FALSE)
  }
}

my_res_empt <- has_empty_list(test_res)

read_back <- readLines(temp_res_file)

View(as.data.frame(read_back))

glimpse(test_res[['TX8671AM']])
