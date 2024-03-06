# temp ----
# add new columns to confirmed file ----
# compare results with the new code ----
## download from Google drive ----
downloaded_file_path <- 
  file.path(my_paths$inputs,
            current_project_basename,
            "egregious violators for investigation_2023-08-15_to_2024-02-13_OLE - egregious_violators_to_investigate_2024-02-21.csv")

downloaded_result <- 
  read_csv(downloaded_file_path)

downloaded_result

same_name <-
  intersect(
    names(
      compl_corr_to_investigation1_short_dup_marked__permit_region__fhier_names__fhier_addr__mv_cols
    ),
    names(downloaded_result)
  )

same_name |>
  cat(sep = ', ')

suffixes = c("__new", "__old")

old_n_new <-
  full_join(
    compl_corr_to_investigation1_short_dup_marked__permit_region__fhier_names__fhier_addr__mv_cols,
    downloaded_result,
    join_by(vessel_official_number),
    suffix = suffixes
  )

dim(compl_corr_to_investigation1_short_dup_marked__permit_region__fhier_names__fhier_addr__mv_cols)
# [1] 217  20
dim(downloaded_result)
# [1] 262  18
dim(old_n_new)
# [1] 263  37

# View(old_n_new)
exclude <- c("Notes", "vessel_official_number")

same_name_to_look_up <- 
  same_name |> 
  as.data.frame() |>
  filter(!same_name %in% exclude)

diff_vals <-
  same_name_to_look_up$same_name |>
  map(\(curr_name) {
    # browser()
    curr_fields <- lapply(suffixes,
                          function(x) {
                            paste0(curr_name, x) |> sym()
                          })
    old_n_new |>
      group_by(vessel_official_number) |>
      filter(!(!!curr_fields[[1]] == !!curr_fields[[2]])) |>
      select(vessel_official_number, curr_fields[[1]], curr_fields[[2]]) |>
      distinct() %>%
      return()
    
  })

# View(diff_vals)
is_diff <-
  diff_vals[map(diff_vals, ~ nrow(.)) > 0]

# View(is_diff[[1]])
# View(is_diff[[2]])

compl_corr_to_investigation1_short_dup_marked__permit_region__fhier_names__fhier_addr__mv_cols |> 
    filter(vessel_official_number %in% is_diff[[2]]$vessel_official_number) |> 
    arrange(vessel_official_number) |> 
    select(vessel_official_number, hailing_port_city, hailing_port_state) |> 
    distinct()
# 8 FL5193RE               MADEIRA BEACH     FL                
# ok

downloaded_result |> 
    filter(vessel_official_number %in% is_diff[[2]]$vessel_official_number) |> 
    arrange(vessel_official_number) |> 
    select(vessel_official_number, contains("port")) |> 
    distinct()
# ok

## drop first two columns (empty in new) and rename long colnames ----
old_n_new_ren <-
  # old_n_new |>
  old_n_new[3:ncol(old_n_new)] |> 
  rename_with(
    stringr::str_replace,
    pattern = "^Confirmed Egregious.+$",
    replacement = "new_conf_mark__old"
    # ,
    # matches("Length")
  )

names(old_n_new_ren)

## check marked with "no ----
marked_no <-
  old_n_new_ren |>
  filter(tolower(new_conf_mark__old) == "no") |> 
  filter(if_any(ends_with("__new"), ~ is.na(.)))

nrow(marked_no)
# 83
# 76 not na

# View(marked_no)

marked_no |> 
    select(Notes__old) |> 
    distinct() |> 
    print(n = 50)

marked_no__old <-
  marked_no |>
  select(vessel_official_number, ends_with("__old")) |>
  mutate(
    resons = case_when(
      grepl("submitted.+24", 
            Notes__old, 
            ignore.case = TRUE) ~
        "report_submitted_in_feb",
      grepl("compliance not applicable back", 
            Notes__old, 
            ignore.case = TRUE) ~
        "not_all_time_permit",
      grepl("expire.+24", 
            Notes__old, 
            ignore.case = TRUE) ~
        "permit_expired",
      .default = "other"
    )
  )

marked_no__old |> 
  select(Notes__old, resons) |> 
  distinct() |> 
  glimpse()
  # View()

marked_no__old |>
  select(Notes__old, resons) |>
  filter(resons == "other") |>
  distinct() |>
  # View()
  glimpse()

marked_no__old |>
  select(vessel_official_number, Notes__old, resons) |>
  filter(resons == "other") |>
  distinct() |>
  View()

# combine confirmed with hailing port info ----
confirmed_w_hailing_port <- 
  old_n_new |> 
  select(vessel_official_number,
         `Confirmed Egregious? (permits must still be active as of 2/13/24, missing past 6 months, and (1) they called/emailed us (incoming), or (2) at least 2 contacts (outgoing) with at least 1 call (voicemail counts) and at least 1 email)`,
         ends_with("__old"), starts_with("hailing")) |> 
  rename_with(
    stringr::str_replace,
    pattern = "__old$",
    replacement = ""
  )

# names(old_n_new)[[21]] == names(old_n_new)[[1]]
# # F
# 
# names(old_n_new)[[21]]

new_result_path <-
  file.path(
    my_paths$outputs,
    current_project_basename,
    str_glue(
      "egregious_violators_to_investigate_confirmed_{today()}.csv"
    )
  )

write_csv(confirmed_w_hailing_port,
          new_result_path)
