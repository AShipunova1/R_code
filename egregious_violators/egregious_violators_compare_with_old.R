# Check the difference between old and new results ----

## Find changed info ----

### rename long col names ----

old_n_new_result_list <-
  list("old_res" = prev_result, 
       "new_res" = compl_corr_to_investigation_short_dup_marked__permit_region__add_columns)

# View(cc)

# unify col names
old_n_new_result_list_short_col_name <-
  old_n_new_result_list |>
  purrr::map(~ dplyr::rename(.x, confirmed_egr = !!2)) |> 
  purrr::map(~ dplyr::rename(.x, notes = !!3))

# check
# old_n_new_result_list_short_col_name |> 
# purrr::map(print_df_names)

old_n_new_results <-
  dplyr::left_join(
    old_n_new_result_list_short_col_name[[1]],
    old_n_new_result_list_short_col_name[[2]],
    by = dplyr::join_by(
      vessel_official_number
      # ,
      # permitgroup,
      # permit_groupexpiration,
      # contactrecipientname,
      # permit_holder_names,
      # contactphone_number,
      # contactemailaddress,
      # date__contacttypes,
      # duplicate_w_last_time,
      # hailing_port_city,
      # hailing_port_state
    ),
      suffix = c("_old", "_new")
  )

# View(old_n_new_results)

# auxfunctions::print_df_names(old_n_new_results)

old_n_new_results$confirmed_egr_old |> unique()
# [1] "Yes" "No" 

old_n_new_cols_0 <-
  names(old_n_new_results) |>
  stringr::str_extract(".+_old$") |>
  stringr::str_remove(".*week.*") |>
  stringr::str_remove(".*year.*") |>
  stringr::str_remove("duplicate_w_last_time_.+") |>
  unique() |>
  na.omit()

old_n_new_cols <- 
  old_n_new_cols_0[old_n_new_cols_0 != ""]

# grep("^$", old_n_new_cols, value = T)
# 0 ok

old_n_new_cols

  # c(
  #   "confirmed_egr_old",
  #   "permitgroup_old",
  #   "permit_groupexpiration_old",
  #   "contactrecipientname_old",
  #   "permit_holder_names_old",
  #   "contactphone_number_old",
  #   "contactemailaddress_old",
  #   "date__contacttypes_old",
  #   "duplicate_w_last_time_old",
  #   "hailing_port_city_old",
  #   "hailing_port_state_old"
  # )

old_new_func <- function(old_col_name) {
  # browser()
  
  new_col_name <-
    stringr::str_replace(old_col_name, "_old$", "_new")
  
  not_the_same <-
    old_n_new_results |>
    filter(!(!!sym(old_col_name)) == !!sym(new_col_name))
  
  diff_result <-
    not_the_same |>
    dplyr::select(vessel_official_number, 
                  !!old_col_name, 
                  !!new_col_name)
  
  return(diff_result)
}

### get differencies ----
res_diff_old_n_new <-
  purrr::map(old_n_new_cols, old_new_func) |>
  purrr::reduce(full_join, by = "vessel_official_number") |> 
  auxfunctions::remove_empty_cols()

res_diff_old_n_new |> View()
  
## check marked as not egregious previously ----
old_n_new_results |>
  dplyr::select(confirmed_egr_old) |>
  dplyr::distinct()
# 1               Yes
# 2                No

# print_df_names(old_n_new_results)
# 
old_n_new_results_not_egr <-
  old_n_new_results |>
  dplyr::filter(tolower(confirmed_egr_old) == "no" &
                  !is.na(contact_date_dttm_new))

dim(old_n_new_results_not_egr)
# [1] 12 82

# View(old_n_new_results_not_egr)
