# all_dfs_list_no_srhs |> print_df_names()
# [1] "compliance_from_fhier, permits_from_pims, metrics_report, permit_info_from_db, transfer_applications_from_pims"

# file_name_combinations

grep("in_comp.+not_", ls(), value = T) |>
  cat(sep = '\n')

# vessel_in_compl_not_in_metrics
# vessel_in_compl_not_in_permit_info_from_db
# vessel_in_compl_not_in_pims_perm1

# in compl 1
length(vessel_in_compl_not_in_metrics)
# 240
in_compl__not_in_metr__not_db_perm <-
  intersect(vessel_in_compl_not_in_metrics,
          vessel_in_compl_not_in_permit_info_from_db)
length(in_compl__not_in_metr__not_db_perm)
# 8

# in compl 2
in_compl__not_in_metr__not_pims_perm <-
  intersect(vessel_in_compl_not_in_metrics,
          vessel_in_compl_not_in_pims_perm$vessel_official_number)
length(in_compl__not_in_metr__not_pims_perm)
# 188

# in compl 2a
in_compl__not_in_metr__not_pims_perm1 <-
  intersect(vessel_in_compl_not_in_metrics,
          vessel_in_compl_not_in_pims_perm1)
length(in_compl__not_in_metr__not_pims_perm1)
# 188

# in compl 3
length(vessel_in_compl_not_in_permit_info_from_db)
# 20
length(vessel_in_compl_not_in_pims_perm1)
# 1061

in_compl__not_in_db_perm__not_pims_perm1__inters <-
  intersect(vessel_in_compl_not_in_permit_info_from_db,
          vessel_in_compl_not_in_pims_perm1)

length(in_compl__not_in_db_perm__not_pims_perm1__inters)
# 13

# get all ----

all_vessel_in_names_0 <-
  ls(pattern = "^vessel_in_.+not_")

all_vessel_in_names <-
  grep("compl_info",
       all_vessel_in_names_0,
       invert = TRUE,
       value = TRUE)

all_vessel_in_names_list__combinations <-
  combn(all_vessel_in_names, 2) |>
  # t() |>
  as.data.frame()

# str(all_vessel_in_names_list__combinations)

# View(all_vessel_in_names_list__combinations)

# dfs_test <- mget(ls(pattern = '^ca_pop_\\d{4}$'))

all_vessel_in_names_list__combinations__res <- list()
  # replicate(ncol(all_vessel_in_names_list__combinations),
            # list())

# map(all_vessel_in_names_list__combinations[, 61],
#     # V61
#     ~ get(.x) |> class()
#     )

all_vessel_in_names_list__combinations__comb_names <-
  all_vessel_in_names_list__combinations |>
  map(~ str_glue("{.x[[1]]}__{.x[[2]]}"))

all_vessel_in_names_list__combinations__in_one_list <-
  map2(
    all_vessel_in_names_list__combinations,
    all_vessel_in_names_list__combinations__comb_names,
    \(one_comb, curr_name) {
    # browser()

    curr_classes <-
      map(one_comb,
          \(curr_combination){
            temp_res <- get(curr_combination)
            all(class(temp_res) == "character")
          })

    if (!FALSE %in% curr_classes) {
      res_inters <-
        intersect(get(one_comb[[1]]),
                  get(one_comb[[2]]))

      all_vessel_in_names_list__combinations__res[[curr_name]] <-
        res_inters

    }

  })

names(all_vessel_in_names_list__combinations__in_one_list) <-
  all_vessel_in_names_list__combinations__comb_names

# View(all_vessel_in_names_list__combinations__in_one_list)

map(all_vessel_in_names_list__combinations__in_one_list,
    length)

