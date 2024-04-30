# vessel_in_metrics_not_in_compl ----
# <-
#   setdiff(
#     all_dfs_list_no_srhs$metrics_report$vessel_official_number,
#     all_dfs_list_no_srhs$compliance_from_fhier$vessel_official_number
#   )

# all_dfs_list_no_srhs$metrics_report |>
#   filter(vessel_official_number %in% vessel_in_metrics_not_in_compl) |>
#   View()

compliance_from_fhier |>
  filter(`Vessel Official Number` == "TX4661EK") |>
  glimpse()

# vessel_in_compl_not_in_metrics ----

vessel_in_compl_not_in_metrics__compl_info <-
  all_dfs_list1$compliance_from_fhier |>
  filter(vessel_official_number %in% vessel_in_compl_not_in_metrics)

vessel_in_compl_not_in_metrics__compl_info_short <-
  vessel_in_compl_not_in_metrics__compl_info |>
  select(vessel_official_number, permit_groupexpiration) |>
  distinct()

vessel_in_compl_not_in_metrics__compl_info_short |> count(year)
# 1  2023   240

vessel_in_compl_not_in_metrics__compl_info_short__not_2023_exp <-
  vessel_in_compl_not_in_metrics__compl_info_short |>
  mutate(permit_groupexpiration = mdy(permit_groupexpiration)) |>
  filter(!year(permit_groupexpiration) == "2023")

# check srhs
srhs_vessels__renamed |>
  filter(vessel_official_number %in% vessel_in_compl_not_in_metrics) |>
  nrow()
# 0
#

vessel_in_compl_not_in_metrics__compl_info_short__not_2023_exp |>
  arrange(vessel_official_number, permit_groupexpiration) |>
  write.csv(
    file.path(
      curr_proj_output_path,
      "vessel_in_compl_not_in_metrics__compl_info_short__not_2023_exp.csv"
    )
  )
