# t neg (DNF) and vessels for 2022
# use data from db_compliance.R

# View(trip_neg_2022_w_y)
# View(vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22)
v_p_tne <-
  vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22 |>
  filter(!permit_sa_gom == "sa_only") |>
  # dim()
  # 2593
  inner_join(trip_neg_2022_w_y,
             by = join_by(VESSEL_VESSEL_ID == VESSEL_ID),
             relationship = "many-to-many")

# trip_neg_2022_w_y_dates_ids$VESSEL_ID,
#   vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22__list_dates__sa_w_p22_ids$VESSEL_VESSEL_ID
# ) |>

View(v_p_tne)
# [1] 58349    51
