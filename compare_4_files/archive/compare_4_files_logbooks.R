### db_logbooks: unify vessel ids ----
# grep("vessel", names(all_4_dfs3$db_logbooks), value = T)
# [1] "vessel_id"           "vessel_official_nbr" "vessel_name"
# [4] "sero_vessel_permit"  "garfo_vessel_permit"

all_4_dfs3$db_logbooks <-
  all_4_dfs3$db_logbooks |>
  rename("vessel_official_number" = "vessel_official_nbr",
         "db_logbooks_vessel_id" = "vessel_id") # needed to see if NA in the full join

