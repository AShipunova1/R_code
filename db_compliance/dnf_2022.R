# t neg (DNF) and vessels for 2022
# use data from db_compliance.R

# View(trip_neg_2022_w_y)
# View(vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22)
View(vessels_permits_2022_r)
v_p_tne <-
  vessels_permits_2022_r |>
  filter(!permit_sa_gom == "sa_only") |>
  # dim()
  # 9776
  inner_join(trip_neg_2022_w_y,
             by = join_by(VESSEL_VESSEL_ID == VESSEL_ID),
             relationship = "many-to-many")

# trip_neg_2022_w_y_dates_ids$VESSEL_ID,
#   vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22__list_dates__sa_w_p22_ids$VESSEL_VESSEL_ID
# ) |>

View(v_p_tne)
# [1] 58349    51
# [1] 210168     66 (w all permits)

vessels__trip_neg_22 <-
  vessels_permits_2022_r %>%
  inner_join(
    trip_neg_2022_w_y,
    join_by(VESSEL_VESSEL_ID == VESSEL_ID),
    relationship = "many-to-many",
    suffix = c(".v", ".tneg")
  )

View(vessels__trip_neg_22)

vessels__trip_neg_22 |>
  filter(PERMIT_GROUP == 7) |>
  select(PERMIT_VESSEL_ID, VESSEL_VESSEL_ID) |>
  distinct() |>
  dim()
# [1] 1975    2 vessels

vessels__trip_neg_22 |>
  filter(PERMIT_GROUP == 7) |>
  select(TRIP_ID) |>
  distinct() |>
  dim()
# 410054      

vessels__trip_neg_22 |>
  filter(PERMIT_GROUP == 7) |>
  select(TRIP_ID, permit_sa_gom) |>
  distinct() |>
  count(permit_sa_gom)
# 1      gom_only  34321
# 2       sa_only 392199

# v_p and dual + neg ----
v_p_tne__dual <-
  vessels_permits_2022_r_end_date_l_overlap_join_w_dual |>
  inner_join(
    trip_neg_2022_w_y,
    join_by(VESSEL_VESSEL_ID == VESSEL_ID),
    relationship = "many-to-many",
    suffix = c(".vp", ".tne")
  )

print_df_names(v_p_tne__dual)

v_p_tne__dual |>
  # filter(PERMIT_GROUP == 7) |>
  select(TRIP_ID) |>
  distinct() |>
  dim()
# [1] 410943      1

v_p_tne__dual |>
  # filter(PERMIT_GROUP == 7) |>
  select(PERMIT_VESSEL_ID, VESSEL_VESSEL_ID) |>
  distinct() |>
  dim()
# [1] 1979    2


v_p_tne__dual |>
  select(PERMIT_VESSEL_ID, VESSEL_VESSEL_ID) |>
  distinct() |>
  count

v_p_tne__dual |>
  select(TRIP_ID, permit_sa_gom) |>
  distinct() |>
  count(permit_sa_gom)
#   permit_sa_gom      n
#   <chr>          <int>
# 1 dual           15747
# 2 gom_only       19013
# 3 sa_only       380495

# 15747+19013 = 34760
# Jenny's ----
# "C:\Users\anna.shipunova\Documents\R_files_local\jennys_code\output\GOMvesselsDNFreports.xlsx"
# DNFsSubmittedButNotRequired
# VA2668BK

v_p_tne__dual |> 
  filter(PERMIT_VESSEL_ID == "VA2668BK") |> 
  View()

v_p_tne |> 
  filter(PERMIT_VESSEL_ID == "VA2668BK") |> 
  View()

check_j_ids_f_name <- r"(~\R_files_local\jennys_code\output\GOMvesselsDNFreports.xlsx)"

file.exists(check_j_ids_f_name)
# T
check_j_ids <-
  read_xlsx(check_j_ids_f_name)

names(check_j_ids) <- "VESSEL_OFFICIAL_NUMBER_GOMDNF"
# [1] "unique(GOMDNFreports$VESSEL_OFFICIAL_NUMBER)"

glimpse(vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22__list_dates__sa_w_p22)
check_j_ids

glimpse(vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22_uid_short__list)
vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22_uid_short__list$gom_only |> 
  head() |> 
  tidyr::unnest_wider(unique_all_vessel_ids, names_sep = "_") |> 
  glimpse()
# ...
# $ unique_all_vessel_ids_1 <chr> "FL4459MW", "FL4459MW", "FL445…
# $ unique_all_vessel_ids_2 <chr> "391019", "391019", "390425", …
# $ permit_sa_gom           <chr> "gom_only", "gom_only", "gom_o…
# x <-
#   vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22_uid_short__list$gom_only |>
#   head()
# y <- list(check_j_ids$VESSEL_OFFICIAL_NUMBER_GOMDNF)
# 
# str(y)
# 
# my_f <- function(x, y) {
#     browser()
#     # stringi::stri_detect_fixed()
#     head(x, n = 1) |> 
#       glimpse()
#     # x %>%
#   # filter(unique_all_vessel_ids %>%
#   #          map(str_detect, y) %>%
#   #          map_lgl(any))
#   #   ! `pattern` must be a string, not a list.
  # }

# purrr::pmap(x, y, my_f(x, y)) |> str()

# map(y, function() {unique_all_vessel_ids })
vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22_uid_short__list$gom_only |> 
  head() |> 
  filter(unique_all_vessel_ids %>%
           purrr::pmap(check_j_ids)
           # purrr::pmap(str_detect, "Lannister|Stark") 
         # %>%
           # map_lgl(any)
         ) %>%
  str()

# ====
x <-
  vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22_uid_short__list$gom_only |>
  head(5)
y <- list(check_j_ids$VESSEL_OFFICIAL_NUMBER_GOMDNF)

my_f <- function(x, y) {
  
  browser()
  # stringi::stri_detect_fixed()
  head(x, n = 1) |>
    glimpse()
  
  res <-
    map(x$unique_all_vessel_ids,
        dplyr::intersect,
        y)
  
  # x %>%
  # filter(unique_all_vessel_ids %>%
  #          map(str_detect, y) %>%
  #          map_lgl(any))
  #   ! `pattern` must be a string, not a list.
  
  return(res)
}

rr <- my_f(x, y)
# length(rr[[1]])
# 0
