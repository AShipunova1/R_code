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
  vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22_uid_short__list$gom_only
# |>
#   head(5)
y <- check_j_ids$VESSEL_OFFICIAL_NUMBER_GOMDNF

my_f <- function(x, y) {
  
  # browser()
  # stringi::stri_detect_fixed()
  # head(x, n = 1) |>
  #   glimpse()
  
  res <- map(y, function(one_j_id) {
    # browser()
    map(x$unique_all_vessel_ids,
        function(unique_all_vessel_id) {
          # browser()
          stringr::str_detect(unique_all_vessel_id, one_j_id) %>%
            return()
        })
  })
  # map(0:1, function(x)
  #   map_df(0:7,function(y)
  # dplyr::intersect,
  # x$unique_all_vessel_ids)
  
  # x %>%
  # filter(unique_all_vessel_ids %>%
  #          map(str_detect, y) %>%
  #          map_lgl(any))
  #   ! `pattern` must be a string, not a list.
  
  return(res)
}

tic("compare_column_list")
rr <- my_f(x, y)
toc()
length(rr[[1]])
# 0
str(rr)
map(rr, length)

# ====
v_p_gom <-
  vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22_uid_short__list$gom_only |>
  tidyr::unnest_wider(unique_all_vessel_ids, names_sep = "_") 

v_p_dual <-
  vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22_uid_short__list$dual |>
  tidyr::unnest_wider(unique_all_vessel_ids, names_sep = "_") 

names(v_p_gom)
v_p_gom1__j <-
  left_join(
    v_p_gom,
    check_j_ids,
    join_by(unique_all_vessel_ids_1 == VESSEL_OFFICIAL_NUMBER_GOMDNF)
  )

dim(v_p_gom1__j)
# tibble [1,940 × 6] (S3: tbl_df/tbl/data.frame)

vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22_uid_short__list$gom_only$unique_all_vessel_ids[[1]] |> str()
 # chr [1:2] "FL4459MW" "391019"
 # - attr(*, "na.action")= 'omit' int 3

v_p_gom2__j <-
  left_join(
    v_p_gom,
    check_j_ids,
    join_by(unique_all_vessel_ids_2 == VESSEL_OFFICIAL_NUMBER_GOMDNF)
  )
dim(v_p_gom2__j)
# [1] 1940    6

v_p_gom3__j <-
  left_join(
    v_p_gom,
    check_j_ids,
    join_by(unique_all_vessel_ids_3 == VESSEL_OFFICIAL_NUMBER_GOMDNF)
  )
str(v_p_gom3__j)
# [1] 1940    6
str(v_p_gom$unique_all_vessel_ids_1)
all_u_ids <- append(v_p_gom$unique_all_vessel_ids_1,
                   v_p_gom$unique_all_vessel_ids_2) |> 
  append(v_p_gom$unique_all_vessel_ids_3) |> 
  unique()
# str(all_u_ids)
#  chr [1:2578] "FL4459MW" "FL4459PW" "FL4463MX" "FL4482NJ" ...

int_xy_gom <-
intersect(all_u_ids, check_j_ids$VESSEL_OFFICIAL_NUMBER_GOMDNF)
length(int_xy_gom)
# [1] 1009

intersect(v_p_gom$unique_all_vessel_ids_1,
          check_j_ids$VESSEL_OFFICIAL_NUMBER_GOMDNF) |>
  length()
# [1] 1009

intersect(v_p_gom$unique_all_vessel_ids_3,
          check_j_ids$VESSEL_OFFICIAL_NUMBER_GOMDNF) |>
  length()
# 2 & 3: 0

# ====
v_p_gom1__j <-
  inner_join(
    v_p_gom,
    check_j_ids,
    join_by(unique_all_vessel_ids_1 == VESSEL_OFFICIAL_NUMBER_GOMDNF)
  )

View(v_p_gom1__j)
v_p_gom1__j |>
  select(unique_all_vessel_ids_1) |>
  distinct() |>
  dim()
# [1] 1009    1
ok

# ====
v_p__gom_dual__unnest <-
  vessels_permits_2022_r_end_date_l_overlap_join_w_dual_22_uid |>
  dplyr::filter(permit_sa_gom %in% c("dual", "gom_only")) |>
  tidyr::unnest_wider(unique_all_vessel_ids, names_sep = "_")

# str(v_p__gom_dual__unnest)
# tibble [2,593 × 28] (S3: tbl_df/tbl/data.frame)

v_p_gom_dual_1__j <-
  inner_join(
    v_p__gom_dual__unnest,
    check_j_ids,
    join_by(unique_all_vessel_ids_1 == VESSEL_OFFICIAL_NUMBER_GOMDNF)
  )

dim(v_p_gom_dual_1__j)
# [1] 2239   28

dim(check_j_ids)
# [1] 1327    1

v_p_gom_dual_1__j |> 
select(unique_all_vessel_ids_1) |>
  distinct() |>
  dim()
# [1] 1298    1
# 

intersect(v_p__gom_dual__unnest$unique_all_vessel_ids_1,
          check_j_ids$VESSEL_OFFICIAL_NUMBER_GOMDNF) |>
  length()
# [1] 1298


in_j_only <-
  setdiff(
    check_j_ids$VESSEL_OFFICIAL_NUMBER_GOMDNF,
    v_p__gom_dual__unnest$unique_all_vessel_ids_1
  )

glimpse(in_j_only)
# chr [1:29]

from_part <- c(
  "safis.vessels@secapxdv_dblk.sfsc.noaa.gov",
  "safis.trips_neg@secapxdv_dblk.sfsc.noaa.gov"
)

field_names <- tibble(
  c(
    "SERO_OFFICIAL_NUMBER",
    "VESSEL_ID",
    "SUPPLIER_VESSEL_ID",
    "COAST_GUARD_NBR",
    "STATE_REG_NBR"
  )
)

all_29_ids <- paste0(in_j_only, collapse = "', '")

trip_neg_query_templ <- 
  "SELECT *
  FROM
  {from_part}
  WHERE 
  {field_name}
  in ('{all_29_ids}')"

# res_q <-
#   map_chr(field_names[,1], str_interp, string = trip_neg_query_templ)

# stringr::str_glue(
#   "My name is {field_names[1,]}, ",
#   "my age next year is {from_part}, "
# )

stringr::str_glue("SELECT *
  FROM
  {from_part}
  WHERE 
  {field_names[1,]}
  in ('{all_29_ids}')")

# field_names |> 
  # map(~)


in_j_only_t_neg <-
  dbGetQuery(con, trip_neg_query)

# create temp p_v_22 table ----
create_p_v_22_query <-
  "CREATE PRIVATE TEMPORARY TABLE ora$ptt_p_v ON COMMIT PRESERVE DEFINITION
  AS
    SELECT DISTINCT
      p.vessel_id AS p_vessel_id,
      entity_id,
      expiration_date,
      permit_group,
      p.TOP as p_top,
      permit,
      effective_date,
      end_date,
      initial_eff_date,
      grp_eff_date,
      last_expiration_date,
      tm_order,
      tm_top_order,
      prior_owner,
      new_owner,
      grp_prior_owner,
      application_id,
      permit_status,
      vessel_alt_num,
      min_period,
      max_period,
      top_name,
      v.vessel_id AS v_vessel_id,
      county_code,
      state_code,
      entry_date,
      supplier_vessel_id,
      port_code,
      hull_id_nbr,
      coast_guard_nbr,
      state_reg_nbr,
      registering_state,
      vessel_name,
      passenger_capacity,
      vessel_type,
      year_built,
      update_date,
      primary_gear,
      owner_id,
      event_id,
      de,
      ue,
      dc,
      uc,
      STATUS,
      ser_id,
      updated_flag,
      sero_home_port_city,
      sero_home_port_county,
      sero_home_port_state,
      sero_official_number
    FROM
           srh.mv_sero_fh_permits_his@secapxdv_dblk.sfsc.noaa.gov p
      JOIN safis.vessels@secapxdv_dblk.sfsc.noaa.gov v
      ON ( p.vessel_id = sero_official_number )
    WHERE
      ( end_date >= TO_DATE('01-JAN-22', 'dd-mon-yy')
        OR expiration_date >= TO_DATE('01-JAN-22', 'dd-mon-yy') )
      AND effective_date <= TO_DATE('31-DEC-22', 'dd-mon-yy')
"

temp_table_p_v_22 <-
  dbGetQuery(con, create_p_v_22_query)

all_j_names1_500 <-
  paste0(check_j_ids$VESSEL_OFFICIAL_NUMBER_GOMDNF[1:500], collapse = "', '")
# 1327
all_j_names500_ <-
  paste0(check_j_ids$VESSEL_OFFICIAL_NUMBER_GOMDNF[501:length(check_j_ids$VESSEL_OFFICIAL_NUMBER_GOMDNF)], collapse = "', '")

# in ('{all_j_names1_500}')
all_trip_neg_query <-
stringr::str_glue("SELECT 
distinct sero_official_number,
count(trip_id) as total_dnf
FROM
  ora$ptt_p_v p_v
    JOIN safis.trips_neg@secapxdv_dblk.sfsc.noaa.gov tne
  ON ( p_v.v_vessel_id = tne.vessel_id )
WHERE sero_official_number
  in ('{all_j_names1_500}')
AND p_top LIKE '%G%'
AND effective_date <= TO_DATE('31-DEC-22', 'dd-mon-yy')
AND expiration_date > TO_DATE('31-DEC-21', 'dd-mon-yy')
WHERE sero_official_number
  in ('{all_j_names1_500}')

and trip_date <= TO_DATE('31-DEC-22', 'dd-mon-yy')
and trip_date > TO_DATE('31-DEC-21', 'dd-mon-yy')
group by sero_official_number
")

# all_j_names1_500
in_j_only_t_neg_all1 <-
  dbGetQuery(con, all_trip_neg_query)

# all_j_names500_
in_j_only_t_neg_all501_ <-
  dbGetQuery(con, all_trip_neg_query)

str(in_j_only_t_neg_all1)
# 'data.frame':	109 obs. of  2 variables:
str(in_j_only_t_neg_all501_)
# 'data.frame':	177 obs. of  2 variables:

# in_j_only_t_neg_all501_
# in_j_only_t_neg_all <-
#   dbGetQuery(con, "Select * from ora$ptt_p_v")
# dim(in_j_only_t_neg_all)
# [1] 20208    51

# compbine results ----
all_in_j_only_t_neg_all <-
  rbind(in_j_only_t_neg_all1,                                   in_j_only_t_neg_all501_) |> 
  distinct()

dim(all_in_j_only_t_neg_all)
286

sum(all_in_j_only_t_neg_all$TOTAL_DNF)
# [1] 93276

# should be
# Total Did Not Fish Reports
# 28,564	

# check why too much ----
# 1064902
# 400

# SELECT
#   count(distinct trip_id)
# FROM
#        safis.vessels@secapxdv_dblk.sfsc.noaa.gov v
#   JOIN safis.trips_neg@secapxdv_dblk.sfsc.noaa.gov tne
#   USING ( vessel_id )
# WHERE
#   sero_official_number = '1064902';
# --346

# count without permits ----
v__tne_query <-
  stringr::str_glue("SELECT
  distinct sero_official_number,
  count(trip_id) as total_dnf
  FROM
       safis.vessels@secapxdv_dblk.sfsc.noaa.gov v
  JOIN safis.trips_neg@secapxdv_dblk.sfsc.noaa.gov tne
  USING ( vessel_id )
  WHERE 
  sero_official_number in ('{all_j_names1_500}')
  or
  sero_official_number in ('{all_j_names500_}')
  and trip_date > TO_DATE('31-DEC-21', 'dd-mon-yy')
  and trip_date <= TO_DATE('31-DEC-22', 'dd-mon-yy')
  group by sero_official_number
")

all_cnts_from_v_tne <-
  dbGetQuery(con, v__tne_query)
# 351

all_cnts_from_v_tne |> 
select(SERO_OFFICIAL_NUMBER) |> 
  distinct() |> 
  dim()
# 351   

sum(all_cnts_from_v_tne$TOTAL_DNF)
# [1] 45665

# should be
# Total Did Not Fish Reports
# 28,564	

# count trip_dates ----
# the same result

# count as in db with list of names ----
v__tne_query_1 <-
  stringr::str_glue("SELECT
  sero_official_number,
  count(distinct trip_id) as total_trip_ids
FROM
       safis.vessels@secapxdv_dblk.sfsc.noaa.gov v
  JOIN safis.trips_neg@secapxdv_dblk.sfsc.noaa.gov tne
  USING ( vessel_id )
WHERE
  sero_official_number in 
  ('945573',
'1116186',
'FL7991RP'
)
  AND trip_date BETWEEN TO_DATE('31-DEC-21', 'dd-mon-yy') and TO_DATE('31-DEC-22', 'dd-mon-yy')  
  GROUP by sero_official_number 
  order by total_trip_ids desc
  ")

all_cnts_from_v__tne_1 <-
  dbGetQuery(con, v__tne_query_1)
# 3

View(all_cnts_from_v__tne_1)

all_cnts_from_v__tne_1 |> 
select(SERO_OFFICIAL_NUMBER) |> 
  distinct() |> 
  dim()
# 3

all_cnts_from_v__tne_1 |> 
count(wt = TOTAL_TRIP_IDS)  
# 1504
#   TOTAL_TRIP_IDS
# 1            365
# 2            372
# 3            767
# correct

# should be
# Total Did Not Fish Reports
# 28,564	

# count again ----
v__tne_query_btw <-
  stringr::str_glue("
SELECT
sero_official_number,
  count(distinct trip_id) as total_trip_ids
FROM
       safis.vessels@secapxdv_dblk.sfsc.noaa.gov v
  JOIN safis.trips_neg@secapxdv_dblk.sfsc.noaa.gov tne
  USING ( vessel_id )
WHERE
    sero_official_number in ('{all_j_names1_500}')
  or
  sero_official_number in ('{all_j_names500_}')
  AND trip_date BETWEEN TO_DATE('31-DEC-21', 'dd-mon-yy') and TO_DATE('31-DEC-22', 'dd-mon-yy')  
  GROUP by sero_official_number 
  order by total_trip_ids desc
  ")

all_cnts_from_v__tne_query_btw <-
  dbGetQuery(con, v__tne_query_btw)

str(all_cnts_from_v__tne_query_btw)
all_cnts_from_v__tne_query_btw |> 
  filter(SERO_OFFICIAL_NUMBER == 'FL0094NN')

sum(all_cnts_from_v__tne_query_btw$TOTAL_TRIP_IDS)
# 45710
all_cnts_from_v__tne_query_btw |> 
  count(wt = TOTAL_TRIP_IDS)
# 45710

all_cnts_from_v__tne_query_btw |> 
  select(SERO_OFFICIAL_NUMBER) |> 
  distinct() |> 
  dim()
# 351
# not 0 in db - 288

# detailed_report from FHIER ----
fhier_cnts_file <-
  r"(~\R_files_local\jennys_code\Detail Report - via Valid and Renewable Permits Filter (SERO_NEW Source).xlsx)"

file.exists(fhier_cnts_file)
# T
fhier_cnts <-
  read_xlsx(fhier_cnts_file,
            .name_repair = fix_names
            # ,
      # guess_max = 21474836,
      # # read all columns as text
      # col_types = "text"
      )

glimpse(fhier_cnts)
# Vessel Official Number                        3591

fhier_cnts_g_d <-
  fhier_cnts |> 
  filter(gom_permits_ == "Y")

data_overview(fhier_cnts_g_d)
# vessel_official_number                        1327
# total_did_not_fish_reports                     147

# print_df_names(fhier_cnts_g_d)
# vessel_official_number, vessel_name, effective_date, end_date, permits, sa_permits_, gom_permits_, permit_grouping_region, total_trip_notifications_fishing_intention, total_trip_notifications_no_fishing_intention, total_logbooks, total_did_not_fish_reports, total_power_down

fhier_cnts_g_d_short <-
  fhier_cnts_g_d |> 
  select(
    vessel_official_number,
    vessel_name,
    effective_date,
    end_date,
    permits,
    sa_permits_,
    gom_permits_,
    permit_grouping_region,
    total_did_not_fish_reports
  ) |>
  distinct()

# [1] 1327    9
fhier_cnts_g_d_short_not_exp <-
  fhier_cnts_g_d_short |> 
  filter(end_date <= as.Date('2022-12-31'))
dim(fhier_cnts_g_d_short_not_exp)
# 277

fhier_cnts_g_d_short_vessel_ids <-
  fhier_cnts_g_d_short |> 
  select(vessel_official_number) |> 
  distinct()

intersect(check_j_ids$VESSEL_OFFICIAL_NUMBER_GOMDNF,
        fhier_cnts_g_d_short_vessel_ids$vessel_official_number) |> 
  length()
# 1327 (the same)

length(check_j_ids$VESSEL_OFFICIAL_NUMBER_GOMDNF)
length(fhier_cnts_g_d_short_vessel_ids$vessel_official_number)
# 1327

# join with my db results ----
fhier_and_db <-
  full_join(
    fhier_cnts_g_d_short,
    all_cnts_from_v__tne_query_btw,
    join_by(vessel_official_number == SERO_OFFICIAL_NUMBER)
  )

# View(fhier_and_db)
# [1] 1327   10
