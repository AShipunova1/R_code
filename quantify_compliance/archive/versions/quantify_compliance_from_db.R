# quantify_compliance
# TODO 2023 separately for "both" permits
library(zoo)
library(gridExtra)
library(cowplot)

source("~/R_code_github/useful_functions_module.r")
my_paths <- set_work_dir()

source("~/R_code_github/quantify_compliance/get_data.R")
source("~/R_code_github/quantify_compliance/quantify_compliance_functions.R")

# using data from db
# add year_month ----
compl_err_db_data_m <-
  compl_err_db_data %>%
  dplyr::mutate(year_month = as.yearmon(comp_week_start_dt))

# ---- separate SA and GOM permits ----
compl_err_db_data_permit_grps <-
  separate_permits_into_3_groups(compl_err_db_data_m,
                                 "permit_group")
  
# View(compl_err_db_data_permit_grps)

# SA only ----

# at least 1 logbook or no fish report per week
# "No REPORT" err
# How many non-compliant in each week?
# For a given month:
# 100% - the total # of non-compl. vessels
# x%   - submitted 1 week, 2 weeks etc.
# proportion of weeks rep. are missing

# View(compl_clean_sa_vs_gom_m_int)

sa_compl_err_db_data_permit_grps <-
  compl_err_db_data_permit_grps %>% 
  filter(permit_sa_gom == "sa_only")

dim(sa_compl_err_db_data_permit_grps)
# [1] 39270    40

# View(sa_compl_err_db_data_permit_grps)

### SA errors distribution ----
sa_compl_err_db_data_permit_grps %>% dplyr::count(comp_error_type_cd)
# 1      NO_TRIP_FOUND 39270
# NO_TRIP_FOUND	= NO REPORT
#   error_type_wo_desc     n
# 1          NO REPORT 61294
# 2               <NA> 83893
# > 

### one month test ----
sa_compl_err_db_data_permit_grps_nc <-
  sa_compl_err_db_data_permit_grps %>% 
  filter(is_comp == 0) %>% 
  filter(is_comp_override == 0) %>% 
  filter(year_month == "Dec 2022") %>%
  dplyr::count(vessel_official_nbr, name = "id_n") %>%
  # how many non_compliant this month
  dplyr::count(id_n, name = "non_compl_weeks_in_month")
#   id_n non_compl_weeks_in_month
# 1    1                       15
# 2    2                       19
# 3    3                       35
# 4    4                      361

# with filter(is_comp_override == 0)
#   id_n non_compl_weeks_in_month
# 1    1                       14
# 2    2                       19
# 3    3                       34
# 4    4                      349

# test
sa_compl_err_db_data_permit_grps %>% 
  filter(is_comp == 0) %>% 
  filter(is_comp_override == 0) %>% 
  filter(year_month == "Dec 2022") %>%
  select(vessel_official_nbr) %>%
  unique() %>% 
  dim()
# 416 = 349+34+19+14

## non compliant only ----
sa_compl_err_db_data_permit_grps_nc <-
  sa_compl_err_db_data_permit_grps %>% 
  filter(is_comp == 0)

## non compliant and not overridden ----
sa_compl_err_db_data_permit_grps_nc_no <-
  sa_compl_err_db_data_permit_grps %>% 
  filter(is_comp == 0) %>% 
  filter(is_comp_override == 0)
  
### SA percentage nc weeks ----
sa_compl_err_db_data_permit_grps_nc_perc <-
  get_non_compl_week_counts_percent(sa_compl_err_db_data_permit_grps_nc,
                                    "vessel_official_nbr")

# names(sa_compl_err_db_data_permit_grps_nc_perc)

### SA plots ----
# one month plot
# gg_22_01 <- 
#   sa_compl_err_db_data_permit_grps_nc_perc %>% 
#   filter(year_month == "Jan 2022") %>%
#   ggplot(aes(non_compl_weeks, percent_nc)) +
#   geom_col()

sa_compl_err_db_data_permit_grps_nc_perc_short <-
  sa_compl_err_db_data_permit_grps_nc_perc %>%
  select(year_month, non_compl_weeks, percent_nc)

month_names <- sa_compl_err_db_data_permit_grps_nc_perc_short$year_month %>%
  unique()

gg_sa_compl_err_db_data_permit_grps_nc_perc <-
  month_names |>
  purrr::map(
    \(current_year_month)
    perc_plots_by_month(
      sa_compl_err_db_data_permit_grps_nc_perc_short,
      current_year_month
    )
  )

# gg_sa_compl_err_db_data_permit_grps_nc_perc[[12]]

super_title = "SA only db: how many weeks vessels were non_compliant"

grid.arrange(grobs = gg_sa_compl_err_db_data_permit_grps_nc_perc,
             top = super_title,
             # left = my_legend,
             ncol = 4)

# SA + dual for 2023 (TODO) ----

# GOM + dual for 2022 ----

## requirements ----
# e.comp_error_type_cd = 'DECL_NO_TRIP'
# AND TRUNC(SYSDATE) > TRUNC(tn.trip_start_date)
# )
# OR -- Past activity date
# (
# e.comp_error_type_cd = 'TRIP_NO_DECL'

# AND TRUNC(SYSDATE) < (TRUNC(c.comp_week_end_dt) + 14) THEN -- Not past allotted correction time

# SA:
#    TRUNC(SYSDATE) >= (TRUNC(c.comp_week_end_dt) + 24) THEN -- Past allotted correction time

# select * from srh.srfh_for_hire_type@secapxdv_dblk.sfsc.noaa.gov
# 1	HEADBOAT
# 2	CHARTER
# 42	RECREATIONAL
# 81	COMMERCIAL
# 62	UNKNOWN

## not SA 2022 ----
gom_compl_err_db_data_permit_grps <-
  compl_err_db_data_permit_grps %>%
  filter(comp_year == '2022') %>%
  filter(!(permit_sa_gom == "sa_only"))

dim(compl_err_db_data_permit_grps)
# [1] 44930    39
# [1] 44662    40

dim(gom_compl_err_db_data_permit_grps)
# [1] 5405   39
# [1] 2810   40

gom_compl_err_db_data_permit_grps %>%
  dplyr::count(comp_error_type_cd)
#     comp_error_type_cd    n
# 1         DECL_NO_TRIP  761
# 2        NO_TRIP_FOUND 3456
# 3 SUBMIT_AFTER_ARRIVAL  578
# 4     TRIP_BEFORE_DECL   20
# 5         TRIP_NO_DECL  430
# 6   VAL_ERROR_TRIP_GOM   65
# 7     VMS_DECL_NO_TRIP   95

# names(gom_compl_err_db_data_permit_grps) %>% paste0(collapse = ", ")

# [1] "srh_vessel_comp_id, srh_vessel_comp_err_id, table_pk, comp_error_type_cd, is_override, override_dt, override_user_id, override_cmt, is_send_to_vesl, send_to_vesl_dt, send_to_vesl_user_id, is_pa_review_needed, pa_review_needed_dt, pa_review_needed_user_id, is_pa_reviewed, pa_reviewed_dt, pa_reviewed_user_id, pa_reviewed_cmt, val_tr_res_id, vms_table_pk, srh_vessel_id, safis_vessel_id, vessel_official_nbr, permit_group, prm_grp_exp_date, comp_year, comp_week, comp_week_start_dt, comp_week_end_dt, is_created_period, is_comp, is_comp_override, comp_override_dt, comp_override_user_id, srfh_for_hire_type_id, comp_override_cmt, is_pmt_on_hold, srfh_assignment_id, permit_sa_gom"

## keep fewer columns ----
gom_compl_err_db_data_permit_grps_short <-
  gom_compl_err_db_data_permit_grps %>% 
  select(
    c(
      comp_error_type_cd,
      vessel_official_nbr,
      permit_group,
      comp_year,
      comp_week,
      comp_week_start_dt,
      comp_week_end_dt,
      is_comp,
      is_comp_override,
      comp_override_cmt,
      year_month,
      permit_sa_gom
    )
  )

# View(gom_compl_err_db_data_permit_grps_short)

## GOM + dual 2022 non compliant and not overridden----

gom_compl_err_db_data_permit_grps_short_nc_no <-
  gom_compl_err_db_data_permit_grps_short %>% 
  filter(is_comp == 0 & is_comp_override == 0)

dim(gom_compl_err_db_data_permit_grps_short_nc_no)
# Rows: 613
# Columns: 12

gom_compl_err_db_data_permit_grps_short %>% 
  dplyr::count(comp_error_type_cd)
#     comp_error_type_cd    n
# 1         DECL_NO_TRIP  761
# 2        NO_TRIP_FOUND 3456
# 3 SUBMIT_AFTER_ARRIVAL  578
# 4     TRIP_BEFORE_DECL   20
# 5         TRIP_NO_DECL  430
# 6   VAL_ERROR_TRIP_GOM   65
# 7     VMS_DECL_NO_TRIP   95

gom_compl_err_db_data_permit_grps_short_nc_no %>% 
    dplyr::count(comp_error_type_cd)
# 1      NO_TRIP_FOUND 613 
# TODO: why sa err?

# names(gom_compl_err_db_data_permit_grps_short_nc)
perc_gom_compl_err_db_data_permit_grps_short_nc_no <-
  get_non_compl_week_counts_percent(gom_compl_err_db_data_permit_grps_short_nc_no,
                                    "vessel_official_nbr")

# View(perc_gom_compl_err_db_data_permit_grps_short_nc_no)

# gg_22_01_gom <- 
#   perc_gom_compl_err_db_data_permit_grps_short_22_clean_nc %>%
#   filter(year_month == "Jan 2022") %>%
#   ggplot(aes(non_compl_weeks, percent_nc)) +
#   geom_col()

## GOM 2022 plots ----
gom22_month_names <- perc_gom_compl_err_db_data_permit_grps_short_nc_no$year_month %>%
  unique() %>% 
  sort()

# dplyr::glimpse(gom22_month_names)

gg_perc_gom_compl_err_db_data_permit_grps_short_nc_no <-
  gom22_month_names |>
  purrr::map(
    \(current_year_month)
    perc_plots_by_month(
      perc_gom_compl_err_db_data_permit_grps_short_nc_no,
      current_year_month
    )
  )

super_title = "GOM + dual 2022 from db: how many weeks vessels were non_compliant and not overridden"
grid.arrange(grobs =
               gg_perc_gom_compl_err_db_data_permit_grps_short_nc_no,
             top = super_title,
             ncol = 4)

# percent of non_compliant and compliant ----
compl_err_db_data_permit_grps %>% dplyr::count(permit_sa_gom, is_comp)
#   permit_sa_gom is_comp     n
# 1          both       0  3453
# 2          both       1   611
# 3      gom_only       0  1185
# 4      gom_only       1   272
# 5       sa_only       0 37540
# 6       sa_only       1  2558

compl_err_db_data_permit_grps %>% 
  dplyr::mutate(nc_no = is_comp | is_comp_override) %>% 
  dplyr::count(permit_sa_gom, nc_no)
