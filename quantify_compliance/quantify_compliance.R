# quantify_compliance
# TODO 2023 separately for "both" permits
library(zoo)
library(gridExtra)
library(cowplot)

source("~/R_code_github/useful_functions_module.r")
my_paths <- set_work_dir()

source("~/R_code_github/quantify_compliance/get_data.R")

# using data from db
# add year_month ----
compl_err_db_data_m <-
  compl_err_db_data %>%
  mutate(year_month = as.yearmon(comp_week_start_dt))


# ---- separate SA and GOM permits ----
separate_permits_into_3_groups <- function(my_df, permit_group_field_name = "permitgroup") {
  my_df %>%
  mutate(permit_sa_gom =
           case_when(
             !grepl("RCG|HRCG|CHG|HCHG", !!sym(permit_group_field_name)) ~ "sa_only",
             !grepl("CDW|CHS|SC", !!sym(permit_group_field_name)) ~ "gom_only",
             .default = "both"
           )) %>%
    return()
}

compl_err_db_data_permit_grps <-
  separate_permits_into_3_groups(compl_err_db_data_m,
                                 "permit_group")
  
View(compl_err_db_data_permit_grps)

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

View(sa_compl_err_db_data_permit_grps)

sa_compl_err_db_data_permit_grps %>% count(comp_error_type_cd)
# 1      NO_TRIP_FOUND 39270
# NO_TRIP_FOUND	= NO REPORT
#   error_type_wo_desc     n
# 1          NO REPORT 61294
# 2               <NA> 83893
# > 


compl_clean_sa_vs_gom_m_int %>% 
  filter(compliant_ == "NO") %>% 
  filter(year_month == "Dec 2022") %>%
  count(vessel_official_number, name = "id_n") %>%
  # how many non_compliant this month
  count(id_n, name = "non_compl_weeks_in_month")
  # count(week_num, compliant_)
#    id_n non_compl_weeks_in_month
# 1     1                       25
# 2     2                       25
# 3     3                       38
# 4     4                      379

non_compl_per_week_month <-
  compl_clean_sa_vs_gom_m_int %>%
  filter(compliant_ == "NO") %>%
  count(year_month, vessel_official_number, 
        name = "non_compl_weeks") %>%
  # how many non_compliant each month
  count(year_month, non_compl_weeks, name = "non_compl_in_month") 
glimpse(non_compl_per_week_month)

# non_compl_total_nc_per_month <-
#   compl_clean_sa_vs_gom_m_int %>% 
#   filter(compliant_ == "NO") %>% 
#   count(year_month, name = "total_nc_per_month")
#   # %>% glimpse()

compl_clean_sa_vs_gom_m_int %>% 
  filter(compliant_ == "NO") %>% 
  filter(year_month == "Dec 2022") %>%
  select(vessel_official_number) %>%
  unique() %>% 
  dim()
# 467 = 25+25+38+379

names(non_compl_per_week_month)

non_compl_per_week_month_wide <-
  non_compl_per_week_month %>%
    pivot_wider(names_from = non_compl_weeks,
                values_from = non_compl_in_month,
                values_fill = 0)

names(non_compl_per_week_month_wide)

non_compl_per_week_month_wide_w_total <-
  non_compl_per_week_month_wide %>% 
  mutate(total_nc_vsl_per_month = rowSums(.[2:6]))

names(non_compl_per_week_month_wide_w_total)

non_compl_per_week_month_w_total <-
  non_compl_per_week_month_wide_w_total %>% 
    pivot_longer(-c(year_month, total_nc_vsl_per_month),
                 names_to = "non_compl_weeks",
                 values_to = "non_compl_in_month") %>% 
    mutate(percent_nc_weeks = 100 * as.integer(non_compl_in_month) / total_nc_vsl_per_month
             )
# View(non_compl_per_week_month_w_total)

gg_22_01 <- 
  non_compl_per_week_month_w_total %>% 
  filter(year_month == "Jan 2022") %>%
  ggplot(aes(non_compl_weeks, percent_nc_weeks)) +
  geom_col()

non_compl_per_week_month_w_total_short <-
  non_compl_per_week_month_w_total %>%
  select(year_month, non_compl_weeks, percent_nc_weeks)

gg_non_compl_per_week_month_w_total <-
  non_compl_per_week_month_w_total_short$year_month %>%
  unique() %>% 
  map(function(current_year_month) {
    non_compl_per_week_month_w_total_short %>%
      filter(year_month == current_year_month) %>%
      ggplot(aes(non_compl_weeks, percent_nc_weeks)) +
      geom_col(fill = "lightblue") +
      labs(title = current_year_month) %>%
      # TODO: add color, a month as a title, axes text
      return()
  })

gg_non_compl_per_week_month_w_total[[12]]

grid.arrange(grobs = gg_non_compl_per_week_month_w_total,
             # top = super_title,
             # left = my_legend,
             ncol = 4)

# SA + dual for 2023 ----

# GOM + dual for 2022 ----

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
# [1] 44662    39

dim(gom_compl_err_db_data_permit_grps)
# [1] 5405   39
# [1] 2810   40

gom_compl_err_db_data_permit_grps %>%
  count(comp_error_type_cd)
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
      permit_sa_gom
    )
  )

View(gom_compl_err_db_data_permit_grps_short)

## GOM + dual 2022 non compliant ----

gom_compl_err_db_data_permit_grps_short_22_clean_nc <-
  gom_compl_err_db_data_permit_grps_short %>% 
  filter(is_comp == 0 & is_comp_override == 0)

glimpse(gom_compl_err_db_data_permit_grps_short_22_clean_nc)
# Rows: 613
# Columns: 11

gom_compl_err_db_data_permit_grps_short %>% 
  count(comp_error_type_cd)
#     comp_error_type_cd    n
# 1         DECL_NO_TRIP  761
# 2        NO_TRIP_FOUND 3456
# 3 SUBMIT_AFTER_ARRIVAL  578
# 4     TRIP_BEFORE_DECL   20
# 5         TRIP_NO_DECL  430
# 6   VAL_ERROR_TRIP_GOM   65
# 7     VMS_DECL_NO_TRIP   95

# gom_compl_err_db_data_permit_grps_short_nc %>% 
#   count(comp_error_type_cd)
# 1      NO_TRIP_FOUND 2916

gom_compl_err_db_data_permit_grps_short_22_clean_nc %>% 
    count(comp_error_type_cd)
# 1      NO_TRIP_FOUND 613

# TODO: func get_non_compl_week_counts with params to use here

get_non_compl_week_counts_percent <- function(my_df, vessel_id_col_name) {
    my_df %>%
    # how many non_compliant weeks per vessel this month
    count(year_month, !!sym(vessel_id_col_name),
          name = "nc_weeks_per_vessl_m") %>%
    # nc weeks per month
    count(year_month, nc_weeks_per_vessl_m,
          name = "occurence_in_month") %>%
    # turn amount of nc weeks into headers, to have one row per year_month
    pivot_wider(names_from = nc_weeks_per_vessl_m,
                # number of vessels
                values_from = occurence_in_month,
                values_fill = 0) %>%
    # sum nc by month to get Total
    mutate(total_nc_vsl_per_month = rowSums(.[2:6])) %>%
    # turn to have num of weeks per month in a row
    pivot_longer(-c(year_month, total_nc_vsl_per_month),
                 names_to = "non_compl_weeks",
                 values_to = "non_compl_in_month") %>%
    # count percentage
    mutate(percent_nc = round(
      100 * as.integer(non_compl_in_month) / total_nc_vsl_per_month,
      digits = 2
    )) %>%
    return()
}

perc_gom_compl_err_db_data_permit_grps_short_22_clean_nc <-
  get_non_compl_week_counts_percent(gom_compl_err_db_data_permit_grps_short_22_clean_nc,
                                    "vessel_official_nbr")

View(perc_gom_compl_err_db_data_permit_grps_short_22_clean_nc)

# gg_22_01_gom <- 
#   perc_gom_compl_err_db_data_permit_grps_short_22_clean_nc %>%
#   filter(year_month == "Jan 2022") %>%
#   ggplot(aes(non_compl_weeks, percent_nc)) +
#   geom_col()

## all GOM 2022 plots ----
gg_gom_non_compl_per_week_month_w_total <-
  perc_gom_compl_err_db_data_permit_grps_short_22_clean_nc$year_month %>%
  unique() %>% 
  map(function(current_year_month) {
    perc_gom_compl_err_db_data_permit_grps_short_22_clean_nc %>%
      filter(year_month == current_year_month) %>%
      ggplot(aes(non_compl_weeks, percent_nc)) +
      geom_col(fill = "lightblue") +
      geom_text(aes(label = paste0(percent_nc, "%")),
                position = position_dodge(width = 0.9)
                # ,
                # vjust = -0.5
                ) +
      labs(title = current_year_month,
           # x = "",
           x = "Num of weeks",
           y = ""
           ) %>%
           # TODO: axes text
           return()
  })
gg_gom_non_compl_per_week_month_w_total[[1]]

super_title = "GOM: how many weeks vessels were non_compliant"
grid.arrange(grobs =
               gg_gom_non_compl_per_week_month_w_total,
             top = super_title,
             ncol = 4)
