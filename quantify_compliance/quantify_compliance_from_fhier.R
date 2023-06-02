# quantify_compliance

library(zoo)
library(gridExtra)
library(cowplot)

source("~/R_code_github/useful_functions_module.r")
my_paths <- set_work_dir()

source("~/R_code_github/quantify_compliance/get_data.R")

# ---- separate SA and GOM permits ----
separate_permits_into_3_groups <- function(compl_clean, permit_group_field_name = "permitgroup") {
  compl_clean %>%
  mutate(permit_sa_gom =
           case_when(
             !grepl("RCG|HRCG|CHG|HCHG", !!sym(permit_group_field_name)) ~ "sa_only",
             !grepl("CDW|CHS|SC", !!sym(permit_group_field_name)) ~ "gom_only",
             .default = "both"
           )) %>%
    return()
}

## fhier data ---

compl_clean_sa_vs_gom <- separate_permits_into_3_groups(compl_clean)

# View(compl_clean_sa_vs_gom)

# ---- add columns for month and quarter ----
compl_clean_sa_vs_gom_m <-
  compl_clean_sa_vs_gom %>%
  # add month
  mutate(year_month = as.yearmon(week_start)) %>%
  # add quarter
  mutate(year_quarter = as.yearqtr(week_start))

# ---- convert report numbers to numeric ----
compl_clean_sa_vs_gom_m_int <-
  compl_clean_sa_vs_gom_m %>%
  mutate(
    captainreports__ = as.integer(captainreports__),
    negativereports__ = as.integer(negativereports__),
    gom_permitteddeclarations__ = as.integer(gom_permitteddeclarations__)
  )

# View(err_desc_clean_headers_csv_content)

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

dim(compl_clean_sa_vs_gom)
# [1] 208893     22

# SA only ----

# at least 1 logbook or no fish report per week
# "No REPORT" err
# How many non-compliant in each week?
# For a given month:
# 100% - the total # of non-compl. vessels
# x%   - submitted 1 week, 2 weeks etc.
# proportion of weeks rep. are missing

# View(compl_clean_sa_vs_gom_m_int)

## get SA only ----
sa_compl_clean_sa_vs_gom_m_int <-
  compl_clean_sa_vs_gom_m_int %>% 
  filter(permit_sa_gom == "sa_only")

dim(sa_compl_clean_sa_vs_gom_m_int)
# 123,453

## SA only non compliant ----
sa_compl_clean_sa_vs_gom_m_int %>% 
  count(compliant_)
#     compliant_     n
# 1 NO         38259
# 2 YES        85194

sa_compl_clean_sa_vs_gom_m_int %>% 
  count(compliant_, overridden_)
#   compliant_ overridden_     n
#   <chr>      <chr>       <int>
# 1 NO         NO          33322
# 2 NO         YES          4937
# 3 YES        NO          85194

sa_compl_clean_sa_vs_gom_m_int_non_c <-
  sa_compl_clean_sa_vs_gom_m_int %>% 
  filter(compliant_ == "NO")

### test one month ----
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

sa_compl_clean_sa_vs_gom_m_int_non_c_perc <-
  get_non_compl_week_counts_percent(sa_compl_clean_sa_vs_gom_m_int_non_c,
                                    "vessel_official_number")
View(sa_compl_clean_sa_vs_gom_m_int_non_c_perc)

## SA only plots ----
### one plot ----
gg_22_01 <- 
  sa_compl_clean_sa_vs_gom_m_int_non_c_perc %>% 
  filter(year_month == "Jan 2022") %>%
  ggplot(aes(non_compl_weeks, percent_nc)) +
  geom_col()


sa_compl_err_db_data_permit_grps_nc_perc_short$year_month %>%
  unique()

gg_sa_compl_clean_sa_vs_gom_m_int_non_c_perc <-
  unique(sa_compl_clean_sa_vs_gom_m_int_non_c_perc$year_month) |>
  map(
    \(current_year_month)
    perc_plots_by_month(
      sa_compl_clean_sa_vs_gom_m_int_non_c_perc,
      current_year_month
    )
  )

gg_sa_compl_clean_sa_vs_gom_m_int_non_c_perc[[12]]

super_title = "SA only from csvs: how many weeks vessels were non_compliant"

grid.arrange(grobs = gg_sa_compl_clean_sa_vs_gom_m_int_non_c_perc,
             top = super_title,
             # left = my_legend,
             ncol = 4)

## Same using only report counts without "compliant__" ----

# GOM + dual ----
gom_all_compl_clean_sa_vs_gom_m_int <-
  compl_clean_sa_vs_gom_m_int %>% 
  filter(!(permit_sa_gom == "sa_only"))

str(gom_all_compl_clean_sa_vs_gom_m_int)
# [1] 85440    24

gom_all_compl_clean_sa_vs_gom_m_int_even <-
  gom_all_compl_clean_sa_vs_gom_m_int %>%
  mutate(
    logb_n_decl = captainreports__ + gom_permitteddeclarations__,
    even_num_rep = dplyr::if_else(((logb_n_decl %% 2) == 0),
                                  "even", "odd")
  )
View(gom_all_compl_clean_sa_vs_gom_m_int_even)

gom_all_compl_clean_sa_vs_gom_m_int_even %>% 
  count(even_num_rep, compliant_)
#   even_num_rep compliant_     n
#   <chr>        <chr>      <int>
# 1 even         NO          3542 ?
# 2 even         YES        76908
# 3 odd          NO           373
# 4 odd          YES         4617 ?

## investigate  odd/YES ----
# gom_all_compl_clean_sa_vs_gom_m_int_even %>% 
#   filter(compliant_ == "YES" & even_num_rep == "odd") %>% View()

gom_all_compl_clean_sa_vs_gom_m_int_even %>% 
  filter(compliant_ == "YES" & even_num_rep == "odd") %>% head(1) %>% glimpse()
# $ vessel_official_number      <chr> "TX3416RA"
# $ name                        <chr> "CONTROLLED CHAOS"
# $ permitgroup                 <chr> "(CHG)885,(RCG)836"
# $ year_month                  <yearmon> Dec 2022
# $ week_num                    <int> 52
# $ week                        <chr> "52: 12/26/2022 - 01/01/2023"
# $ week_start                  <date> 2022-12-26
# $ week_end                    <date> 2023-01-01


## by "compliant?"
gom_all_compl_clean_sa_vs_gom_m_int_non_comp <-
  gom_all_compl_clean_sa_vs_gom_m_int %>% 
  filter(compliant_ == "NO")

gom_all_compl_clean_sa_vs_gom_m_int_non_comp_perc <-
  get_non_compl_week_counts(gom_all_compl_clean_sa_vs_gom_m_int_non_comp)
# numbers are too low

# GOM + dual from db ----

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

# ===
gom_compl_err_db_data_sa_g <-
  compl_err_db_data_sa_g %>% 
  filter(!(permit_sa_gom == "sa_only"))
dim(compl_err_db_data_sa_g)
# [1] 44930    39

dim(gom_compl_err_db_data_sa_g)
# [1] 5405   39

gom_compl_err_db_data_sa_g %>%
  count(comp_error_type_cd)
#     comp_error_type_cd    n
# 1         DECL_NO_TRIP  761
# 2        NO_TRIP_FOUND 3456
# 3 SUBMIT_AFTER_ARRIVAL  578
# 4     TRIP_BEFORE_DECL   20
# 5         TRIP_NO_DECL  430
# 6   VAL_ERROR_TRIP_GOM   65
# 7     VMS_DECL_NO_TRIP   95

  # select(comp_error_type_cd) %>%
  # unique()
#       comp_error_type_cd
# 1           DECL_NO_TRIP
# 2   SUBMIT_AFTER_ARRIVAL
# 7           TRIP_NO_DECL
# 8     VAL_ERROR_TRIP_GOM
# 11         NO_TRIP_FOUND
# 37      VMS_DECL_NO_TRIP
# 236     TRIP_BEFORE_DECL

# names(gom_compl_err_db_data_sa_g) %>% paste0(collapse = ", ")

# [1] "srh_vessel_comp_id, srh_vessel_comp_err_id, table_pk, comp_error_type_cd, is_override, override_dt, override_user_id, override_cmt, is_send_to_vesl, send_to_vesl_dt, send_to_vesl_user_id, is_pa_review_needed, pa_review_needed_dt, pa_review_needed_user_id, is_pa_reviewed, pa_reviewed_dt, pa_reviewed_user_id, pa_reviewed_cmt, val_tr_res_id, vms_table_pk, srh_vessel_id, safis_vessel_id, vessel_official_nbr, permit_group, prm_grp_exp_date, comp_year, comp_week, comp_week_start_dt, comp_week_end_dt, is_created_period, is_comp, is_comp_override, comp_override_dt, comp_override_user_id, srfh_for_hire_type_id, comp_override_cmt, is_pmt_on_hold, srfh_assignment_id, permit_sa_gom"

gom_compl_err_db_data_sa_g_short <-
  gom_compl_err_db_data_sa_g %>% 
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

View(gom_compl_err_db_data_sa_g_short)

gom_compl_err_db_data_sa_g_short_22_clean <-
  gom_compl_err_db_data_sa_g_short %>%
  filter(comp_year == '2022') %>%
  mutate(year_month = as.yearmon(comp_week_start_dt)) 

# TODO 2023 separately for "both" permits

gom_compl_err_db_data_sa_g_short_22_clean_nc <-
  gom_compl_err_db_data_sa_g_short_22_clean %>% 
  filter(is_comp == 0 & is_comp_override == 0)

# glimpse(gom_compl_err_db_data_sa_g_short_22_clean_nc)
# Rows: 613
# Columns: 12

gom_compl_err_db_data_sa_g_short %>% 
  count(comp_error_type_cd)
#     comp_error_type_cd    n
# 1         DECL_NO_TRIP  761
# 2        NO_TRIP_FOUND 3456
# 3 SUBMIT_AFTER_ARRIVAL  578
# 4     TRIP_BEFORE_DECL   20
# 5         TRIP_NO_DECL  430
# 6   VAL_ERROR_TRIP_GOM   65
# 7     VMS_DECL_NO_TRIP   95

gom_compl_err_db_data_sa_g_short_nc %>% 
  count(comp_error_type_cd)
# 1      NO_TRIP_FOUND 2916

gom_compl_err_db_data_sa_g_short_22_clean_nc %>% 
    count(comp_error_type_cd)
# 1      NO_TRIP_FOUND 613

# TODO change get_non_compl_week_counts params to use here

perc_gom_compl_err_db_data_sa_g_short_22_clean_nc <-
  gom_compl_err_db_data_sa_g_short_22_clean_nc %>%
  # how many non_compliant weeks per vessel this month
  count(year_month, vessel_official_nbr,
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
  ))

View(perc_gom_compl_err_db_data_sa_g_short_22_clean_nc)

gg_22_01_gom <- 
  perc_gom_compl_err_db_data_sa_g_short_22_clean_nc %>%
  filter(year_month == "Jan 2022") %>%
  ggplot(aes(non_compl_weeks, percent_nc)) +
  geom_col()

gg_gom_non_compl_per_week_month_w_total <-
  perc_gom_compl_err_db_data_sa_g_short_22_clean_nc$year_month %>%
  unique() %>% 
  map(function(current_year_month) {
    perc_gom_compl_err_db_data_sa_g_short_22_clean_nc %>%
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
