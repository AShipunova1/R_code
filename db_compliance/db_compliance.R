# db_compliance
# Throughout the code
# "Declaration" == "trip notification"
# "Logbook"     == "trip"

# Assumptions
# 1) Disregard the time zone for trips and trip notifications

# It would be really interesting to see an after the fact analysis done for compliance with another point of view, you can query the source of the activity from the tables we download from ACCSP and the permit materialized view mv_sero_fh_permits_his and then apply the latest compliance rules (all the fields needed for compliance are in these tables), it is very different to write an analysis report after the fact than build something for day-to-day activity that it has to be assigned to multiple users step by step.
# CATCHES
# EFFORTS
# TRIPS
# TRIPS_NEG
# TRIP_NOTIFICATIONS
# VESSELS

# setup ----
library(tictoc)
library(zoo)
# install.packages("sqldf")
library(sqldf)
library(gridExtra)
library(readxl)

source("~/R_code_github/useful_functions_module.r")
my_paths <- set_work_dir()
current_project_name <- "db_compliance"

# err msg if no connection, but keep running
try(con <- connect_to_secpr())
# con <- connect_to_secpr()

# Get data ----
get_data_file_path <- file.path(
  my_paths$git_r,
  current_project_name,
  paste0("get_data_",
         current_project_name,
         ".R")
)
source(get_data_file_path)

# Clean data ----
source(file.path(my_paths$git_r,
                 current_project_name,
                 "clean_data_db_compliance.R"))

# Source SA and GOM compliance ----

# Run to get SA compliance
source(file.path(my_paths$git_r,
                 current_project_name,
                 "sa_22_db_compliance.R"))
# result:
# compliance per year per vessel
# v_p__t__tne_d_weeks_sa_compl_cnt_w_compl22_short

source(file.path(my_paths$git_r,
                 current_project_name,
                 "gom_22_db_compliance.R"))
# results:
# compliance per week
# v_p__t__tn_d_weeks_gom_short_matched_compl_w_5_overr_total_comp1_short_compl_w_short
# per year
# v_p__t__tn_d_weeks_gom_short_matched_compl_w_5_overr_total_comp1_short_compl_w_short_y
# per month
# v_p__t__tn_d_weeks_gom_short_matched_compl_w_5_overr_total_comp1_short_compl_w_short_m


# Non compliant percentage ----

# v_p__t__tn_d_weeks_gom_short_matched_compl_w_5_overr_total_comp1_short_compl_w_short
# v_p__t__tn_d_weeks_gom_short_matched_compl_w_5_overr_total_comp1_short_compl_w_short_m

# v_p__t__tn_d_weeks_gom_short_matched_compl_w_5_overr_total_comp1_short_compl_w_short_nc <-
#   v_p__t__tn_d_weeks_gom_short_matched_compl_w_5_overr_total_comp1_short_compl_w_short |>
#   dplyr::filter(compl_w_total == "no")

# v_p__t__tn_d_weeks_gom_short_matched_compl_w_5_overr_total_comp1_short_compl_w_short_nc |>
#   dim()
# [1] 7635   12
# [1] 7288   10 overr

v_p__t__tn_d_weeks_gom_short_matched_compl_w_5_overr_total_comp1_short_compl_w_short_cnt <-
  v_p__t__tn_d_weeks_gom_short_matched_compl_w_5_overr_total_comp1_short_compl_w_short |>
  dplyr::group_by(date_y_m,
           WEEK_OF_YEAR,
           compl_w_total) |>
  dplyr::mutate(vsls_nc_w = n_distinct(PERMIT_VESSEL_ID)) |>
  dplyr::ungroup()

dim(v_p__t__tn_d_weeks_gom_short_matched_compl_w_5_overr_total_comp1_short_compl_w_short_cnt)
# [1] 21551    11

v_p__t__tn_d_weeks_gom_short_matched_compl_w_5_overr_total_comp1_short_compl_w_short_cnt |>
  dplyr::filter(PERMIT_VESSEL_ID == "FL4459MW") |>
  glimpse()
# $ WEEK_OF_YEAR         <dbl> 13, 14
# $ date_y_m             <yearmon> Apr 2022, Apr 2022
# $ compl_w_total        <chr> "no", "no"
# $ vsls_nc_w            <int> 97, 143

v_p__t__tn_d_weeks_gom_short_matched_compl_w_5_overr_total_comp1_short_compl_w_short_cnt |>
    dplyr::filter(WEEK_OF_YEAR == 13) |>
    head() |>
    glimpse()
# $ PERMIT_VESSEL_ID     "FL4459MW", "FL4459PW", "FL4482NJ", "FL4482NJ", "FL3…
# $ MONTH_OF_YEAR        4, 3, 3, 4, 3, 4
# $ WEEK_OF_YEAR         13, 13, 13, 13, 13, 13
# $ date_y_m             Apr 2022, Mar 2022, Mar 2022, Apr 2022, Mar 202…
# $ compl_w_total        "no", "yes", "no", "no", "yes", "yes"
# $ vsls_nc_w            97, 251, 126, 97, 251, 151

v_p__t__tn_d_weeks_gom_short_matched_compl_w_5_overr_total_comp1_short_compl_w_short_cnt |>
    dplyr::filter(WEEK_OF_YEAR == 13 &
             date_y_m == "Mar 2022") |>
    dplyr::select(PERMIT_VESSEL_ID) |>
    dplyr::distinct() |>
    count()
# 373

v_p__t__tn_d_weeks_gom_short_matched_compl_w_5_overr_total_comp1_short_compl_w_short_cnt |>
  dplyr::select(PERMIT_VESSEL_ID,
         # date_y_m,
         WEEK_OF_YEAR,
         compl_w_total) |>
  dplyr::distinct() |>
  dplyr::group_by(
    # date_y_m,
           WEEK_OF_YEAR,
           compl_w_total) |>
  dplyr::filter(WEEK_OF_YEAR == 13) |>
  # subset(PERMIT_VESSEL_ID %in% PERMIT_VESSEL_ID[matched_compl == 'yes']) |>
  dplyr::mutate(cnt_vsls_w = n_distinct(PERMIT_VESSEL_ID)) |>
  # glimpse()
  # Rows: 418
  dplyr::ungroup() |>
  dplyr::select(-PERMIT_VESSEL_ID) |>
  dplyr::distinct() |>
  glimpse()
# $ WEEK_OF_YEAR  <dbl> 13, 13
# $ compl_w_total <chr> "no", "yes"
# $ cnt_vsls_w    <int> 144, 274
# 144 + 274 = 418

# 1) count percents - a given vsl non_compl per counted weeks total ----
## 1a) how many weeks each vessel was present in all compliant ----

weeks_per_vsl_permit_year_compl_cnt <-
  v_p__t__tn_d_weeks_gom_short_matched_compl_w_5_overr_total_comp1_short_compl_w_short_cnt |>
  dplyr::add_count(permit_2022_int,
                   VESSEL_VESSEL_ID,
                   PERMIT_VESSEL_ID,
                   compl_w_total,
                   name = "weeks_per_vessel_per_compl") %>%
  dplyr::add_count(permit_2022_int,
                   VESSEL_VESSEL_ID,
                   PERMIT_VESSEL_ID,
                   name = "total_weeks_per_vessel") %>%
  dplyr::ungroup()

weeks_per_vsl_permit_year_compl_cnt |>
  dplyr::filter(PERMIT_VESSEL_ID == "FL4463MX") |>
  View()

weeks_per_vsl_permit_year_compl_cnt |>
  dplyr::filter(!weeks_per_vessel_per_compl == total_weeks_per_vessel) |>
  dim()
# [1] 8709   13

## test 1a ----
weeks_per_vsl_permit_year_compl_cnt %>%
  dplyr::filter(PERMIT_VESSEL_ID == "1000042") |>
  dplyr::select(YEAR,
                compl_w_total,
                # compl_m,
                weeks_per_vessel_per_compl,
                total_weeks_per_vessel) %>%
  dplyr::distinct()
# 1 2022 yes 25 40
# 2 2022 no  15 40

# was
#   year  compliant_ weeks_per_vessel_per_compl total_weeks_per_vessel
# 1 2022 YES 50 52
# 2 2022 NO 2 52

# HERE
nc_2022_sa_only_test <-
  weeks_per_vsl_permit_year_compl_cnt %>%
  dplyr::filter(
    # year_permit == "2022 sa_only",
    # compl_w_total == "no",
    PERMIT_VESSEL_ID %in% c(
      "VA9236AV",
      "VA6784AD",
      "VA4480ZY",
      "SC9207BX",
      "SC8907DF",
      "SC8298DH"
    )
  ) %>%
  dplyr::select(PERMIT_VESSEL_ID,
                weeks_per_vessel_per_compl,
                total_weeks_per_vessel) %>%
  unique() |>
  arrange(desc(total_weeks_per_vessel))

glimpse(nc_2022_sa_only_test)
# 0

# sa only
v_p__t__tn_d_weeks |>
  dplyr::filter(
    # year_permit == "2022 sa_only",
    # compl_w_total == "no",
    PERMIT_VESSEL_ID %in% c(
      "VA9236AV",
      "VA6784AD",
      "VA4480ZY",
      "SC9207BX",
      "SC8907DF",
      "SC8298DH"
    )
  ) %>%
  View()

## 1b) percent of compl/non-compl per total weeks each vsl was present ----
count_weeks_per_vsl_permit_year_compl_p <-
  weeks_per_vsl_permit_year_compl_cnt %>%
  dplyr::mutate(percent_compl =
           weeks_per_vessel_per_compl * 100 / total_weeks_per_vessel)

glimpse(count_weeks_per_vsl_permit_year_compl_p)

# 2) split nc percentage into 4 buckets ----
## 2a Only non-compl and fewer cols ----

# print_df_names(count_weeks_per_vsl_permit_year_compl_p)

count_weeks_per_vsl_permit_year_n_compl_p_short_m <-
  count_weeks_per_vsl_permit_year_compl_p %>%
  dplyr::filter(tolower(compl_w_total) == "no") %>%
  dplyr::select(
    PERMIT_VESSEL_ID,
    permit_2022_int,
    date_y_m,
    weeks_per_vessel_per_compl,
    total_weeks_per_vessel,
    percent_compl
  ) %>%
  dplyr::distinct() |>
  unique()

count_weeks_per_vsl_permit_year_n_compl_p_short_y <-
  count_weeks_per_vsl_permit_year_compl_p %>%
  dplyr::filter(tolower(compl_w_total) == "no") %>%
  dplyr::select(
    PERMIT_VESSEL_ID,
    # permit_2022_int,
    # date_y_m,
    weeks_per_vessel_per_compl,
    total_weeks_per_vessel,
    percent_compl
  ) %>%
  dplyr::distinct() |>
  unique()

dim(count_weeks_per_vsl_permit_year_n_compl_p_short_y)
# [1] 453   4
dim(count_weeks_per_vsl_permit_year_n_compl_p_short_m)
# [1] 2293    6 (with month)

## 2b) get percentage "buckets" ----
# percent buckets
get_p_buckets <- function(my_df, field_name) {
  my_df %>%
    dplyr::mutate(
      percent_n_compl_rank =
        dplyr::case_when(
          !!sym(field_name) < 25 ~ '0<= & <25%',
          25 <= !!sym(field_name) &
            !!sym(field_name) < 50 ~ '25<= & <50%',
          50 <= !!sym(field_name) &
            !!sym(field_name) < 75 ~ '50<= & <75%',
          75 <= !!sym(field_name) ~ '75<= & <=100%'
        )
    ) %>%
    return()
}

count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_m <-
  get_p_buckets(count_weeks_per_vsl_permit_year_n_compl_p_short_m,
                "percent_compl")

count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_y <-
  get_p_buckets(count_weeks_per_vsl_permit_year_n_compl_p_short_y,
                "percent_compl")

dim(count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_y)
# [1] 453   6
dim(count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_m)
# [1] 2293    7 w month

count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_y |>
  glimpse()

### test 2 ----
# count in one bucket

count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_y %>%
  dplyr::filter(percent_n_compl_rank == '75<= & <=100%') %>%
  # dplyr::filter(date_y_m == "2022 sa_only") %>%
  dplyr::count(percent_compl,
               # date_y_m,
               name = "amount_of_occurences") %>%
  # glimpse()
#   $ percent_compl        <dbl> 75.00000, 77.27273, 77.35849, 78.26087, 78.787…
# $ amount_of_occurences <int> 4, 11, 12, 9, 9, 2, 9, 9, 9, 9, 20, 5, 5, 21, …
  dplyr::arrange(desc(percent_compl)) %>%
  # sum amount_of_occurences
  dplyr::count(wt = amount_of_occurences)
# 1848 m
# 262 y

# 3) count how many in each bucket ----

count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_cnt_in_b_m <-
  count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_m %>%
  dplyr::add_count(percent_n_compl_rank,
                   name = "cnt_v_in_bucket")

count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_cnt_in_b_y <-
  count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_y %>%
  dplyr::add_count(percent_n_compl_rank,
                   name = "cnt_v_in_bucket")

### test 3 ----
count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_cnt_in_b_y %>%
  # dplyr::filter(year_permit == "2022 sa_only") %>%
  dplyr::select(percent_n_compl_rank,
                cnt_v_in_bucket) %>%
  dplyr::distinct() %>%
  dplyr::add_count(wt = cnt_v_in_bucket, name = "total_per_y_r") %>%
  dplyr::arrange(percent_n_compl_rank) %>%
  str()
# $ percent_n_compl_rank: chr [1:4] "0<= & <25%" "25<= & <50%" "50<= & <75%" "75<= & <=100%"
 # $ cnt_v_in_bucket     : int [1:4] 127 29 35 262
 # $ total_per_y_r       : int [1:4] 453 453 453 453

# 127+29+35+262
# 453
# correct

# 4) cnt percents of (3) ----

count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_cnt_in_b_perc_y <-
  count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_cnt_in_b_y %>%
  # cnt vessels per year, permit region and compliance
  dplyr::add_count(name = "vsls_per_y_r") %>%
  dplyr::mutate(perc_vsls_per_y_r_b = cnt_v_in_bucket * 100 / vsls_per_y_r) %>%
  dplyr::mutate(perc_labels = paste0(round(perc_vsls_per_y_r_b, 1), "%"))

count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_cnt_in_b_perc_m <-
  count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_cnt_in_b_m %>%
  # cnt vessels per year, permit region and compliance
  dplyr::add_count(date_y_m,
    name = "vsls_per_y_r") %>%
  dplyr::mutate(perc_vsls_per_y_r_b = cnt_v_in_bucket * 100 / vsls_per_y_r) %>%
  dplyr::mutate(perc_labels = paste0(round(perc_vsls_per_y_r_b, 1), "%"))

dim(count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_cnt_in_b_perc_y)
# 453
dim(count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_cnt_in_b_perc_m)
# 2293

### check 4 ----
count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_cnt_in_b_perc_y %>%
  # dplyr::filter(year_permit == "2022 sa_only") %>%
  dplyr::select(percent_n_compl_rank,
                perc_vsls_per_y_r_b) %>%
  unique() %>%
  dplyr::arrange(percent_n_compl_rank) %>%
  head()
#   percent_n_compl_rank perc_vsls_per_y_r_b
#   <chr>                              <dbl>
# 1 0<= & <25%                         31.0
# 2 25<= & <50%                        13.3
# 3 50<= & <75%                         6.59
# 4 75<= & <=100%                      49.1

# GOM new
# 1 0<= & <25%                         28.0
# 2 25<= & <50%                         6.40
# 3 50<= & <75%                         7.73
# 4 75<= & <=100%                      57.8

# 5) blue plots by year ----

# View(count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_cnt_in_b_perc)

# print_df_names(count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_cnt_in_b_perc)

# "2022: % Non-Compliant GOM + Dual Permitted Vessels Missing >25%, <=25-49.9%, <=50-74.9%, <75% of their reports"
# [subtitle this]  "(Total Non-Compliant = 304 Vessels; Active Permits = 1192 Vessels)"
# "2022: % Non-Compliant SA Only Permitted Vessels Missing >25%, <=25-49.9%, <=50-74.9%, <75% of their reports"
# [subtitle this] "(Total Non-Compliant = 1289 Vessels; Active Permits = 1707 Vessels)"
# For plot 4:
# "2023: SA + Dual Permitted SEFHIER Vessels (Total Permitted: 2235 ; Total Noncompliant: 1628; Expired Permits: 1)"

blue_year_plot_titles <-
  data.frame(
    year_permit = c("2022 sa_only",
                    "2022 gom_dual",
                    "2023 sa_dual"),
    first_part = c(
      "SA Only Permitted Vessels\n(",
      "GOM + Dual Permitted Vessels\n(",
      "2023: SA + Dual Permitted SEFHIER Vessels\n(Total Permitted = 2235 Vessels; "
    )
  )

gg_count_weeks_per_vsl_permit_year_compl_p_short_cuts_cnt_in_b_tot_perc <-
  count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_cnt_in_b_perc$year_permit %>%
  unique() %>%
  sort() %>%
  # repeat for each year_permit
  purrr::map(function(curr_year_permit) {
    # browser()
    curr_df <-
      count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_cnt_in_b_perc_y
    # %>%
      # dplyr::filter(year_permit == curr_year_permit)

    total_non_compl_df <-
      curr_df %>%
      dplyr::select(perc_vsls_per_y_r_b,
                    percent_n_compl_rank,
                    perc_labels,
                    vsls_per_y_r) %>%
      unique()

    active_permits <- curr_df %>%
      dplyr::filter(perm_exp_y == "active") %>%
      dplyr::select(exp_y_tot_cnt)

    expired_permits <- curr_df %>%
      dplyr::filter(perm_exp_y == "expired") %>%
      dplyr::select(exp_y_tot_cnt)

    # See the function definition F2
    curr_title_y_p <- make_year_permit_label(curr_year_permit)

    y_p_title <- "GOM 22"

    curr_blue_year_plot_title <-
      blue_year_plot_titles %>%
      dplyr::filter(year_permit == curr_year_permit)

    y_p_title <-
      paste0(
        curr_blue_year_plot_title$first_part,
        "Total Non-Compliant = ",
        total_non_compl_df$vsls_per_y_r,
        " Vessels; Acitve permits = ",
        active_permits$exp_y_tot_cnt,
        # "; Expired permits: ",
        # expired_permits$exp_y_tot_cnt,
        " Vessels)"
      )

    one_plot <-
      ggplot(total_non_compl_df,
             aes(x = percent_n_compl_rank,
                 y = perc_vsls_per_y_r_b)) +
      geom_col(fill = "deepskyblue") +
      labs(title = y_p_title,
           x = "",
           y = "% nc vsls per year & permit") +
      # text on bars
      geom_text(aes(label = perc_labels),
                position = position_stack(vjust = 0.5)) +
      # y axes 0 to 100
      ylim(0, 100) +
      # size of an individual plot's title
      theme(plot.title =
              element_text(size = 12))

    return(one_plot)
  })

gg_count_weeks_per_vsl_permit_year_compl_p_short_cuts_cnt_in_b_tot_perc[[3]]

## plot 2022 ----
ndash <- "\u2013"
super_title = paste0(
  "2022: % Non-Compliant Vessels Missing <25%, 25%", ndash, "49.9%, 50%", ndash, "74.9%, >=75% of their reports"
)

# footnote = textGrob(
#   "X axes is % of missing reports for non-compliant vessels",
#   gp = gpar(fontface = 3, fontsize = 10),
#   # justify left
#   # hjust = 0,
#   hjust = -1.5,
#   just = "right",
#   x = 0.01, y = 0.99,
#   vjust = 1
# )

### common y axes ----
yleft <- textGrob("% per permit region",
                  # rotate
                  rot = 90,
                  gp = gpar(fontsize = 10))

p <-
  list(gg_count_weeks_per_vsl_permit_year_compl_p_short_cuts_cnt_in_b_tot_perc[1:2])[[1]] %>%
  # remove individual x and y labels for each plot
  map( ~ .x + labs(x = NULL, y = NULL))

plot_perc_22 <- gridExtra::grid.arrange(
  grobs = p,
  left = yleft,
  top = super_title)
