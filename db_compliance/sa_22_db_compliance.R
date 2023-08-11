# run from db_compliance.R
# SA 2022 compliance ----
# # There should be at least one logbook or one DNFs filed for any given week except the last one (can submit the following Tuesday).
# # DNFs should not be submitted more than 30 days in advance
#
# # all weeks of 2022 * all vessels
# # SA: each can have:
# # 1) a permit
# # 2) a trip
# # 3) a negative report
# # 1 only
# # 1,2
# # 1,3
# # 2 only
# # 3 only
# # 2,3?

# SA compliance by year ----
## get sa only vsls ----
v_p__t__tne_d_weeks_sa <-
  v_p__t__tne_d_weeks |>
  filter(permit_sa_gom_dual == "sa_only")
dim(v_p__t__tne_d_weeks_sa)
# [1] 90766    15
# [1] 194697     92

## reports_exists filter ----
reports_exists_filter <- rlang::quo(
  !(is.na(rep_type.t) & is.na(rep_type.tne))
)

## mark weekly compliance ----
tic("v_p__t__tne_d_weeks_sa_compl")
v_p__t__tne_d_weeks_sa_compl_w <-
  v_p__t__tne_d_weeks_sa |>
  group_by(PERMIT_VESSEL_ID,
           VESSEL_VESSEL_ID,
           WEEK_OF_YEAR,
           date_y_m,
           YEAR) |>
  # not compliant if both reports (trips and t negative) are absent
  mutate(sa_compl_week = case_when(!!reports_exists_filter ~
                                "yes",
                              .default = "no")) |>
  ungroup()
toc()
# v_p__t__tne_d_weeks_sa_compl: 28.39 sec elapsed
# v_p__t__tne_d_weeks_sa_compl: 22.39 sec elapsed
# v_p__t__tne_d_weeks_sa_compl_w: 33.11 sec elapsed

dim(v_p__t__tne_d_weeks_sa_compl_w)
# [1] 90766    16
# [1] 194697     93

## count compl weeks ----
# Do not group by year, the last week of 2021 should be counted together with 2022

v_p__t__tne_d_weeks_sa_compl_cnt_w <-
  v_p__t__tne_d_weeks_sa_compl_w |>
  group_by(PERMIT_VESSEL_ID,
           VESSEL_VESSEL_ID) |>
  mutate(compl_w_cnt = n_distinct(WEEK_OF_YEAR)) |>
  ungroup()

dim(v_p__t__tne_d_weeks_sa_compl_cnt_w)
# [1] 90766    16
# [1] 194697     94

### check compl week count ----
v_p__t__tne_d_weeks_sa_compl_cnt_w |>
  filter(PERMIT_VESSEL_ID == "FL4430NN") |>
  select(WEEK_OF_YEAR, date_y_m, all_of(starts_with("rep_type")), compl_w_cnt) |>
    distinct() |>
    dim()
# 14 distinct weeks
# 17 rows bc some weeks are in 2 month, e.g. 48 in Nov 2022 and Dec 2022

## compliance per year ----
v_p__t__tne_d_weeks_sa_compl_cnt_w_compl22 <-
  v_p__t__tne_d_weeks_sa_compl_cnt_w |>
  mutate(compl_2022 =
           case_when(
    !!reports_exists_filter &
      compl_w_cnt >= permit_weeks_amnt_22 ~ "yes",
           .default = "no")
  ) |>
  ungroup()

dim(v_p__t__tne_d_weeks_sa_compl_cnt_w_compl22)
# [1] 90766    17
# [1] 194697     95

v_p__t__tne_d_weeks_sa_compl_cnt_w_compl22 |> 
  count(ACTIVITY_TYPE)
# 1             0  64007
# 2             2      7
# 3             8     22
# 4            80    229
# 5            NA 130432

# v_p__t__tne_d_weeks_sa_compl_cnt_w_compl22 |>
#   # select(PERMIT_VESSEL_ID, ACTIVITY_TYPE, all_of(starts_with("UE"))) |>
#   select(PERMIT_VESSEL_ID, ACTIVITY_TYPE, UE.t) |>
#   distinct() |>
#   filter(ACTIVITY_TYPE %in% c("2", "8")) |>
#   head(10)

### fewer columns ----
v_p__t__tne_d_weeks_sa_compl_cnt_w_compl22_short <-
  v_p__t__tne_d_weeks_sa_compl_cnt_w_compl22 |>
  select(
    PERMIT_VESSEL_ID,
    permit_2022_int,
    permit_weeks_amnt_22,
    YEAR,
    compl_w_cnt,
    compl_2022,
    rep_type.t,
    rep_type.tne
  ) |>
  distinct()

dim(v_p__t__tne_d_weeks_sa_compl_cnt_w_compl22_short)
# [1] 5275    6
# [1] 6627    8
# [1] 4934    8 (metrics vsls)

v_p__t__tne_d_weeks_sa_compl_cnt_w_compl22_short |>
  filter(compl_2022 == "yes") |>
  head() |>
  glimpse()
# $ PERMIT_VESSEL_ID     <chr> "FL4430NN", "FL2698TE", "FL2698TE", "FL2720R…
# $ permit_2022_int      <Interval> 2022-10-11 00:00:00 EDT--2022-12-30 19:…
# $ permit_weeks_amnt_22 <dbl> 12, 32, 32, 52, 52, 52
# $ YEAR                 <dbl> 2022, 2022, 2021, 2022, 2021, 2022
# $ compl_w_cnt          <int> 14, 53, 53, 53, 53, 52
# $ compl_2022           <chr> "yes", "yes", "yes", "yes", "yes", "yes"
# $ rep_type.t           <chr> NA, NA, NA, NA, NA, "trips"
# $ rep_type.tne         <chr> "trips_neg", "trips_neg", "trips_neg", "trip…


v_p__t__tne_d_weeks_sa_compl_cnt_w_compl22_short |>
filter(PERMIT_VESSEL_ID == "FL2698TE") |>
  glimpse()
# $ permit_weeks_amnt_22 <dbl> 32, 32
# $ YEAR                 <dbl> 2022, 2021
# $ compl_w_cnt          <int> 53, 1
# $ compl_2022           <chr> "yes", "no"
# w/o join_by YEAR:
# $ compl_w_cnt          <int> 53, 53
# $ compl_2022           <chr> "yes", "yes"

# TODO: check year = NA

## plot SA year ----
length(unique(v_p__t__tne_d_weeks_sa_compl_cnt_w_compl22_short$PERMIT_VESSEL_ID))
# PERMIT_VESSEL_ID     3956
# 2302 (from metrics)

sa_compl_cnts <-
  v_p__t__tne_d_weeks_sa_compl_cnt_w_compl22_short |>
  select(PERMIT_VESSEL_ID,
         compl_2022) |>
  distinct() |>
  add_count(compl_2022, name = "total_compl_y")

sa_compl_cnts |>
  select(compl_2022, total_compl_y) |>
  distinct()
# 2 no                  2700
# 1 yes                 1257
#   compl_2022 total_compl_y
#   <chr>              <int>
# 1 yes                 1234
# 2 no                  1069

# compl
# 1262 * 100 / (3956)
# 32%
# 1257 * 100 / (3956)
# 31.77452
1234 * 100 / (1234 + 1069)
# [1] 53.58228

# no
# 2695 * 100 / (3956)
# 68%
# 2700 * 100 / (3956)
# [1] 68.25076
1069 * 100 / (1234 + 1069)
# [1] 46.41772

sa_compl_cnts_perc <-
  sa_compl_cnts |>
  mutate(total_vsls = n_distinct(PERMIT_VESSEL_ID)) |>
  select(-PERMIT_VESSEL_ID) |>
  distinct() |>
  group_by(compl_2022) |>
  mutate(compl_perc =
           total_compl_y * 100 / (total_vsls)) |>
  ungroup()

# (was 41% yes vs. 59% no from 2178 vessels)
# print_df_names(sa_compl_cnts_perc)
sa22_title = "SA Only Permitted Vessels (Total permitted: {sa_compl_cnts_perc$total_vsls})"

compl_2022_ord <- factor(sa_compl_cnts_perc$compl_2022,
                         levels = c("yes", "no"))
year_plot_sa <-
  sa_compl_cnts_perc %>%
  ggplot(aes(x = compl_2022_ord,
             y = compl_perc,
             fill = compl_2022)) +
  # geom_col(position = "dodge") +
  geom_col() +
  ylim(0, 100) +
  labs(title = str_glue(sa22_title),
       x = "",
       # x = "Compliant",
       y = "") +
  geom_text(aes(label = paste0(round(compl_perc, 1), "%")),
            position = position_stack(vjust = 0.5)) +
  scale_fill_manual(
    values =
      c("yes" = "turquoise1",
        "no" = "yellow"),
    name = "Is compliant?",
    labels = c("no", "yes")
  )

# year_plot_sa

# SA compliance by month ----
v_p__t__tne_d_weeks_sa_compl_cnt_m <-
  v_p__t__tne_d_weeks_sa_compl_w |>
  group_by(PERMIT_VESSEL_ID,
           VESSEL_VESSEL_ID,
           date_y_m) |>
  mutate(compl_w_cnt = n_distinct(WEEK_OF_YEAR)) |>
  ungroup()

View(v_p__t__tne_d_weeks_sa_compl_cnt_m)