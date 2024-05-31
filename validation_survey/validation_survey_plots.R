# plot intervals ----
library(ggplot2)

big_diff_times <- 
  lgb_join_i1__t_diff_short__w_int_all_dup_rm__int_dup_rm_short |> 
  filter(big_diff_time == "yes") |> 
  mutate(int_before_trip = trip_end_interview_diff < 1,
         diff_as_num = as.numeric(trip_end_interview_diff) / 60)

# View(big_diff_times)
# 145

# big_diff_times |>
#   ggplot(aes(x = interview_date_time, y = int_before_trip, colour = int_before_trip)) +
#   geom_segment(aes(xend = trip_end_date_time, yend = int_before_trip),
#                colour = "black") +
#   geom_point(size = 3) +
#   geom_point(aes(x = trip_end_date_time), size = 3) +
#   theme_bw() +
#   theme(legend.position = "none")

trip_int_diff_plot <- 
 big_diff_times |>
  ggplot(
    aes(
      x = trip_end_date_time,
      xend = interview_date_time,
      y = diff_as_num,
      # yend = diff_as_num,
      yend = 0,
      color = diff_as_num
    )
  ) + geom_segment(size = 3)

trip_int_diff_vsl_plot <-
  big_diff_times |>
  ggplot(aes(x = trip_end_date_time, y = VESSEL_OFFICIAL_NBR, colour = VESSEL_OFFICIAL_NBR)) +
  geom_segment(aes(xend = interview_date_time, yend = VESSEL_OFFICIAL_NBR),
               colour = "black") +
  geom_point(size = 3) +
  geom_point(aes(x = interview_date_time), size = 3) +
  theme_bw() +
  theme(legend.position = "none")

# a <- lubridate::ymd_hms("2022-06-01 07:09:00")
# month(a)

trip_int_diff_vsl_3m_plot <-
  big_diff_times |>
  filter(month(trip_end_date_time) == 7) |>
  ggplot(aes(x = trip_end_date_time, 
             y = VESSEL_OFFICIAL_NBR, 
             colour = VESSEL_OFFICIAL_NBR,
             label = diff_as_num)) +
  geom_segment(aes(xend = interview_date_time, 
                   yend = VESSEL_OFFICIAL_NBR),
               colour = "black") +
  geom_point(size = 3) +
  geom_point(aes(x = interview_date_time), size = 3) +
  theme_bw() +
  theme(legend.position = "none")

trip_int_diff_vsl_3m_plot +
  geom_text(check_overlap = TRUE,
            vjust = 0,
            nudge_y = 0.3)
# 
#             vjust = 0)

# ---- year/week
library(lubridate)

y25 <-
  seq(ymd('2024-12-29'), ymd('2026-1-3'), by = 'day')

y25 |> head()
y25 |> tail()
isoweek(y25) |> head()
isoweek(y25) |> tail()

y25_b <-
  seq(ymd('2024-12-29'), ymd('2025-2-1'), by = 'day') |>
  as.data.frame()

names(y25_b) <- "the_date"

# isoweek(y25_b) |> tail()

y25_e <-
  seq(ymd('2025-11-30'), ymd('2026-1-3'), by = 'day') |>
  as.data.frame() 

names(y25_e) <- "the_date"

# y25_b |> head()
# y25_e |> tail()
# isoweek(y25_b) |> head()
# isoweek(y25_b) |> tail()
# 
# isoweek(y25_e) |> head()
# isoweek(y25_e) |> tail()

y25_b_all <-
  y25_b |>
  mutate(
    week_num = isoweek(the_date),
    the_year = isoyear(the_date),
    week_start = floor_date(the_date, "week", week_start = 1),
    week_end = ceiling_date(the_date, "week", week_start = 1) - 1,
    year_start = floor_date(the_date, "year", week_start = 1),
    year_end = ceiling_date(the_date, "year", week_start = 1) - 1
  )

y25_b_all |> glimpse()

y25_e_all <-
  y25_e |>
  mutate(
    week_num = isoweek(the_date),
    the_year = isoyear(the_date),
    week_start = floor_date(the_date, "week", week_start = 1),
    week_end = ceiling_date(the_date, "week", week_start = 1) - 1,
    year_start = floor_date(the_date, "year", week_start = 1),
    year_end = ceiling_date(the_date, "year", week_start = 1) - 1
  )

tail(y25_e_all, 10)

y25_b_all |> print_df_names()

ggplot(data = y25_b_all,
       aes(week_start, 0,
       label = week_num,
       color = week_num
       )) +
  geom_point() +
  geom_point(data = y25_e_all,
       aes(week_start, 0,
       label = week_num,
       color = week_num
       )) +
  geom_segment(data = y25_b_all, 
               aes(week_start, 0, xend = week_end, yend = 0)) +
  geom_segment(data = y25_e_all, 
               aes(week_start, 0, xend = week_end, yend = 0))

    # xend = interview_date_time, 
    #                yend = VESSEL_OFFICIAL_NBR),
    #            colour = "black") +
