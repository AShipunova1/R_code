# plot intervals ----
library(ggplot2)
library(ggbreak)

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
y25_all <- 
  y25 |> 
  as.data.frame()

names(y25_all) <- "the_date"

y25_all <-
  y25_all |>
  mutate(
    week_num = isoweek(the_date),
    the_year = isoyear(the_date),
    week_start = floor_date(the_date, "week", week_start = 1),
    week_end = ceiling_date(the_date, "week", week_start = 1) - 1,
    year_start = floor_date(the_date, "year", week_start = 1),
    year_end = ceiling_date(the_date, "year", week_start = 1) - 1
  )

head(y25_all)

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

gg_full <-
  ggplot(data = y25_all, aes(week_start, 0, label = week_num, color = week_num)) +
  geom_point(size = 3) +
  geom_segment(aes(week_start, 0, xend = week_end, yend = 0)) +
  geom_text(
    check_overlap = TRUE,
    vjust = 0,
    nudge_y = 0.1
  ) +
  theme_bw() +
  ylim(-1, 1)
# p + coord_cartesian(xlim =c(Sys.Date() - 30, NA), ylim = c(10, 20))

gg_gap <-
  gg_full +
  scale_x_break(c(ymd("2025-02-1"), ymd("2025-11-30"))) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b")

color_25 <- "orange"
gg_gap_25 <-
  gg_gap +
  geom_vline(xintercept = ymd("2025-01-1"), color = color_25) +
  geom_vline(xintercept = ymd("2025-12-31"), color = color_25) +
  annotate(
    "text",
    x = ymd("2024-12-31"),
    y = 0.5,
    label = "2025-01-01",
    angle = 90,
    color = color_25
  ) +
  annotate(
    "text",
    x = ymd("2025-12-31") - 1,
    y = 0.5,
    label = "2025-12-31",
    angle = 90,
    color = color_25
  ) +
  geom_segment(aes(
    x = ymd("2025-01-01"),
    y = 0.25,
    xend = ymd("2025-12-31"),
    yend = 0.25
  ), colour = color_25) +
  annotate(
    "text",
    x = ymd("2025-1-15") - 1,
    y = 0.35,
    label = "Year 2025",
    color = color_25
  )

# lubridate::wday("2025-01-01", week_start = 1, label = T)
# Wed

first_week_start <- floor_date(ymd("2024-12-31"), "week", week_start = 1)
first_week_end <- ceiling_date(ymd("2024-12-31"), "week", week_start = 1) - 1

color_24 <- "darkgreen"

gg_gap_25_24 <-
  gg_gap_25 +
  geom_segment(aes(
    x = first_week_start,
    xend = ymd("2024-12-31") + 1,
    y = -0.25,
    yend = -0.25
  ),
  colour = color_24) +
  annotate(
    "text",
    x = first_week_end - 9,
    y = -0.35,
    label = "Year 2024",
    color = color_24
  ) +
  geom_point(aes(x = first_week_start, y = -0.25),
             shape = 18,
             color = color_24)

# gg_gap_25_24_26

last_day <- ymd("2025-12-31")
last_week_start <- floor_date(last_day, "week", week_start = 1)
last_week_end <- ceiling_date(last_day, "week", week_start = 1) - 1

color_26 <- "green"

gg_gap_25_24 +
  geom_segment(aes(
    x = last_day + 1,
    xend = last_week_end,
    y = -0.25,
    yend = -0.25
  ),
  colour = color_26) +
  annotate(
    "text",
    x = last_day + 3,
    y = -0.35,
    label = "2026",
    color = color_26
  ) +
  geom_point(aes(x = last_week_end, y = -0.25),
             shape = 18,
             color = color_26)

