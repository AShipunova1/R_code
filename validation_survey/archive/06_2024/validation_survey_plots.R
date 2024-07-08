# plot intervals ----
library(ggplot2)
library(ggbreak)

big_diff_times <-
  lgb_join_i1__t_diff_short__w_int_all_dup_rm__int_dup_rm_short |>
  filter(big_diff_time == "yes") |>
  mutate(
    int_before_trip = trip_end_interview_diff < 1,
    diff_as_num = as.numeric(trip_end_interview_diff) / 60
  )

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
  ggplot(
    aes(
      x = trip_end_date_time,
      y = VESSEL_OFFICIAL_NBR,
      colour = VESSEL_OFFICIAL_NBR,
      label = diff_as_num
    )
  ) +
  geom_segment(aes(xend = interview_date_time, yend = VESSEL_OFFICIAL_NBR),
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

first_date <- ymd('2024-12-29')
last_date <- ymd('2026-1-3')

y24_25_26 <-
  seq(first_date, last_date, by = 'day')

y25_all <-
  y24_25_26 |>
  as.data.frame()

names(y25_all) <- "the_date"

y25_all <-
  y25_all |>
  mutate(
    week_num = week(the_date),
    the_year = year(the_date),
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

gg_full <-
  ggplot(data = y25_all, aes(week_start, 0, label = week_num, color = week_num)) +
  geom_point(size = 3) +
  geom_segment(aes(week_start, 0, xend = week_end, yend = 0)) +
  geom_text(check_overlap = TRUE,
            vjust = 0,
            nudge_y = 0.1) +
  theme_bw() +
  ylim(-1, 1)
# p + coord_cartesian(xlim =c(Sys.Date() - 30, NA), ylim = c(10, 20))

break_start <- ymd("2025-01-1") + 15
break_end <- ymd("2026-1-1") - 15
gg_gap <-
  gg_full +
  scale_x_break(c(break_start, break_end)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b")

# gg_gap

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

# gg_gap_25
# lubridate::wday("2025-01-01", week_start = 1, label = T)
# Wed

first_week_start <- floor_date(ymd("2024-12-31"), "week", week_start = 1)
first_week_end <- ceiling_date(ymd("2024-12-31"), "week", week_start = 1) - 1

color_24 <- "darkgreen"

gg_gap_25_24 <-
  gg_gap_25 +
  geom_segment(aes(
    x = ymd(first_date),
    xend = ymd("2024-12-31") + 1,
    y = -0.25,
    yend = -0.25
  ),
  colour = color_24) +
  annotate(
    "text",
    x = first_week_start - 2,
    y = -0.35,
    label = "Year 2024",
    color = color_24
  )

gg_gap_25_24

# gg_gap_25_24_26 ----

last_day <- ymd("2025-12-31")
last_week_start <- floor_date(last_day, "week", week_start = 1)
last_week_end <- ceiling_date(last_day, "week", week_start = 1) - 1

color_26 <- "green"

gg_gap_25_24_26 <-
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
  )
# geom_point(aes(x = last_week_end, y = -0.25),
#            shape = 18,
#            color = color_26)

gg_gap_25_24_26

# add trip
# add dnf
# add compliance year

# add trip ----
trip1_start <- "2024-12-30"
trip1_end <- "2025-01-02"

trip2_start <- "2025-12-30"
trip2_end <- "2026-01-02"

color_trip1 <- "purple"
gg_gap_25_24_26_tr1 <-
  gg_gap_25_24_26 +
  geom_segment(aes(
    x = ymd(trip1_start),
    xend = ymd(trip1_end),
    y = -0.5,
    yend = -0.5
  ), colour = color_trip1) +
  annotate(
    "text",
    x = ymd(trip1_start),
    y = -0.55,
    label = "Trip 1",
    color = color_trip1
  )

color_trip2 <- "violet"

gg_gap_25_24_26_tr1_tr2 <-
  gg_gap_25_24_26_tr1 +
  geom_segment(aes(
    x = ymd(trip2_start),
    xend = ymd(trip2_end),
    y = -0.5,
    yend = -0.5
  ), colour = color_trip2) +
  annotate(
    "text",
    x = ymd(trip2_start) + 3,
    y = -0.55,
    label = "Trip 2",
    color = color_trip2
  )

gg_gap_25_24_26_tr1_tr2

# isoweek(ymd("2024-12-31"))
# week(ymd("2024-12-31"))
# year(ymd("2024-12-31"))

# add compliance year

color_compliance_year <- "blue"
gg_gap_25_24_26_tr1_tr2_compl <-
  gg_gap_25_24_26_tr1_tr2 +
  geom_segment(aes(
    x = first_week_start,
    y = 0.75,
    xend = last_week_start,
    yend = 0.75
  ),
  colour = color_compliance_year) +
  annotate(
    "text",
    x = first_week_start + 10,
    y = 0.8,
    label = "Compliance Year 2025",
    color = color_compliance_year
  ) +
  geom_segment(
    aes(
      x = first_week_start,
      y = 0,
      xend = first_week_start,
      yend = 1
    ),
    colour = color_compliance_year,
    linetype = "dashed"
  ) +
  geom_segment(
    aes(
      x = last_week_start,
      y = 0,
      xend = last_week_start,
      yend = 1
    ),
    colour = color_compliance_year,
    linetype = "dashed"
  )

gg_gap_25_24_26_tr1_tr2_compl +
  labs(
    title = "Dates used in the analysis.
       Compliance Year incl. the fringe week at the beginning, but excl. the fringe week at the end",
    x = "Dates",
    y = ""
  ) +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )


# 
