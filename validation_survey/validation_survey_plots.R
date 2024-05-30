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

