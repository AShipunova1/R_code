## did they interview before or after logbook submission ----

# Quantify when surveyors are intercepting vessels at the dock, in relation to when the trip is transmitted. The specific question is, did they interview before or after logbook submission?  Using the matched surveys to logbooks and transmission date/time vs survey data/time fields, could you please quantify how many surveys occur before the logbook is transmitted vs how many surveys occur after the logbook is transmitted? You can do this as a % of the total (matched) surveys (e.g. 40% of surveys occurred before logbook was submitted; 60% after)

lgb_join_i1__int_lgb__has_lgb_short <-
  lgb_join_i1__int_lgb__short |>
  filter(int_lgb == "has_lgb") |> 
  select(-int_lgb)

survey_data_time <-
  survey_data_l_2022_i1_w_dates_clean_vsl_no_na_vsl_num |>
  select(id_code,
         interview_date,
         interview_date_time,
         survey_vessel_id) |> 
  mutate(id_code = as.character(id_code))

## shorten
# Note. TRANSMISSION_DATE is missing in most entries and often is before the trip date, and TRIP_DE seems more accurate, will use it instead.

logbooks_transmission_time_short <-
  db_logbooks_2022_clean_vesl_clean |>
  select(TRIP_ID,
         VESSEL_OFFICIAL_NBR,
         TRIP_START_DATE,
         TRIP_END_DATE,
         TRIP_DE
         ) |>
  distinct() |> 
  remove_empty_cols()

dim(logbooks_transmission_time_short)
# [1] 94870     5

# View(logbooks_transmission_time_short)

# check the trip year
logbooks_transmission_time_short |> 
filter(!lubridate::year(TRIP_START_DATE) == "2022") |> 
  filter(!lubridate::year(TRIP_END_DATE) == "2022")
# 0
# OK, at least one date in 2022

# check dates
logbooks_transmission_time_short |> 
    filter(lubridate::date(TRIP_DE) < lubridate::date(TRIP_START_DATE)) |> 
  nrow()
# 7

logbooks_transmission_time_short |> 
    filter(is.na(TRIP_DE)) |> 
  nrow()
# 0

### combine data for transmission date ----
lgb_int__join_lgb_de <- 
  lgb_join_i1__int_lgb__has_lgb_short |> 
  left_join(logbooks_transmission_time_short,
            join_by(TRIP_ID, VESSEL_OFFICIAL_NBR))

dim(lgb_join_i1__int_lgb__has_lgb_short)
# [1] 1353    5

dim(lgb_int__join_lgb_de)
# [1] 1353    8

# Add survey time
lgb_int__join_lgb_de__surv_time <- 
  lgb_int__join_lgb_de |> 
  left_join(survey_data_time,
            join_by(id_code, survey_vessel_id))

dim(lgb_int__join_lgb_de__surv_time)
# [1] 1353   10

# Michelle:
# Yes, I’d like to see the difference in mins in both directions. If the logbook was submitted before the survey you could list those in negative mins, and if it was after then in positive mins. If it was seconds after then just round even 1 sec up to the min. 
# 
# Then we can do some summary stats on that: min, max, mean, median

# Find the time difference
lgb_int__join_lgb_de__surv_time__diff <-
  lgb_int__join_lgb_de__surv_time |>
  mutate(trip_de__interview_diff =
           difftime(TRIP_DE, interview_date_time, units = "mins")) |>
  mutate(trip_de__interview_diff_num =
           as.numeric(trip_de__interview_diff, units = "mins")) |>
  mutate(trip_de__interview_diff_dur =
           duration(trip_de__interview_diff_num, "minutes"))

# View(lgb_int__join_lgb_de__surv_time__diff)

# stats
summary(lgb_int__join_lgb_de__surv_time__diff$trip_de__interview_diff_num)
# Min.:-447.5
# 1st Qu.:49.5
# Median:167.4
# Mean:9156.8
# 3rd Qu.:446.3
# Max.:396382.8
# NA's   :159

lgb_int__join_lgb_de__surv_time__diff |> 
  filter(trip_de__interview_diff_num > 9156) |> glimpse()

summary(lgb_int__join_lgb_de__surv_time__diff$trip_de__interview_diff_dur)
# Min.                          1st Qu. 
# "-26852s (~-7.46 hours)"      "2972.25s (~49.54 minutes)" 
# Median                             Mean 
# "10043.5s (~2.79 hours)" "549406.476549414s (~6.36 days)" 
# 3rd Qu.                             Max. 
# "26780.5s (~7.44 hours)"       "23782965s (~39.32 weeks)" 
# NA's 
# "159" 

summary(lgb_int__join_lgb_de__surv_time__diff$trip_de__interview_diff_num)

q_limits <-
  quantile(lgb_int__join_lgb_de__surv_time__diff$trip_de__interview_diff_num,
           na.rm = TRUE)


lgb_int__join_lgb_de__surv_time__diff_groups <-
  lgb_int__join_lgb_de__surv_time__diff |>
  select(TRIP_ID, VESSEL_OFFICIAL_NBR,
         trip_de__interview_diff_num) |>
  mutate(diff_groups =
           cut(trip_de__interview_diff_num, q_limits))
  
lgb_int__join_lgb_de__surv_time__diff_groups |>
  select(diff_groups, TRIP_ID) |> 
  # group_by(diff_groups) |>
  count(diff_groups) |>
  # ungroup() |>
  glimpse()

lgb_int__join_lgb_de__surv_time__diff_groups |>
  select(VESSEL_OFFICIAL_NBR, diff_groups) |> 
  # group_by(diff_groups) |>
  count(diff_groups) |>
  # ungroup() |>
  glimpse()

lgb_int__join_lgb_de__surv_time__diff_groups |>
  count(VESSEL_OFFICIAL_NBR, diff_groups) |> 
  arrange(desc(n)) |> 
  glimpse()

lgb_int__join_lgb_de__surv_time__diff_groups |>
  count(TRIP_ID, diff_groups) |> 
  arrange(desc(n)) |> 
  glimpse()

lgb_int__join_lgb_de__surv_time__diff_groups |>
  count(diff_groups) |> 
  arrange(diff_groups)

# View(lgb_int__join_lgb_de__surv_time__diff_groups)

lgb_int__join_lgb_de__surv_time__diff_groups$diff_groups |> 
  unique()

# split another way

rr <-
  lgb_int__join_lgb_de__surv_time__diff |>
  mutate(diff_groups =
           cut(
             trip_de__interview_diff_num,
             breaks = c(q_limits[["0%"]], 
                        -60, 
                        60, 
                        q_limits[["50%"]],
                        q_limits[["75%"]],
                        q_limits[["100%"]])
               ,
           include.lowest = TRUE
           ))

# View(rr)
rr |> 
    count(diff_groups)

threshhold_dur <- lubridate::duration(2, "days")

plot_lt_3_d_diff <-
  lgb_int__join_lgb_de__surv_time__diff |>
  filter(trip_de__interview_diff_dur <
           threshhold_dur) |>
  ggplot(aes(x = trip_de__interview_diff_num))

plot_lt_3_d_diff__hist <-
  plot_lt_3_d_diff +
  geom_histogram(bins = 40,
                 color = "black",
                 fill = "blue") +
  annotate(
    geom = "rect",
    xmin = q_limits[["25%"]],
    xmax = q_limits[["75%"]],
    ymin = 0,
    ymax = Inf,
    fill = "palegreen",
    colour = "black",
    alpha = 0.5
  ) +
  geom_vline(aes(xintercept = median(trip_de__interview_diff_num)), 
             color = "purple",
             linewidth = 1.5
             ) +
  geom_vline(aes(xintercept = q_limits[["25%"]]), linetype = "dashed") +
  geom_vline(aes(xintercept = q_limits[["75%"]]), linetype = "dashed") +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 12)) +
  scale_y_continuous(breaks = scales::breaks_pretty(n = 7)) +
  xlab("Difference in minutes")

title <- stringr::str_glue('The difference between the interview time and logbook submission time.\nThe green rectangle is the area between 25 and 75 percentiles,\ni.e. most frequent differences (between {round(q_limits[["25%"]])} and {round(q_limits[["75%"]])} mins).\nThe purple line is the median ({round(q_limits[["50%"]])} min).\nTo fit data in one plot only the difference < {threshhold_dur} is shown.')

plot_lt_3_d_diff__hist + ggtitle(title)
          
# days = 86400 seconds
# 86400/60 * 3
# 4320 min == 3 days
  
# plot all diffs

plot_all_diffs <- ggplot(lgb_int__join_lgb_de__surv_time__diff, 
                aes(x = trip_de__interview_diff_num))

plot_all_diffs + 
  geom_histogram(bins = 20, color = "black", fill = "green") +
  labs(title = "TRIP_DE minus interview_date_time") + 
  # ylab("SOC (g C/m2/yr)") + 
  xlab("Difference in minutes")
