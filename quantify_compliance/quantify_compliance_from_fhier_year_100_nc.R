# run from quantify_compliance_start.R after quantify_compliance_from_fhier_year.R

# SA vessels that never reported anything ----
# Jessica wants to see 1 more figure for the SA, that is the proportion of SA vessels that never reported anything - whereas, your compliance for all of 2022 means of the 54% non-compliant, they may only be missing 1 week in the whole year. 
print_df_names(count_weeks_per_vsl_permit_year_compl_p)

count_weeks_per_vsl_permit_year_compl_p_sa_22 <-
  count_weeks_per_vsl_permit_year_compl_p |>
  filter(year_permit == "2022 sa_only")

count_weeks_per_vsl_permit_year_compl_p_sa_22_non_100 <-
  count_weeks_per_vsl_permit_year_compl_p_sa_22 |>
  select(vessel_official_number,
         compliant_,
         year_permit,
         percent_compl) |>
  dplyr::distinct() |>
  filter(compliant_ == "NO") |>
  filter(percent_compl == 100)

dim(count_weeks_per_vsl_permit_year_compl_p_sa_22_non_100)
# 487

# All vessels
length(unique(count_weeks_per_vsl_permit_year_compl_p$vessel_official_number))
# 3669

# All SA 2022 vessels
length(unique(count_weeks_per_vsl_permit_year_compl_p_sa_22$vessel_official_number))
# 2152
# in metrics
# Total Vessels With SA Only
# 2275
# 2275 - 2152 = 123

sa_22_non_c_vessels <-
  count_weeks_per_vsl_permit_year_compl_p_sa_22 |>
  filter(compliant_ == "NO") |>
  select(vessel_official_number) |>
  dplyr::distinct()

sa_22_vessels <-
  count_weeks_per_vsl_permit_year_compl_p_sa_22 |> 
  select(vessel_official_number) |>
  dplyr::distinct()

percent_of_never_compl_from_all_non_c <- 
  dim(count_weeks_per_vsl_permit_year_compl_p_sa_22_non_100)[[1]] * 100 / dim(sa_22_non_c_vessels)[[1]]
# [1] 41.87446 %

percent_of_never_compl_from_all_sa_2022 <- 
  dim(count_weeks_per_vsl_permit_year_compl_p_sa_22_non_100)[[1]] * 100 / dim(sa_22_vessels)[[1]]
# [1] 22.63011 %

# SA vessels 2022 vessels cnt / percent compl ----
# count_weeks_per_vsl_permit_year_compl_p |> 
#   View()

count_weeks_per_vsl_permit_year_compl_p_short <- 
  count_weeks_per_vsl_permit_year_compl_p |>
  select(
    vessel_official_number,
    compliant_,
    year_permit,
    weeks_per_vessel_per_compl,
    total_weeks_per_vessel,
    percent_compl
  ) |>
  dplyr::distinct()

# data_overview(count_weeks_per_vsl_permit_year_compl_p_short)
# vessel_official_number     3669

count_weeks_per_vsl_permit_year_compl_p_short_count <- 
  count_weeks_per_vsl_permit_year_compl_p_short |> 
  filter(compliant_ == "NO") |> 
  filter(year_permit == "2022 sa_only") |> 
  select(vessel_official_number, percent_compl) |> 
  dplyr::add_count(percent_compl, name = "vessels_cnt")

head(count_weeks_per_vsl_permit_year_compl_p_short_count, 2)

# View(count_weeks_per_vsl_permit_year_compl_p_sa_22_non_100)
# print_df_names(count_weeks_per_vsl_permit_year_compl_p_short_count)

## add columns ----
count_weeks_per_vsl_permit_year_compl_p_short_count_perc <-
  count_weeks_per_vsl_permit_year_compl_p_short_count |>
  mutate(total_vessels = n_distinct(vessel_official_number)) |>
  mutate(
    perc_nc_100_gr = base::findInterval(percent_compl, c(1, 100)),
    perc_nc_100_gr_name =
      dplyr::case_when(perc_nc_100_gr == 2 ~
                  "Never Reported",
                .default = "Reported At Least 1 Time")
  ) |>
  dplyr::group_by(perc_nc_100_gr) |>
  mutate(group_vsl_cnt = n_distinct(vessel_official_number)) |>
  select(-vessel_official_number) |>
  dplyr::distinct() |>
  mutate(
    perc_of_perc =
      dplyr::case_when(
        perc_nc_100_gr == 2 ~
          vessels_cnt * 100 / total_vessels,
        perc_nc_100_gr == 1 ~
          sum(vessels_cnt) * 100 / total_vessels
      )
  ) |>
  dplyr::ungroup()

# View(count_weeks_per_vsl_permit_year_compl_p_short_count_perc)
nc_sa_22_100_plot <-
  count_weeks_per_vsl_permit_year_compl_p_short_count_perc |>
  select(perc_nc_100_gr,
         perc_nc_100_gr_name,
         group_vsl_cnt,
         perc_of_perc) |>
  dplyr::distinct() |>
  ggplot(aes(x = perc_nc_100_gr_name,
             y = round(perc_of_perc, 0),
             fill = as.factor(perc_nc_100_gr))) +
  geom_col() +
  scale_fill_manual(
    # use custom colors
    values =
      c(
        # "1" = "pink",
        # "2" = "red"
        "1" = "skyblue1",
        "2" = "#0570B0"
      ),
    # Legend title
    name = "Non compliant",
    labels = unique(count_weeks_per_vsl_permit_year_compl_p_short_count_perc$perc_nc_100_gr_name)
  ) +
  theme(legend.position = "none") +
  theme(
    axis.title.y = element_text(size = text_sizes[["axis_text_y_size"]]),
    axis.text.x =
      element_text(size = text_sizes[["axis_text_x_size"]]),
    axis.text.y =
      element_text(size = text_sizes[["axis_text_y_size"]])
  ) +
  # no x and y titles for individual plots
  labs(title = 
         stringr::str_glue("Non compliant SA vsls in 2022 (total non compliant = {count_weeks_per_vsl_permit_year_compl_p_short_count_perc$total_vessels})"),
       y = "Non compliant in 2022 (%)",
       x = "") +
  ylim(0, 100)

# print_df_names(count_weeks_per_vsl_permit_year_compl_p_short_count_perc)
# Add percent numbers on the bars
nc_sa_22_100_plot <-
  nc_sa_22_100_plot +
  geom_text(aes(label =
                  paste0(round(perc_of_perc, 0), "%")),
            # in the middle of the bar
            position =
              position_stack(vjust = 0.5),
            size = text_sizes[["geom_text_size"]])

nc_sa_22_100_plot
    #   
    # } else {
    #   one_plot <-
    #     one_plot + annotate("text",
    #                         x = 1:2,
    #                         y = 20,
    #                         label = label_percent)
    # }

ggsave(
  file = "sa_22_nc_100.png",
  plot = nc_sa_22_100_plot,
  device = "png",
  path = file.path(my_paths$outputs,
                   r"(quantify_compliance\vsl_cnt_by_perc_non_compl)"),
  width = 20,
  height = 10,
  units = "cm"
)

## 100% non compliant, less than 100 and compliant ----
# View(count_weeks_per_vsl_permit_year_compl_p_short_count_perc)
total_vessels_c_n_nc <- 
  count_weeks_per_vsl_permit_year_compl_p_short |> 
  select(vessel_official_number) |> 
  dplyr::distinct() |> 
  dim()
# vessel_official_number     3669

nc_sa_22_100_plot <-
  count_weeks_per_vsl_permit_year_compl_p_short_count_perc |>
  select(perc_nc_100_gr,
         perc_nc_100_gr_name,
         group_vsl_cnt,
         perc_of_perc) |>
  dplyr::distinct() |>
  ggplot(aes(x = perc_nc_100_gr_name,
             y = round(perc_of_perc, 0),
             fill = as.factor(perc_nc_100_gr))) +
  geom_col() +
  scale_fill_manual(
    # use custom colors
    values =
      c(
        # "1" = "pink",
        # "2" = "red"
        "1" = "skyblue1",
        "2" = "#0570B0"
      ),
    # Legend title
    name = "Non compliant",
    labels = unique(count_weeks_per_vsl_permit_year_compl_p_short_count_perc$perc_nc_100_gr_name)
  ) +
  theme(legend.position = "none") +
  theme(
    axis.title.y = element_text(size = text_sizes[["axis_text_y_size"]]),
    axis.text.x =
      element_text(size = text_sizes[["axis_text_x_size"]]),
    axis.text.y =
      element_text(size = text_sizes[["axis_text_y_size"]])
  ) +
  # no x and y titles for individual plots
  labs(title = 
         stringr::str_glue("Non compliant SA vsls in 2022 (total non compliant = {count_weeks_per_vsl_permit_year_compl_p_short_count_perc$total_vessels})"),
       y = "Non compliant in 2022 (%)",
       x = "") +
  ylim(0, 100)


# plot(count_weeks_per_vsl_permit_year_compl_p_short_count)
## Less than 100% ----
count_weeks_per_vsl_permit_year_compl_p_short_count_less_100 <-
  count_weeks_per_vsl_permit_year_compl_p_short_count |>
  filter(vessels_cnt < 100)


perc_non_compl_plot_less_100 <-
  ggplot(count_weeks_per_vsl_permit_year_compl_p_short_count_less_100,
         aes(x = vessels_cnt,
             y = percent_compl)) +
  geom_line(color = "deepskyblue") +
  labs(title = "Non compliant SA vessels (2022) number by percent of non compliant where % non compliant < 100",
       x = "Vessel count",
       y = "% nc vsls") +
  # y axes 0 to 100
  ylim(0, 100) +
  # size of an individual plot's title
  theme(plot.title =
          element_text(size = 12))

perc_non_compl_plot_less_100 <- 
  perc_non_compl_plot_less_100 +
  geom_point() +
  # text on dots
  # on top
  geom_text(aes(label = round(percent_compl, 1)),
            vjust = -0.3)

max_percent_compl_less_100 <-
  max(count_weeks_per_vsl_permit_year_compl_p_short_count_less_100$percent_compl)
# [1] 98.07692

min_percent_compl_less_100 <-
  min(count_weeks_per_vsl_permit_year_compl_p_short_count_less_100$percent_compl)
# [1] 1.923077

perc_non_compl_plot_less_100_hline <- 
  perc_non_compl_plot_less_100 +
  geom_hline(yintercept = min_percent_compl_less_100,
             color = "red") +
  geom_hline(yintercept = max_percent_compl_less_100,
             color = "red")

# perc_non_compl_plot_less_100 + geom_smooth(method = "lm", se = FALSE)

perc_non_compl_plot_less_100_ann <-
  perc_non_compl_plot_less_100_hline +
  annotate(
    "text",
    label = paste0(round(min_percent_compl_less_100, 1), "%"),
    x = 53,
    y = min_percent_compl_less_100 + 3,
    size = 4,
    colour = "red"
  ) +
  annotate(
    "text",
    label = paste0(round(max_percent_compl_less_100, 1), "%"),
    x = 53,
    y = 100,
    size = 4,
    colour = "red"
  )


perc_non_compl_plot_less_100_ann

# split by group ----
count_weeks_per_vsl_permit_year_compl_p_short_count_less_100_gr <-
  count_weeks_per_vsl_permit_year_compl_p_short_count_less_100 |>
  mutate(vessels_cnt_tot = sum(vessels_cnt)) |> 
  mutate(vessel_cnt_group = base::findInterval(vessels_cnt, c(0, 6))) |>
  dplyr::add_count(vessel_cnt_group, wt = vessels_cnt, name = "vessel_cnt_group_num") |>
  mutate(vessel_cnt_group_name =
           dplyr::case_when(
             vessel_cnt_group == 1 ~
               paste0("<= 5 vessels (",
                      vessel_cnt_group_num,
                      " v)"),
             .default = paste0("> 5 vessels (",
                               vessel_cnt_group_num,
                               " v)")
           )) |>
  mutate(percent_group = base::findInterval(percent_compl, c(0, 50, 75))) |>
  dplyr::add_count(percent_group, wt = vessels_cnt, name = "percent_group_num") |>
  mutate(
    percent_group_name =
      dplyr::case_when(
        percent_group == 1 ~ str_glue("1--50% non compliant ({percent_group_num} v.)"),
        percent_group == 2 ~ str_glue("50--75% non compliant ({percent_group_num} v.)"),
        percent_group == 3 ~ str_glue("75--98% non compliant({percent_group_num} v.)")
      )
  )

# View(count_weeks_per_vsl_permit_year_compl_p_short_count_less_100_gr)
count_weeks_per_vsl_permit_year_compl_p_short_count_less_100_gr |> 
  dplyr::count(vessel_cnt_group_name, wt = vessels_cnt)
# 1 <= 5 vessels (240 v)    388
# 2 > 5 vessels (21 v)      288
# 388 + 288 = 676

# w/o weight
# 1 <= 5 vessels (240 v)    240
# 2 > 5 vessels (21 v)       21

count_weeks_per_vsl_permit_year_compl_p_short_count_less_100_gr |> 
  dplyr::count(percent_group_name, wt = vessels_cnt)
# 1 0--50% non compliant    539
# 2 50--75% non compliant    80
# 3 75--98% non compliant    57
# 539 + 80 + 57 = 676

# w/o weight
# 1 0--50% non compliant    176
# 2 50--75% non compliant    44
# 3 75--98% non compliant    41
# 176 + 44 + 41 = 261

# View(count_weeks_per_vsl_permit_year_compl_p_short_count)

count_weeks_per_vsl_permit_year_compl_p_short_count_less_100_gr |> 
  ggplot(aes(x = vessels_cnt,
         y = percent_compl)) +
  geom_line(aes(colour = factor(vessel_cnt_group))) +
  geom_point(color = "darkblue") +
  geom_text(aes(label = round(percent_compl, 1)),
            vjust = 1.3,
            color = "blue") +
  labs(title = "Non compliant SA vessels (2022) number by percent of non compliant where % non compliant < 100",
       x = "Vessel count",
       y = "% nc vsls") +
  # y axes 0 to 100
  ylim(0, 100)
  
  
ggplot(
  count_weeks_per_vsl_permit_year_compl_p_short_count_less_100,
  aes(x = vessels_cnt)
) +
  geom_histogram(binwidth = 2)


# split by group all ----
count_weeks_per_vsl_permit_year_compl_p_short_count_gr <-
  count_weeks_per_vsl_permit_year_compl_p_short_count |>
  mutate(vessels_cnt_tot = n_distinct(vessel_official_number)) |>
  select(-vessel_official_number) |> 
  dplyr::distinct() |> 
  mutate(vessel_cnt_group = base::findInterval(vessels_cnt, c(0, 6, 450))) |> 
  dplyr::add_count(vessel_cnt_group, 
            wt = vessels_cnt, 
            name = "vessel_cnt_group_num") |>
  mutate(vessel_cnt_group_name =
           dplyr::case_when(
             vessel_cnt_group == 1 ~
               str_glue("{vessel_cnt_group}: 1--5 vessels ({vessel_cnt_group_num} v)"),
             vessel_cnt_group == 2 ~
               str_glue("{vessel_cnt_group}: 6--450 vessels ({vessel_cnt_group_num} v)"),
             vessel_cnt_group == 3 ~
               str_glue("{vessel_cnt_group}: 451--500 vessels ({vessel_cnt_group_num} v)"),
           )) |>
  mutate(percent_group = base::findInterval(percent_compl, c(0, 50, 99))) |>
  dplyr::add_count(percent_group, wt = vessels_cnt, name = "percent_group_num") |>
  mutate(
    percent_group_name =
      dplyr::case_when(
        percent_group == 1 ~ str_glue("1--50% non compliant  ({percent_group_num} v.)"),
        percent_group == 2 ~ str_glue("50--98% non compliant ({percent_group_num} v.)"),
                percent_group == 3 ~ str_glue("99--100% non compliant ({percent_group_num} v.)")
      )
  )

# View(count_weeks_per_vsl_permit_year_compl_p_short_count_gr)
count_weeks_per_vsl_permit_year_compl_p_short_count_less_100_gr |> 
  dplyr::count(vessel_cnt_group_name, wt = vessels_cnt)
# 1 <= 5 vessels (240 v)    388
# 2 > 5 vessels (21 v)      288
# 388 + 288 = 676

# w/o weight
# 1 <= 5 vessels (240 v)    240
# 2 > 5 vessels (21 v)       21

count_weeks_per_vsl_permit_year_compl_p_short_count_less_100_gr |> 
  dplyr::count(percent_group_name, wt = vessels_cnt)
# 1 0--50% non compliant    539
# 2 50--75% non compliant    80
# 3 75--98% non compliant    57
# 539 + 80 + 57 = 676

# w/o weight
# 1 0--50% non compliant    176
# 2 50--75% non compliant    44
# 3 75--98% non compliant    41
# 176 + 44 + 41 = 261

# View(count_weeks_per_vsl_permit_year_compl_p_short_count)

count_weeks_per_vsl_permit_year_compl_p_short_count_less_100_gr |> 
  ggplot(aes(x = vessels_cnt,
         y = percent_compl)) +
  geom_line(aes(colour = factor(vessel_cnt_group))) +
  geom_point(color = "darkblue") +
  geom_text(aes(label = round(percent_compl, 1)),
            vjust = 1.3,
            color = "blue") +
  labs(title = "Non compliant SA vessels (2022) number by percent of non compliant where % non compliant < 100",
       x = "Vessel count",
       y = "% nc vsls") +
  # y axes 0 to 100
  ylim(0, 100)
  
  
ggplot(
  count_weeks_per_vsl_permit_year_compl_p_short_count_less_100,
  aes(x = vessels_cnt)
) +
  geom_histogram(binwidth = 2)


# facets ----
# p <- ggplot(mtcars, aes(mpg, wt)) +
#   geom_point() +
#   facet_wrap(~ cyl)
# 
# mean_wt <- data.frame(cyl = c(4, 6, 8), wt = c(2.28, 3.11, 4.00))
# p + geom_hline(aes(yintercept = wt), mean_wt)
print_df_names(count_weeks_per_vsl_permit_year_compl_p_short_count_less_100_gr)

labs <- 
  labs(title = "Non compliant SA vessels (2022) number by percent of non compliant where % non compliant < 100",
       x = "Vessel count",
       y = "% nc vsls")

p <-
  count_weeks_per_vsl_permit_year_compl_p_short_count_less_100_gr |>
  ggplot(aes(x = vessels_cnt,
             y = percent_compl)) +
  geom_point(color = "darkgreen") +
  # facet_wrap(vars(vessel_cnt_group_name), scales = "free_x")
  facet_wrap(vars(vessel_cnt_group_num, percent_group_name), labeller = "label_both") +
  labs


p <-
  count_weeks_per_vsl_permit_year_compl_p_short_count_less_100_gr |>
  ggplot(aes(x = vessels_cnt,
             y = percent_compl)) +
  geom_point(color = "darkgreen") +
  facet_wrap(vars(percent_group_name), scales = "free_x") +
  labs
  # facet_wrap(vars(percent_group_name))

# facet plots for all non compliant SA 2022 ----
labs_all <- 
  labs(title = "Number of SA permitted vessels grouped by percent of non compliant time in 2022",
       x = "Vessel count",
       y = "Percent of non conmpliant in 2022")

# View(count_weeks_per_vsl_permit_year_compl_p_short_count_gr)
# All percents ----
count_weeks_per_vsl_permit_year_compl_p_short_count_gr_for_plot <- 
  count_weeks_per_vsl_permit_year_compl_p_short_count_gr |>
  dplyr::group_by(vessel_cnt_group) |> 
  mutate(max_in_vsl_group = max(vessels_cnt),
         min_in_vsl_group = min(vessels_cnt)) |> 
  dplyr::ungroup()

View(count_weeks_per_vsl_permit_year_compl_p_short_count_gr_for_plot)

## All by vessel count ---

count_weeks_per_vsl_permit_year_compl_p_short_count_gr |>
  ggplot(aes(x = vessels_cnt,
             y = percent_compl,
             cex = vessel_cnt_group_num)) +
  geom_point(color = "darkred") +
  facet_wrap(vars(vessel_cnt_group_name), scales = "free_x") +
  labs_all +
  scale_x_continuous(breaks = seq(
    min(
      count_weeks_per_vsl_permit_year_compl_p_short_count_gr_for_plot$min_in_vsl_group
    ),
    max(
      count_weeks_per_vsl_permit_year_compl_p_short_count_gr_for_plot$max_in_vsl_group
    ),
    by = floor(log10(max(
      count_weeks_per_vsl_permit_year_compl_p_short_count_gr_for_plot$max_in_vsl_group)))
  ))
  

## All By percent ----

# View(count_weeks_per_vsl_permit_year_compl_p_short_count_gr)
plot_all_by_percent <- 
  count_weeks_per_vsl_permit_year_compl_p_short_count_gr |>
  ggplot(aes(x = vessels_cnt,
             y = percent_compl,
             cex = vessel_cnt_group_num)) +
  geom_point(color = "darkred") +
  # ggplot2::facet_grid(
  #   cols = vars(percent_group_name),
  #   scales = "free_x",
  #   space = "free_x",
  #   margins = "vessels_cnt"
  # ) +
facet_wrap(vars(percent_group_name),
           scales = "free_x",
           nrow = 1) +
  scale_x_continuous(breaks = seq(
    min(
      count_weeks_per_vsl_permit_year_compl_p_short_count_gr_for_plot$min_in_vsl_group
    ),
    max(
      count_weeks_per_vsl_permit_year_compl_p_short_count_gr_for_plot$max_in_vsl_group
    ),
    by = floor(log10(max(
      count_weeks_per_vsl_permit_year_compl_p_short_count_gr_for_plot$max_in_vsl_group)))
  )) +
  labs_all +
  labs(cex = "Vsl num")

plot_all_by_percent

ggsave(
  file = "sa_22_nc_perc_vsl_cnt_by_percent.png",
  plot = plot_all_by_percent,
  device = "png",
  path = file.path(my_paths$outputs,
                   r"(quantify_compliance\vsl_cnt_by_perc_non_compl)"),
  width = 40,
  height = 20,
  units = "cm"
)

# 100% non compliant out of total ----
count_weeks_per_vsl_permit_year_compl_p_short_count_tot <- 
  count_weeks_per_vsl_permit_year_compl_p_short |> 
  filter(year_permit == "2022 sa_only") |> 
  select(vessel_official_number, compliant_, percent_compl) |> 
  dplyr::add_count(compliant_, percent_compl, name = "vessels_cnt")

head(count_weeks_per_vsl_permit_year_compl_p_short_count_tot, 2)
#   vessel_official_number compliant_ percent_compl vessels_cnt
#   <chr>                  <chr>              <dbl>       <int>
# 1 VI5498TB               YES                  100         990
# 2 VA9236AV               NO                   100         487

## add columns ----
never_reported_filter <-
  rlang::quo(perc_nc_100_gr == 2 &
               tolower(compliant_) == "no")

count_weeks_per_vsl_permit_year_compl_p_short_count_tot_perc <-
  count_weeks_per_vsl_permit_year_compl_p_short_count_tot |>
  mutate(total_vessels = n_distinct(vessel_official_number)) |> 
  # mutate(percent_compl_compl = ) |> 
  mutate(
    perc_nc_100_gr = base::findInterval(percent_compl, c(1, 100))) |> 
  # dplyr::group_by(perc_nc_100_gr, compliant_) |> str()
  mutate(perc_nc_100_gr_name =
      dplyr::case_when(!!never_reported_filter ~
                  "Never Reported",
                .default = "Reported At Least 1 Time")
  ) |> 
  mutate(group_100_vs_rest =
      dplyr::case_when(!!never_reported_filter ~
                  1,
                .default = 2)
  ) |> 
  dplyr::group_by(perc_nc_100_gr_name) |>
  mutate(group_vsl_cnt = n_distinct(vessel_official_number)) |>
  select(-vessel_official_number) |>
  dplyr::distinct() |>
  mutate(
    perc_of_perc =
          group_vsl_cnt * 100 / total_vessels
  ) |>
  dplyr::ungroup()

glimpse(count_weeks_per_vsl_permit_year_compl_p_short_count_tot_perc)
nc_sa_22_tot_100_plot <-
  count_weeks_per_vsl_permit_year_compl_p_short_count_tot_perc |>
  select(group_100_vs_rest,
         perc_nc_100_gr_name,
         group_vsl_cnt,
         perc_of_perc) |>
  dplyr::distinct() |>
  ggplot(aes(x = perc_nc_100_gr_name,
             y = round(perc_of_perc, 0),
             fill = as.factor(group_100_vs_rest))) +
  geom_col() +
  scale_fill_manual(
    # use custom colors
    values =
      c(
        # "1" = "pink",
        # "2" = "red"
        "2" = "skyblue1",
        "1" = "#0570B0"
      ),
    # Legend title
    name = "Non compliant",
    labels = unique(count_weeks_per_vsl_permit_year_compl_p_short_count_tot_perc$perc_nc_100_gr_name)
  ) +
  theme(legend.position = "none") +
  theme(
    axis.title.y = element_text(size = text_sizes[["axis_text_y_size"]]),
    axis.text.x =
      element_text(size = text_sizes[["axis_text_x_size"]]),
    axis.text.y =
      element_text(size = text_sizes[["axis_text_y_size"]])
  ) +
  # no x and y titles for individual plots
  labs(title = 
         stringr::str_glue("Never reported SA vsls in 2022 out of all compliant and non compliant (total vsls = {count_weeks_per_vsl_permit_year_compl_p_short_count_tot_perc$total_vessels})"),
       y = "",
       # y = "% of All Vessels",
       x = "") +
  ylim(0, 100)

# print_df_names(count_weeks_per_vsl_permit_year_compl_p_short_count_tot_perc)
# Add percent numbers on the bars
nc_sa_22_tot_100_plot <-
  nc_sa_22_tot_100_plot +
  geom_text(aes(label =
                  paste0(round(perc_of_perc, 0), "%")),
            # in the middle of the bar
            position =
              position_stack(vjust = 0.5),
            size = text_sizes[["geom_text_size"]])

nc_sa_22_tot_100_plot

ggsave(
  file = "sa_22_tot_100nc_plot.png",
  plot = nc_sa_22_tot_100_plot,
  device = "png",
  path = file.path(my_paths$outputs,
                   r"(quantify_compliance\vsl_cnt_by_perc_non_compl)"),
  width = 20,
  height = 10,
  units = "cm"
)

