# run from quantify_compliance_start.R after quantify_compliance_from_fhier_year.R

# SA vessels that never reported anything ----
# Jessica wants to see 1 more figure for the SA, that is the proportion of SA vessels that never reported anything - whereas, your compliance for all of 2023 means of the 54% non-compliant, they may only be missing 1 week in the whole year. 

# Calculate the percentage of never compliant entries from all entries in 2023

# all SA vessels (compl and not compl alike) ----
count_weeks_per_vsl_permit_year_compl_p_sa_23 <-
  count_weeks_per_vsl_permit_year_compl_p |>
  dplyr::filter(year_permit == "2023 sa_dual")

# count_weeks_per_vsl_permit_year_compl_p$permit_sa_gom |>
#   unique()
# [1] "sa_only"  "dual"     "gom_only"

# Use if separate SA and dual
# count_weeks_per_vsl_permit_year_compl_p_sa_23_perm <-
#   count_weeks_per_vsl_permit_year_compl_p |>
#   dplyr::filter(!permit_sa_gom == "gom_only")

total_sa_dual_vessels <- 
  n_distinct(count_weeks_per_vsl_permit_year_compl_p_sa_23$vessel_official_number)
# 2421

# 100% non compliant out of total ----

## add columns ----
never_reported_filter <-
  rlang::quo(perc_nc_100_gr == 2 &
               tolower(compliant_) == "no")

count_weeks_per_vsl_permit_year_compl_p_sa_23__tot_perc <-
  count_weeks_per_vsl_permit_year_compl_p_sa_23 |>
  mutate(perc_nc_100_gr = base::findInterval(percent_compl, c(1, 100))) |>
  mutate(
    perc_nc_100_gr_name =
      case_when(!!never_reported_filter ~
                  "Never Reported",
                .default = "Reported At Least 1 Time")
  ) |>
  mutate(group_100_vs_rest =
           case_when(!!never_reported_filter ~
                       1,
                     .default = 2)) |>
  group_by(perc_nc_100_gr_name) |>
  mutate(group_vsl_cnt = n_distinct(vessel_official_number)) |>
  dplyr::mutate(perc_of_perc =
                  group_vsl_cnt * 100 / total_vsl_y_by_year_perm) |>
  dplyr::ungroup()

nc_sa_23_tot_100_plot <-
  count_weeks_per_vsl_permit_year_compl_p_sa_23__tot_perc |>
  select(group_100_vs_rest,
         perc_nc_100_gr_name,
         group_vsl_cnt,
         perc_of_perc) |>
  distinct() |>
  ggplot(aes(x = perc_nc_100_gr_name,
             y = round(perc_of_perc, 0),
             fill = as.factor(group_100_vs_rest))) +
  geom_col() +
  scale_fill_manual(
    # use custom colors
    values =
      c(
        "2" = plot_colors[["compliant"]],
        "1" = plot_colors[["non_compliant"]]
      ),
    labels = unique(count_weeks_per_vsl_permit_year_compl_p_sa_23__tot_perc$perc_nc_100_gr_name)
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
         stringr::str_glue("Never reported SA vsls in 2023 out of all compliant and non compliant (total vsls = {count_weeks_per_vsl_permit_year_compl_p_sa_23__tot_perc$total_vsl_y_by_year_perm})"),
       y = "",
       # y = "% of All Vessels",
       x = "") +
  ylim(0, 100)

# print_df_names(count_weeks_per_vsl_permit_year_compl_p_sa_23__tot_perc)
# Add percent numbers on the bars
nc_sa_23_tot_100_plot <-
  nc_sa_23_tot_100_plot +
  geom_text(aes(label =
                  paste0(round(perc_of_perc, 0), "%")),
            # in the middle of the bar
            position =
              position_stack(vjust = 0.5),
            size = text_sizes[["geom_text_size"]])

nc_sa_23_tot_100_plot

ggsave(
  file = "sa_23_tot_100nc_plot.png",
  plot = nc_sa_23_tot_100_plot,
  device = "png",
  path = file.path(my_paths$outputs,
                   project_name,
                   "vsl_cnt_by_perc_non_compl"),
  width = 20,
  height = 10,
  units = "cm"
)

