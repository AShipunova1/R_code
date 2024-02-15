# run from quantify_compliance_start.R after quantify_compliance_from_fhier_year.R

# SA vessels that never reported anything ----
# Jessica wants to see 1 more figure for the SA, that is the proportion of SA vessels that never reported anything - whereas, your compliance for all of 2023 means of the 54% non-compliant, they may only be missing 1 week in the whole year. 

# Calculate the percentage of never compliant entries from all entries in each year
# all SA vessels (compl and not compl alike) ----

# glimpse(count_weeks_per_vsl_permit_year_compl_p)

## fewer fields ----
count_weeks_per_vsl_permit_year_compl_p_short <- 
  count_weeks_per_vsl_permit_year_compl_p |>
  select(-c(week_start, 
            week_num,
            year_month,
            permit_sa_gom_dual)) |>
  distinct()
  
count_weeks_per_vsl_permit_year_compl_p_sa <-
  count_weeks_per_vsl_permit_year_compl_p_short |>
  filter(str_detect(permit_sa_gom_dual_both, "sa"))

# View(count_weeks_per_vsl_permit_year_compl_p_sa)
# check
# > count_weeks_per_vsl_permit_year_compl_p_sa |> 
# +     filter(compliant_ == "NO") |> 
# +     distinct() |> 
# +     filter(year == 2023) |> 
# +     mutate(nn = n_distinct(vessel_official_number)) |> 
# +     select(nn) |> 
# +     distinct()
# # A tibble: 1 × 1
#      nn
#   <int>
# 1  1549
# > count_weeks_per_vsl_permit_year_compl_p_sa |> 
# +     filter(compliant_ == "NO") |> 
# +     distinct() |> 
# +     filter(year == 2023) |> 
# +     nrow()
# [1] 1549

count_weeks_per_vsl_permit_year_compl_p_sa %>%
  group_by(vessel_official_number) %>%
  filter(n() >
           1) %>%
  summarize(n = n())
  
 # 5 1020822                    4
#  8 1022458                    3
# 10 1023960                    2
   # arrange(desc(n))
     # 4

count_weeks_per_vsl_permit_year_compl_p_sa |> 
  filter(vessel_official_number == "1020822") |> 
  glimpse()
# $ year_permit_sa_gom_dual    <chr> "2022 sa_only", "2022 sa_only", "2023 sa_dual",…
# $ percent_compl              <dbl> 63.461538, 36.538462, 90.384615, 9.615385

dim(count_weeks_per_vsl_permit_year_compl_p)
# [1] 298147     10
dim(count_weeks_per_vsl_permit_year_compl_p_sa)
# [1] 6628    8
# [1] 48436    11
# [1] 6525    9

count_weeks_per_vsl_permit_year_compl_p_sa |> 
  select(year, permit_sa_gom_dual_both, year_permit_sa_gom_dual) |> 
  distinct()
# 1 2022  sa_only            2022 sa_only           
# 2 2023  sa_only            2023 sa_dual           
# 3 2023  dual               2023 sa_dual           

#   year  permit_sa_gom_dual_both year_permit_sa_gom_dual
#   <chr> <chr>                   <chr>                  
# 1 2022  sa_only                 2022 sa_only           
# 2 2023  sa_dual                 2023 sa_dual           

total_sa_dual_vessels <-
  count_weeks_per_vsl_permit_year_compl_p_sa |> 
  group_by(year) |>
  mutate(cnts = n_distinct(vessel_official_number)) |>
  ungroup() |> 
  select(year, cnts) |> 
  distinct()
# 1 2022   2231
# 2 2023   2436

# 100% non compliant out of total ----

## add columns ----
never_reported_filter <-
  rlang::quo(perc_nc_100_gr == 2 &
               tolower(compliant_) == "no")

dim(count_weeks_per_vsl_permit_year_compl_p_sa)
# [1] 6628    8
# [1] 48436    11

count_weeks_per_vsl_permit_year_compl_p_sa |>
  filter(percent_compl == "100",
         compliant_ == "NO",
         year == my_year2) |>
  select(vessel_official_number) |>
  # distinct() |>
  nrow()
# 372 ok

# non_compliant_only ----
count_weeks_per_vsl_permit_year_compl_p_sa_nc <-
  count_weeks_per_vsl_permit_year_compl_p_sa

count_weeks_per_vsl_permit_year_compl_p_sa__tot_perc <-
  count_weeks_per_vsl_permit_year_compl_p_sa_nc |>
  group_by(year_permit_sa_gom_dual) |>
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
  ungroup() |> 
  group_by(perc_nc_100_gr_name, year_permit_sa_gom_dual) |>
  mutate(group_vsl_cnt = n_distinct(vessel_official_number)) |>
  # filter(permit_sa_gom_dual == "dual", compliant_ == "YES",
  #        year == "2023") |>
  # filter(vessel_official_number == "TX6550AU") |>
  # glimpse()
  #
  dplyr::mutate(perc_of_perc =
                  group_vsl_cnt * 100 / total_vsl_y_by_year_perm) |>
  dplyr::ungroup()

count_weeks_per_vsl_permit_year_compl_p_sa__tot_perc |>
  filter(vessel_official_number == "1020822") |>
  distinct() |> 
  select(-contains("month")) |> 
  distinct() |> 
  glimpse()
# compl, sa_only
# $ year                       <chr> "2022", "2022", "2023", "2023"
# $ perc_of_perc               <dbl> 79.33662, 79.33662, 83.46348, 83.46348

# curr_year <- my_year2
# curr_permit_sa_gom_dual <- "sa_only"

# Plots ----
make_one_plot_100c <-
  function(curr_df,
           curr_year_permit) {
    # browser()
    
    curr_year_permit_list <-
      str_split_1(curr_year_permit, " ")
    curr_year <- curr_year_permit_list[[1]]
    curr_permit_sa_gom_dual <- curr_year_permit_list[[2]]
    
    # cat(curr_year_permit)
    
    curr_title_permit <-
      title_permits %>%
      filter(permit_sa_gom_dual == curr_permit_sa_gom_dual)
    
    curr_total_vsl_y_by_year_perm <-
      curr_df$total_vsl_y_by_year_perm |>
      unique()
    
    curr_title <-
      stringr::str_glue(
        "Total {curr_title_permit$long_title} Permitted Vessels That Never Reported")
    
    one_plot <-
      curr_df |>
      ggplot(aes(
        x = perc_nc_100_gr_name,
        y = round(perc_of_perc, 0),
        fill = as.factor(group_100_vs_rest)
      )) +
      geom_col() +
      scale_fill_manual(
        # use custom colors
        values =
          c("2" = plot_colors[["compliant"]],
            "1" = plot_colors[["non_compliant"]]),
        labels = unique(
          count_weeks_per_vsl_permit_year_compl_p_sa__tot_perc$perc_nc_100_gr_name
        )
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
      labs(title = curr_title,
           y = "",
           # y = "% of All Vessels",
           x = "") +
      ylim(0, 100)
    
    # Add percent numbers on the bars
    curr_tot_100_plot <-
      one_plot +
      geom_text(aes(label =
                      paste0(round(perc_of_perc, 0), "%")),
                # in the middle of the bar
                position =
                  position_stack(vjust = 0.5),
                size = text_sizes[["geom_text_size"]])
    
    return(curr_tot_100_plot)
  }

# View(count_weeks_per_vsl_permit_year_compl_p_sa__tot_perc)
sa_dual_tot_100_plots <-
  count_weeks_per_vsl_permit_year_compl_p_sa__tot_perc$year_permit_sa_gom_dual |>
  unique() |>
  map(\(curr_year_permit) {
    # browser()
    curr_df <-
      count_weeks_per_vsl_permit_year_compl_p_sa__tot_perc |>
      filter(year_permit_sa_gom_dual == curr_year_permit) |>
      filter(compliant_ == "NO") |> 
      select(
        # vessel_official_number,
        year,
        total_vsl_y_by_year_perm,
        group_100_vs_rest,
        perc_nc_100_gr_name,
        group_vsl_cnt,
        # permit_sa_gom_dual,
        year_permit_sa_gom_dual,
        # compliant_,
        perc_of_perc
      ) |>
      distinct()
    
    # cat(c("curr_year_permit",
    #       curr_year_permit,
    #       "curr_year",
    #       curr_year,
    #       "curr_permit_sa_gom_dual",
    #       curr_permit_sa_gom_dual), "\n")
    
    make_one_plot_100c(curr_df,
                       curr_year_permit)
  })

sa_dual_tot_100_plots

# names(sa_dual_tot_100_plots)
# rm(sa_dual_tot_100_plots_flat_list)
sa_dual_tot_100_plots_flat_list <-
  make_flat_plot_list(sa_dual_tot_100_plots)

# View(sa_dual_tot_100_plots_flat_list)

flat_plot_list_names <-
  sa_dual_tot_100_plots_flat_list |>
  map(\(x) {
    x$data$year_permit_sa_gom_dual |>
      str_replace_all(" ", "_") |>
      tolower() |>
      str_replace("_permitted_vessels", "") |>
      str_replace_all("[^a-z0-9_]", "_") |> 
      unique()
  })
  
names(sa_dual_tot_100_plots_flat_list) <-
    flat_plot_list_names

# View(sa_dual_tot_100_plots_flat_list)

# drop dual 2022
my_grobs_list <- sa_dual_tot_100_plots_flat_list
  # list(sa_dual_tot_100_plots[[1]][[1]],
  #                     sa_dual_tot_100_plots[[2]][[1]],
  #                     sa_dual_tot_100_plots[[2]][[2]])

# combine plots ----
grid.arrange(grobs = my_grobs_list)

flat_plot_list_names |>
  map(\(plot_name) {
    # browser()

    file_full_name_c_nc <-
      file.path(plot_file_path,
                str_glue("100nc_plot_{plot_name}.png"))
    
    save_plots_list_to_files(file_full_name_c_nc,
                             my_grobs_list[[plot_name]],
                             my_width = 20,
                             my_height = 10)
  })

# [1] "C:/Users/anna.shipunova/Documents/R_files_local/my_outputs/quantify_compliance_2023/2024-02-09/100nc_plot_2023_sa_dual.png"

