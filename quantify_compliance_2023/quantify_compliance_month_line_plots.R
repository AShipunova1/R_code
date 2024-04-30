#### Current file:  ~/R_code_github/quantify_compliance2023/quantify_compliance_from_fhier_line_plots.R

percent_names <- paste0(seq(0, 100, by = 10), "%")

geom_text_size = text_sizes[["geom_text_size"]]
# geom_text_size <- 5
axis_title_size <- text_sizes[["axis_text_x_size"]]
axis_title_size <- 12
point_size <- 4

# Add total vessels count per month and region ----
# (both compl. and not, a vsl can be in both)

group_by_col <- c("year", "permit_sa_gom_dual_both", "year_month")
# print_df_names(count_weeks_per_vsl_permit_year_compl_p)

compl_clean_sa_vs_gom_m_int_tot <-
  add_cnt_in_gr(count_weeks_per_vsl_permit_year_compl_p, group_by_col)

# check
# rm(res1)
res1 <-
  compl_clean_sa_vs_gom_m_int__join_metrics__both_p__comb |>
  select(vessel_official_number, 
         year, 
         permit_sa_gom_dual_both, 
         year_month) |>
  distinct() |>
  count(year, 
        permit_sa_gom_dual_both, 
        year_month, 
        name = "total_vsl_m_by_year_perm") |>
  arrange(year_month, permit_sa_gom_dual_both)

# tail(res1)
# 1 2023 dual Nov 2023 270
# 2 2023 gom_only Nov 2023 949
# 3 2023 sa_only Nov 2023 1729
# 4 2023 dual Dec 2023 272
# 5 2023 gom_only Dec 2023 947
# 6 2023 sa_only Dec 2023 1726

res2 <-
  compl_clean_sa_vs_gom_m_int_tot |>
  select(year,
         permit_sa_gom_dual_both,
         year_month,
         total_vsl_m_by_year_perm) |>
  distinct() |>
  arrange(year_month, permit_sa_gom_dual_both)

diffdf::diffdf(res1, res2)
# T

compl_clean_sa_vs_gom_m_int_tot |>
  filter(year == "2023",
         permit_sa_gom_dual %in%
           c("sa_only", "dual")) |>
  select(year_month, compliant_, total_vsl_m_by_year_perm) |>
  distinct() |>
  arrange(year_month) |>
  head()

# sa_dual
# 1 Jan 2023                       1982
# 2 Feb 2023                       1972
# 3 Mar 2023                       1967
# 4 Apr 2023                       1983
# 5 May 2023                       2032
# 6 Jun 2023                       2035

# sa_only
# 1 Jan 2023   YES                            1753
# 2 Jan 2023   NO                             1753
# 3 Jan 2023   YES                             285
# 4 Jan 2023   NO                              285
# 5 Feb 2023   YES                            1744
# 6 Feb 2023   NO                             1744

compl_clean_sa_vs_gom_m_int_tot |>
  filter(year == "2023" &
           !permit_sa_gom_dual == "gom_only") |> 
  select(year_month, permit_sa_gom_dual, total_vsl_m_by_year_perm) |>
  distinct() |> 
  group_by(year_month) |> 
  mutate(sa_dual_cnt = sum(total_vsl_m_by_year_perm)) |> 
  ungroup() |> 
  arrange(year_month) |> 
  head()
# 1 Jan 2023   sa_only                                1753        2038
# 2 Jan 2023   dual                                    285        2038

# Fewer columns ----
compl_clean_sa_vs_gom_m_int_tot_short <-
  compl_clean_sa_vs_gom_m_int_tot |>
  select(
    vessel_official_number,
    compliant_,
    year_month,
    year, 
    permit_sa_gom_dual_both,
    year_permit_sa_gom_dual,
    total_vsl_m_by_year_perm
  ) |>
  distinct()

# Get compl, no compl, or both per month ----

## all columns except "vessel_official_number" and "compliant_" ----
group_by_for_compl_m <-
  vars(-c(vessel_official_number,
          compliant_))

names_from_list <- c("vessel_official_number")

compl_clean_sa_vs_gom_m_int_tot_short_wide <-
  get_compl_by(
    compl_clean_sa_vs_gom_m_int_tot_short,
    group_by_for_compl_m,
    names_from_list
  )

dim(compl_clean_sa_vs_gom_m_int_tot_short_wide)
# [1]   72 4021

## Back to long format ----
not_vessel_id_col_names <-
  c("year_month",
    "year",
    "permit_sa_gom_dual_both",
    "year_permit_sa_gom_dual",
    "total_vsl_m_by_year_perm")

# Explanations:
# Pivot the compl_clean_sa_vs_gom_m_int_tot_short_wide dataframe to a longer format.
# 1. Use tidyr::pivot_longer to transform the dataframe by specifying:
#    - cols: All columns except those listed in not_vessel_id_col_names are considered values.
#    - values_to: The new column "is_compl_or_both" will store the values from the pivoted columns.
#    - names_to: The new column "vessel_official_number" will store the names of the pivoted columns.
compl_clean_sa_vs_gom_m_int_tot_short_wide_long <- 
  compl_clean_sa_vs_gom_m_int_tot_short_wide |> 
  tidyr::pivot_longer(
    # all other columns are vessel ids, use them as names
    cols = !any_of(not_vessel_id_col_names),
    values_to = "is_compl_or_both",
    names_to = "vessel_official_number"
  )

dim(compl_clean_sa_vs_gom_m_int_tot_short_wide_long)
# [1] 289152      7

compl_clean_sa_vs_gom_m_int_tot_short_wide_long$is_compl_or_both |> unique()
# [1] "YES"    "NO"     NA       "NO_YES"

# Add count vessels per month, region and compl ----

# Explanations:
# Filter out rows with missing values in the 'is_compl_or_both' column.
# Mutate a new column 'compl_or_not' based on the values in 'is_compl_or_both'.
# 1. Use filter(stats::complete.cases(is_compl_or_both)) to remove rows with missing values in 'is_compl_or_both'.
# 2. Use mutate to create 'compl_or_not' based on conditions:
#    - If 'is_compl_or_both' is "YES", set 'compl_or_not' to "compliant".
#    - For other cases, set 'compl_or_not' to "non_compliant".
# 3. Use select(-is_compl_or_both) to drop the original 'is_compl_or_both' column.

compl_clean_sa_vs_gom_m_int_tot_short_wide_long__yes_no <-
  compl_clean_sa_vs_gom_m_int_tot_short_wide_long |>
  filter(stats::complete.cases(is_compl_or_both)) %>%
  mutate(compl_or_not =
           case_when(is_compl_or_both == "YES" ~
                       "compliant",
                     .default = "non_compliant")) |> 
  select(-is_compl_or_both)

# print_df_names(compl_clean_sa_vs_gom_m_int_tot_short_wide_long__yes_no)
# [1] "year_month, year, permit_sa_gom_dual_both, year_permit_sa_gom_dual, total_vsl_m_by_year_perm, vessel_official_number, compl_or_not"

group_by_col <-
  c(
    "year",
    "permit_sa_gom_dual_both",
    "year_permit_sa_gom_dual",
    "year_month",
    # "total_vsl_m_by_year_perm",
    # "vessel_official_number",
    "compl_or_not"
  )

compl_clean_sa_vs_gom_m_int_tot__compl_cnt <-
  add_cnt_in_gr(
    compl_clean_sa_vs_gom_m_int_tot_short_wide_long__yes_no,
    group_by_col,
    "cnt_vsl_m_compl"
  )

dim(compl_clean_sa_vs_gom_m_int_tot__compl_cnt)
# [1] 70046     8

## test cnts compl per month ----
compl_clean_sa_vs_gom_m_int_tot__compl_cnt %>%
  select(-vessel_official_number) %>%
  distinct() %>%
  filter(year_month == "Jan 2023") %>%
  glimpse()

# w compl_or_not & sa_dual
# $ year_month               <yearmon> Jan 2023, Jan 2023, Jan 2023, Jan 2023
# $ year_permit              <chr> "2023 sa_dual", "2023 sa_dual", "2023 gom_onl…
# $ total_vsl_m_by_year_perm <int> 1967, 1967, 675, 675
# $ compl_or_not             <chr> "compliant", "non_compliant", "compliant", "non_c…
# $ cnt_vsl_m_compl          <int> 1645, 322, 674, 1

# with sa_only
# today()
# [1] "2024-02-08"
# $ permit_sa_gom_dual       <chr> "sa_only", "sa_only", "dual", "dual", "gom_only", "go…
# $ total_vsl_m_by_year_perm <int> 1753, 1753, 285, 285, 850, 850
# $ compl_or_not             <chr> "compliant", "non_compliant", "compliant", "non_compl…
# $ cnt_vsl_m_compl          <int> 1434, 319, 273, 12, 847, 3

# Month: percent compl vessels per per month ----

compl_clean_sa_vs_gom_m_int_tot__compl_cnt_short <- 
  compl_clean_sa_vs_gom_m_int_tot__compl_cnt |>
  select(-vessel_official_number) |> 
  distinct()

dim(compl_clean_sa_vs_gom_m_int_tot__compl_cnt_short)
# 144   7

compl_clean_sa_vs_gom_m_int_tot__compl_cnt_short_perc <-
  compl_clean_sa_vs_gom_m_int_tot__compl_cnt_short |>
  mutate(cnt_m_compl_perc =
           cnt_vsl_m_compl * 100 / total_vsl_m_by_year_perm)

dim(compl_clean_sa_vs_gom_m_int_tot__compl_cnt_short_perc)
# [1] 144   8

# Plot non compliant perc by month ----

# Non compliant only ----
compl_clean_sa_vs_gom_m_int_tot__compl_cnt_short_perc__nc_comb_col <-
  compl_clean_sa_vs_gom_m_int_tot__compl_cnt_short_perc |>
  filter(compl_or_not == "non_compliant")

dim(compl_clean_sa_vs_gom_m_int_tot__compl_cnt_short_perc__nc_comb_col)
# [1] 72  8

## split ----
# Explanations:
# Split the dataframe 'compl_clean_sa_vs_gom_m_int_tot__compl_cnt_short_perc__nc_comb_col'
# into a list of dataframes based on the levels of 'year_permit_sa_gom_dual'.
# 1. Use split to create a list of dataframes.
# 2. The splitting is done based on the levels of 'year_permit_sa_gom_dual'.
#    - Each dataframe in the list corresponds to a unique value of 'year_permit_sa_gom_dual'.

compl_clean_sa_vs_gom_m_int_tot__compl_cnt_short_perc_l_nc <-
  split(
    compl_clean_sa_vs_gom_m_int_tot__compl_cnt_short_perc__nc_comb_col,
    as.factor(
      compl_clean_sa_vs_gom_m_int_tot__compl_cnt_short_perc__nc_comb_col$year_permit_sa_gom_dual
    )
  )

dim(compl_clean_sa_vs_gom_m_int_tot__compl_cnt_short_perc_l_nc[[1]])
# [1] 12  8

## make % line plots by permit ----
line_df_23_gom_monthly_nc_percent_plot_color =
  plot_colors$non_compliant_by_month

# Explanations:
# Generate a list of line plots for monthly non-compliance percentages for different
# year_permit_sa_gom_dual combinations.
# 1. Iterate over each 'curr_year_permit' in the list of names.
# 2. Split 'curr_year_permit' into 'curr_year' and 'curr_permit_sa_gom_dual'.
# 3. Create a dataframe 'one_df' from the corresponding dataframe in the list.
#    - Add columns 'my_label' and 'tot_cnt_label' for labeling points and annotating text.
# 4. Use ggplot to create a line plot for each dataframe in the list.
# 5. Customize the plot appearance, labels, and limits.
# 6. Scale the x-axis as a date with monthly breaks and labels.
# 7. Expand the x-axis limits to the end of the year.
# 8. Return a list of line plots.

line_monthly_nc_plot_l <-
  names(compl_clean_sa_vs_gom_m_int_tot__compl_cnt_short_perc_l_nc) |>
  #   [1] "2022 dual"     "2022 gom_only" "2022 sa_only"  "2023 dual"     "2023 gom_only"
  # [6] "2023 sa_only"
  purrr::map(function(curr_year_permit) {
    # browser()
    curr_year_permit_l <- str_split(curr_year_permit, " ")
    curr_year <- curr_year_permit_l[[1]][[1]]
    curr_permit_sa_gom_dual <- curr_year_permit_l[[1]][[2]]
    curr_year_end <- str_glue("{curr_year}-12-31")
    
    one_df <-
      compl_clean_sa_vs_gom_m_int_tot__compl_cnt_short_perc_l_nc[[curr_year_permit]] |>
      mutate(my_label = paste0(round(cnt_m_compl_perc, 0), "%")) |>
      mutate(tot_cnt_label =
               str_glue("{cnt_vsl_m_compl}/\n{total_vsl_m_by_year_perm}"))
    
    one_df |>
      ggplot(
        aes(
          x = as.Date(year_month),
          y = cnt_m_compl_perc,
          color = line_df_23_gom_monthly_nc_percent_plot_color
        )
      ) +
      geom_point(color = line_df_23_gom_monthly_nc_percent_plot_color,
                 size = point_size) +
      geom_line(color = line_df_23_gom_monthly_nc_percent_plot_color,
                linewidth = 1) +
      theme_bw() +
      # text under the dot
      # geom_text(aes(hjust =
      #                 ifelse(cnt_m_compl_perc >= 27,
      #                        "outward", 0),
      
      geom_text(
        aes(
          label = my_label,
          hjust =
            ifelse(cnt_m_compl_perc >= 27 &
                     !(my_label == "39%"),
                   "outward",
                   0),
          vjust =
            ifelse(my_label == "22%",
                   -1, 1.5)
          
        ),
        check_overlap = TRUE,
        color = line_df_23_gom_monthly_nc_percent_plot_color,
        size = geom_text_size - 1
      ) +
      theme(
        legend.position = "none",
        axis.title.x = 
          element_text(size = text_sizes[["axis_text_x_size"]]),
        axis.title.y = 
          element_text(size = text_sizes[["axis_text_y_size"]]),
        axis.text.x =
          element_text(size = text_sizes[["axis_text_x_size"]]),
        axis.text.y =
          element_text(size = text_sizes[["axis_text_y_size"]])
      ) +
      ylim(0, 55) +
      labs(
        x = str_glue("Months ({curr_year})"),
        # y = str_glue("Proportion of Non-Compliant {curr_permit_sa_gom_dual} Vessels")) +
        y = str_glue("Proportion of Non-Compliant Vessels")
      ) +
      
      scale_x_date(date_breaks = "1 month",
                   date_labels = "%b") +
      expand_limits(x = as.Date(curr_year_end, "%Y-%m-%d"))
    # annotate("text",
    #          x = as.Date(one_df$year_month),
    #          y = 0,
    #          label =
    #            one_df$tot_cnt_label,
    #          color = "blue")
  })

names(line_monthly_nc_plot_l) <-
  names(compl_clean_sa_vs_gom_m_int_tot__compl_cnt_short_perc_l_nc)

line_monthly_nc_plot_l["2023 sa_only"]

# save to files ----
plots_to_save <- c(line_monthly_nc_plot_l["2022 sa_only"],
                   line_monthly_nc_plot_l["2023 sa_only"],
                   line_monthly_nc_plot_l["2023 dual"])

# Explanations:
# Save each plot in the 'plots_to_save' list to a PNG file.
# 1. Iterate over each 'one_plot' in the list.
# 2. Extract 'permit_sa_gom_dual_both' and 'year' from the plot data.
# 3. Generate a lowercase 'file_name_part' using these extracted values.
# 4. Construct the full file path with the 'file_name_part'.
# 5. Save the plot to the specified file path with custom width and height.
plots_to_save |>
  map(\(one_plot) {
    # browser()
    curr_permit_sa_gom_dual <-
      unique(one_plot$data$permit_sa_gom_dual_both)
    curr_year <- unique(one_plot$data$year)
    
    file_name_part <-
      str_glue("{curr_permit_sa_gom_dual}_{curr_year}") |>
      tolower()
    
    file_full_name_c_nc <-
      file.path(plot_file_path,
                str_glue("m_line_perc_{file_name_part}_plot.png"))
    
    save_plots_list_to_files(file_full_name_c_nc,
                             one_plot,
                             my_width = 20,
                             my_height = 10)
    
  })

