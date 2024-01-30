#### Current file:  ~/R_code_github/quantify_compliance2023/quantify_compliance_from_fhier_line_plots.R

percent_names <- paste0(seq(0, 100, by = 10), "%")

geom_text_size = text_sizes[["geom_text_size"]]
# geom_text_size <- 5
axis_title_size <- text_sizes[["axis_text_x_size"]]
axis_title_size <- 12
point_size <- 4

add_cnt_in_gr <-
  function(my_df, 
           group_by_col, 
           cnt_col_name = "total_vsl_m_by_year_perm") {
    my_df %>%
      # group by per month and permit
    group_by_at(group_by_col) %>%
    # cnt distinct vessels in each group
    mutate({{cnt_col_name}} :=
                    n_distinct(vessel_official_number)) %>%
    ungroup() %>%
    return()
}

# Add total vessels count per month and region ----
# (both compl. and not, a vsl can be in both)

group_by_col <- c("year_permit", "year_month")
compl_clean_sa_vs_gom_m_int_tot <-
  add_cnt_in_gr(compl_clean_sa_vs_gom_m_int, group_by_col)

# check
res1 <-
  compl_clean_sa_vs_gom_m_int |>
  select(vessel_official_number, year_permit, year_month) |>
  distinct() |>
  count(year_permit, year_month, name = "total_vsl_m_by_year_perm") |>
  arrange(total_vsl_m_by_year_perm)

tail(res1)
# 1 2023 sa_dual Dec 2023                       1969
# 2 2023 sa_dual Sep 2023                       1986
# 3 2023 sa_dual May 2023                       2020
# 4 2023 sa_dual Aug 2023                       2023
# 5 2023 sa_dual Jun 2023                       2026
# 6 2023 sa_dual Jul 2023                       2036

res2 <-
  compl_clean_sa_vs_gom_m_int_tot |>
  select(year_permit, year_month, total_vsl_m_by_year_perm) |>
  distinct() |>
  arrange(total_vsl_m_by_year_perm)

all.equal(res1, res2)
# T

compl_clean_sa_vs_gom_m_int_tot |>
  filter(year_permit == "2023 sa_dual") |> 
  select(year_month, total_vsl_m_by_year_perm) |>
  distinct() |> 
  arrange(year_month) |> 
  head()
# 1 Jan 2023          1967
# 2 Feb 2023          1958
# 3 Mar 2023          1954
# 4 Apr 2023          1968
# 5 May 2023          2020
# 6 Jun 2023          2026

# Fewer columns ----
compl_clean_sa_vs_gom_m_int_tot_short <-
  compl_clean_sa_vs_gom_m_int_tot |>
  select(
    vessel_official_number,
    compliant_,
    year_month,
    year_permit,
    total_vsl_m_by_year_perm
  ) |>
  distinct()

# Get compl, no compl, or both per month ----

get_compl_by <- function(my_df, group_by_for_compl, names_from_list) {
  my_df %>%
    group_by_at(group_by_for_compl) %>%
    # can unique, because we are looking at vessels, not weeks
    unique() %>%
    # more columns, a column per vessel
    tidyr::pivot_wider(
      names_from = all_of(names_from_list),
      # names_glue =
      #   "{names_from_list[[1]]}_{names_from_list[[1]]}_{.value}",
      values_from = compliant_,
      # make it "NO_YES" if both
      values_fn = ~ paste0(sort(.x), collapse = "_")
    ) %>%
    ungroup() %>%
    return()
}

# all columns except "vessel_official_number" and "compliant_"
group_by_for_compl_m <-
  vars(c(year_month, year_permit, total_vsl_m_by_year_perm))

names_from_list <- c("vessel_official_number")

compl_clean_sa_vs_gom_m_int_tot_short_wide <-
  get_compl_by(
    compl_clean_sa_vs_gom_m_int_tot_short,
    group_by_for_compl_m,
    names_from_list
  )

## Back to long format ----

not_vessel_id_col_names <-
  c("year_month",
    "year_permit",
    "total_vsl_m_by_year_perm")

compl_clean_sa_vs_gom_m_int_tot_short_wide_long <- 
  compl_clean_sa_vs_gom_m_int_tot_short_wide |> 
  tidyr::pivot_longer(
    # all other columns are vessel ids, use them as names
    cols = !any_of(not_vessel_id_col_names),
    values_to = "is_compl_or_both",
    names_to = "vessel_official_number"
  )

# Add count vessels per month, region and compl ----

compl_clean_sa_vs_gom_m_int_tot_short_wide_long__yes_no <-
  compl_clean_sa_vs_gom_m_int_tot_short_wide_long |>
  filter(stats::complete.cases(is_compl_or_both)) %>%
  mutate(compl_or_not =
           case_when(is_compl_or_both == "YES" ~
                       "compliant",
                     .default = "non_compliant")) |> 
  select(-is_compl_or_both)

# print_df_names(compl_clean_sa_vs_gom_m_int_tot_short_wide_long__yes_no)

group_by_col <- c("year_permit", "year_month", "compl_or_not")

compl_clean_sa_vs_gom_m_int_tot__compl_cnt <-
  add_cnt_in_gr(
    compl_clean_sa_vs_gom_m_int_tot_short_wide_long__yes_no,
    group_by_col,
    "cnt_vsl_m_compl"
  )

## test cnts compl per month ----
# tic("test tot cnts per month")
compl_clean_sa_vs_gom_m_int_tot__compl_cnt %>%
  select(-vessel_official_number) %>%
  unique() %>%
  filter(year_month == "Jan 2023") %>%
  glimpse()
# toc()
# $ year_month      <yearmon> Jan 2022, Jan 2022, Jan 2022, Jan 2022
# $ perm_exp_m      <chr> "active", "active", "active", "active"
# $ exp_m_tot_cnt   <int> 1635, 1635, 1192, 1192
# $ total_vsl_m_by_year_perm     <int> 1635, 1635, 1192, 1192
# $ compliant_      <chr> "YES", "NO", "YES", "NO"
# $ cnt_vsl_m_compl <int> 1057, 703, 1173, 45
# 1057 + 703 = 1760 is more than total. Some vessels can be both in a month, if compliance differs by week. For this analysis I used vessels having at least one week in the month  non-compliant.
# If we are going to use "yes only" than redo "yes, no, no_yes" division as for a year above.
# $ cnt_vsl_m_compl <int> 1052, 688, 1004, 42

# 2023:
# $ year_month      <yearmon> Jan 2023, Jan 2023, Jan 2023, Jan 2023, Jan 2023, …
# $ total_vsl_m_by_year_perm     <int> 1967, 1967, 1967, 675, 1967, 675
# $ compliant_      <chr> "YES", "NO", "YES", "YES", "NO", "NO"
# $ cnt_vsl_m_compl <int> 1693, 322, 1693, 675, 322, 1

# (w yes_no)
# $ year_month               <yearmon> Jan 2023, Jan 2023, Jan 2023, Jan 2023, Jan 2…
# $ year_permit              <chr> "2023 sa_dual", "2023 sa_dual", "2023 sa_dual", "…
# $ total_vsl_m_by_year_perm <int> 1967, 1967, 1967, 1967, 675, 675, 675
# $ is_compl_or_both         <chr> "YES", NA, "NO", "NO_YES", NA, "YES", "NO_YES"
# $ cnt_vsl_m_compl          <int> 1645, 1405, 274, 48, 2697, 674, 1
# > 1645+274+48
# [1] 1967
# correct sum 

# w compl_or_not
# $ year_month               <yearmon> Jan 2023, Jan 2023, Jan 2023, Jan 2023
# $ year_permit              <chr> "2023 sa_dual", "2023 sa_dual", "2023 gom_onl…
# $ total_vsl_m_by_year_perm <int> 1967, 1967, 675, 675
# $ compl_or_not             <chr> "compliant", "non_compliant", "compliant", "non_c…
# $ cnt_vsl_m_compl          <int> 1645, 322, 674, 1

# Month: percent compl vessels per per month ----

compl_clean_sa_vs_gom_m_int_tot__compl_cnt_short <- 
  compl_clean_sa_vs_gom_m_int_tot__compl_cnt |>
  select(-vessel_official_number) |> 
  distinct()

glimpse(compl_clean_sa_vs_gom_m_int_tot__compl_cnt_short)
# [1] 38  5

compl_clean_sa_vs_gom_m_int_tot__compl_cnt_short_perc <-
  compl_clean_sa_vs_gom_m_int_tot__compl_cnt_short |>
  mutate(cnt_m_compl_perc =
           cnt_vsl_m_compl * 100 / total_vsl_m_by_year_perm)

# glimpse(compl_clean_sa_vs_gom_m_int_tot__compl_cnt_short_perc)

# Plot non compliant perc by month ----

## split by year_permit into a list ----
compl_clean_sa_vs_gom_m_int_tot__compl_cnt_short_perc_l <-
  split(compl_clean_sa_vs_gom_m_int_tot__compl_cnt_short_perc,
        as.factor(compl_clean_sa_vs_gom_m_int_tot__compl_cnt_short_perc$year_permit))

## make % line plots by permit ----
line_df_23_gom_monthly_nc_percent_plot_color = plot_colors$non_compliant_by_month

# one_year_permit <- "2023 sa_dual" (for test)

line_monthly_nc_plot_l <-
  names(compl_clean_sa_vs_gom_m_int_tot__compl_cnt_short_perc_l) |>
  # [1] "2023 gom_only" "2023 sa_dual"
  purrr::map(
    function(one_year_permit) {
      one_df <-
        compl_clean_sa_vs_gom_m_int_tot__compl_cnt_short_perc_l[[one_year_permit]] |>
        filter(compl_or_not == "non_compliant") |>
        mutate(my_label = paste0(round(cnt_m_compl_perc, 0), "%")) |>
        mutate(tot_cnt_label =
                 str_glue("{cnt_vsl_m_compl}/\n{total_vsl_m_by_year_perm}"))
        # mutate(tot_per_m_xlabel =
        #          factor(total_vsl_m_by_year_perm)) |> 
        # mutate(year_m_factor =
        #          factor(year_month))
      
      # scaleFactor <-
      #   max(as.double(one_df$year_month)) / max(one_df$total_vsl_m_by_year_perm)

      one_df |>
      # (xf <- factor(x, levels = c("Male", "Man" , "Lady",   "Female"),
      #            labels = c("Male", "Male", "Female", "Female")))

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

        geom_text(aes(
          label = my_label,
          hjust =
            ifelse(cnt_m_compl_perc >= 27,
                   "outward", 0),
          vjust =
            ifelse(my_label == "22%",
                   -1, 1.5)
          
        ),
          check_overlap = TRUE,
          color = line_df_23_gom_monthly_nc_percent_plot_color,
          size = geom_text_size - 1
        ) +
          # scale_y_continuous("Precipitation", 
        # sec.axis = sec_axis(~ (. - a)/b, name = "Temperature")) +
  # scale_x_continuous("Month (2023)", breaks = 1:12,
  #                    sec.axis = dup_axis(~ one_df$total_vsl_m_by_year_perm, 
  #                                        name = "Temperature")
  #                    ) +

          # scale_x_continuous(labels = ~paste(., .+50, sep = "\n"),
          #            name = "Primary axis\nSecondary axis1")

        # scale_x_date(date_breaks = "1 month", 
        #              date_labels = 
        #                ~paste("%b", total_vsl_m_by_year_perm,
        #                       sep = "\n"),
                     # sec.axis =
                     #   dup_axis(name = "total vessel per month",
                     #            labels = 0:length(one_df$year_m_factor)2)
                     # labels =
                                #   c(one_df$tot_per_m_xlabel, "")
                                # )
      # ) +
      # scale_y_continuous(sec.axis = sec_axis( ~ . * scale, name = "Biomarker (IU/mL)")) +
        
        theme(
          legend.position = "none",
          axis.text.x =
            element_text(size = axis_title_size),
          axis.text.y =
            element_text(size = axis_title_size)
        ) +
        ylim(0, 55) +
        labs(x = "Months (2023)",
             y = "Proportion of Non-Compliant Vessels") +
        scale_x_date(date_breaks = "1 month", 
                     date_labels = "%b") +
        expand_limits(x = as.Date("12/31/23", "%m/%d/%y")) +
        annotate("text", 
                 x = as.Date(one_df$year_month),
                 y = 0,
                 label =
                   one_df$tot_cnt_label,
                 color = "blue")

        
        # scale_x_discrete(date_breaks = "1 month", date_labels = "%b") +
        
                           
        # 
        # scale_x_continuous(sec.axis = ~ total_vsl_m_by_year_perm)

    })

# ggplot(data=df,aes(x=Control, y=Stress))+geom_point()+scale_x_continuous(sec.axis = sec_axis(~ .+50,))


sa_dual_line_monthly_nc_plot <- line_monthly_nc_plot_l[[2]]

# save to files ----
file_full_name_m_perc_lines <- file.path(plot_file_path,
                            "m_line_perc_23_sa_dual_plot.png")

# see the function definition F2
save_plots_list_to_files(file_full_name_m_perc_lines,
                         sa_dual_line_monthly_nc_plot)

