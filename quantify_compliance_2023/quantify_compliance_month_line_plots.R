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

group_by_col <- c("year", "permit_sa_gom_dual", "year_month")
# print_df_names(count_weeks_per_vsl_permit_year_compl_p)

compl_clean_sa_vs_gom_m_int_tot <-
  add_cnt_in_gr(count_weeks_per_vsl_permit_year_compl_p, group_by_col)

# check
# rm(res1)
res1 <-
  compl_clean_sa_vs_gom_m_int__join_metrics |>
  select(vessel_official_number, 
         year, 
         permit_sa_gom_dual, 
         year_month) |>
  distinct() |>
  count(year, 
        permit_sa_gom_dual, 
        year_month, 
        name = "total_vsl_m_by_year_perm") |>
  arrange(year_month, permit_sa_gom_dual)

# tail(res1)
# 1 2023 sa_dual Dec 2023                       1969
# 2 2023 sa_dual Sep 2023                       1986
# 3 2023 sa_dual May 2023                       2020
# 4 2023 sa_dual Aug 2023                       2023
# 5 2023 sa_dual Jun 2023                       2026
# 6 2023 sa_dual Jul 2023                       2036

# rm(res2)
res2 <-
  compl_clean_sa_vs_gom_m_int_tot |>
  select(year,
         permit_sa_gom_dual,
         year_month,
         total_vsl_m_by_year_perm) |>
  distinct() |>
  arrange(year_month, permit_sa_gom_dual)

# tail(res2)

diffdf::diffdf(res1, res2)
# T

compl_clean_sa_vs_gom_m_int_tot |>
  filter(year == "2023", permit_sa_gom_dual == "sa_only") |> 
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

# 1 Jan 2023                       1753
# 2 Feb 2023                       1744
# 3 Mar 2023                       1740
# 4 Apr 2023                       1752
# 5 May 2023                       1791
# 6 Jun 2023                       1793


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

# Fewer columns ----
compl_clean_sa_vs_gom_m_int_tot_short <-
  compl_clean_sa_vs_gom_m_int_tot |>
  select(
    vessel_official_number,
    compliant_,
    year_month,
    year, 
    permit_sa_gom_dual,
    total_vsl_m_by_year_perm
  ) |>
  distinct()

# Get compl, no compl, or both per month ----

get_compl_by <- 
  function(my_df, group_by_for_compl, names_from_list) {
  my_df %>%
    group_by_at(group_by_for_compl) %>%
    # can unique, because we are looking at vessels, not weeks
    unique() %>%
    # more columns, a column per vessel
    tidyr::pivot_wider(
      names_from = all_of(names_from_list),
      values_from = compliant_,
      # make it "NO_YES" if both
      values_fn = ~ paste0(sort(.x), collapse = "_")
    ) %>%
    ungroup() %>%
    return()
}

# all columns except "vessel_official_number" and "compliant_"
group_by_for_compl_m <-
  vars(c(
    year_month,
    year,
    permit_sa_gom_dual,
    total_vsl_m_by_year_perm
  ))

names_from_list <- c("vessel_official_number")

compl_clean_sa_vs_gom_m_int_tot_short_wide <-
  get_compl_by(
    compl_clean_sa_vs_gom_m_int_tot_short,
    group_by_for_compl_m,
    names_from_list
  )

# View(compl_clean_sa_vs_gom_m_int_tot_short_wide)

## Back to long format ----

not_vessel_id_col_names <-
  c("year_month",
    "year",
    "permit_sa_gom_dual",
    "total_vsl_m_by_year_perm")

compl_clean_sa_vs_gom_m_int_tot_short_wide_long <- 
  compl_clean_sa_vs_gom_m_int_tot_short_wide |> 
  tidyr::pivot_longer(
    # all other columns are vessel ids, use them as names
    cols = !any_of(not_vessel_id_col_names),
    values_to = "is_compl_or_both",
    names_to = "vessel_official_number"
  )

# View(compl_clean_sa_vs_gom_m_int_tot_short_wide_long)
# compl_clean_sa_vs_gom_m_int_tot_short_wide_long$is_compl_or_both |> unique()
# [1] "YES"    "NO"     NA       "NO_YES"

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

group_by_col <-
  c("year", "permit_sa_gom_dual", "year_month", "compl_or_not")

compl_clean_sa_vs_gom_m_int_tot__compl_cnt <-
  add_cnt_in_gr(
    compl_clean_sa_vs_gom_m_int_tot_short_wide_long__yes_no,
    group_by_col,
    "cnt_vsl_m_compl"
  )

# View(compl_clean_sa_vs_gom_m_int_tot__compl_cnt)

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
# [1] 38  5
# 144   6 (sep dual)

compl_clean_sa_vs_gom_m_int_tot__compl_cnt_short_perc <-
  compl_clean_sa_vs_gom_m_int_tot__compl_cnt_short |>
  mutate(cnt_m_compl_perc =
           cnt_vsl_m_compl * 100 / total_vsl_m_by_year_perm)

# View(compl_clean_sa_vs_gom_m_int_tot__compl_cnt_short_perc)

# Plot non compliant perc by month ----

## split by year_permit into a list ----
## add a new column ----
compl_clean_sa_vs_gom_m_int_tot__compl_cnt_short_perc__nc_comb_col <-
  compl_clean_sa_vs_gom_m_int_tot__compl_cnt_short_perc |>
  filter(compl_or_not == "non_compliant") |> 
  rowwise() |>
  mutate(year_permit_sa_gom_dual = paste(year, permit_sa_gom_dual)) |>
  ungroup()

# View(compl_clean_sa_vs_gom_m_int_tot__compl_cnt_short_perc__nc_comb_col)

## split
compl_clean_sa_vs_gom_m_int_tot__compl_cnt_short_perc_l_nc <-
  split(
    compl_clean_sa_vs_gom_m_int_tot__compl_cnt_short_perc__nc_comb_col,
    as.factor(
      compl_clean_sa_vs_gom_m_int_tot__compl_cnt_short_perc__nc_comb_col$year_permit_sa_gom_dual
    )
  )

# View(compl_clean_sa_vs_gom_m_int_tot__compl_cnt_short_perc_l_nc[[1]])

## make % line plots by permit ----
line_df_23_gom_monthly_nc_percent_plot_color = plot_colors$non_compliant_by_month

# one_year_permit <- "2023 sa_dual" (for test)
# curr_permit_sa_gom_dual <- "2023 sa_only" #(for test)

line_monthly_nc_plot_l <-
  names(compl_clean_sa_vs_gom_m_int_tot__compl_cnt_short_perc_l) |>
#   [1] "2022 dual"     "2022 gom_only" "2022 sa_only"  "2023 dual"     "2023 gom_only"
# [6] "2023 sa_only" 
  purrr::map(
    function(curr_year_permit) {
      # browser()
      curr_year_permit_l <- str_split(curr_year_permit, " ")
      curr_year <- curr_year_permit_l[[1]][[1]]
      curr_permit_sa_gom_dual <- curr_year_permit_l[[1]][[2]]
      
      one_df <-
        compl_clean_sa_vs_gom_m_int_tot__compl_cnt_short_perc_l[[curr_year_permit]] |>
        filter(compl_or_not == "non_compliant") |>
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

        theme(
          legend.position = "none",
          axis.text.x =
            element_text(size = axis_title_size),
          axis.text.y =
            element_text(size = axis_title_size)
        ) +
        ylim(0, 55) +
        labs(x = str_glue("Months ({curr_year})"),
             y = str_glue("Proportion of Non-Compliant {curr_permit_sa_gom_dual} Vessels")) +
        scale_x_date(date_breaks = "1 month", 
                     date_labels = "%b") +
        expand_limits(x = as.Date("12/31/23", "%m/%d/%y")) +
        annotate("text", 
                 x = as.Date(one_df$year_month),
                 y = 0,
                 label =
                   one_df$tot_cnt_label,
                 color = "blue")
    })

# ggplot(data=df,aes(x=Control, y=Stress))+geom_point()+scale_x_continuous(sec.axis = sec_axis(~ .+50,))

line_monthly_nc_plot_l

sa_dual_line_monthly_nc_plot <- line_monthly_nc_plot_l[[2]]

# save to files ----
file_full_name_m_perc_lines <- file.path(plot_file_path,
                            "m_line_perc_23_sa_dual_plot.png")

# see the function definition F2
save_plots_list_to_files(file_full_name_m_perc_lines,
                         sa_dual_line_monthly_nc_plot)

# The same for SA and dual separately ----

## Add total vessels count per month and region ----
# (both compl. and not, a vsl can be in both)

group_by_col <- c("permit_sa_gom", "year_month")
compl_clean_sa_vs_gom_m_int_tot_sep <-
  add_cnt_in_gr(compl_clean_sa_vs_gom_m_int, group_by_col)

compl_clean_sa_vs_gom_m_int_tot_sep <- 
  rename(compl_clean_sa_vs_gom_m_int_tot_sep,
       "total_vsl_m_by_permit_sa_gom" = total_vsl_m_by_year_perm)

# glimpse(compl_clean_sa_vs_gom_m_int_tot_sep)
# check
res1 <-
  compl_clean_sa_vs_gom_m_int |>
  select(vessel_official_number, permit_sa_gom, year_month) |>
  distinct() |>
  count(permit_sa_gom, year_month, name = "total_vsl_m_by_permit_sa_gom") |>
  # arrange(year_month)
  arrange(total_vsl_m_by_permit_sa_gom, year_month)

tail(res1)
# 1 dual          Nov 2023                            288
# 2 gom_only      Nov 2023                            897
# 3 sa_only       Nov 2023                           1678
# 4 dual          Dec 2023                            290
# 5 gom_only      Dec 2023                            902
# 6 sa_only       Dec 2023                           1679

# 1 sa_only       Dec 2023                           1679
# 2 sa_only       Sep 2023                           1692
# 3 sa_only       May 2023                           1714
# 4 sa_only       Jun 2023                           1721
# 5 sa_only       Aug 2023                           1724
# 6 sa_only       Jul 2023                           1736

res2 <-
  compl_clean_sa_vs_gom_m_int_tot_sep |>
  select(permit_sa_gom, year_month, total_vsl_m_by_permit_sa_gom) |>
  distinct() |>
  arrange(total_vsl_m_by_permit_sa_gom, year_month)
  # arrange(year_month)

diffdf::diffdf(res1, res2)
# T

# print_df_names(compl_clean_sa_vs_gom_m_int_tot_sep)

compl_clean_sa_vs_gom_m_int_tot_sep |>
  filter(permit_sa_gom == "sa_only") |> 
  select(year_month, total_vsl_m_by_permit_sa_gom) |>
  distinct() |> 
  arrange(year_month) |> 
  head()
# 1 Jan 2023                           1661
# 2 Feb 2023                           1654
# 3 Mar 2023                           1652
# 4 Apr 2023                           1666
# 5 May 2023                           1714
# 6 Jun 2023                           1721

# Fewer columns ----
compl_clean_sa_vs_gom_m_int_tot_sep_short <-
  compl_clean_sa_vs_gom_m_int_tot_sep |>
  select(
    vessel_official_number,
    compliant_,
    year_month,
    permit_sa_gom,
    total_vsl_m_by_permit_sa_gom
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
  vars(c(year_month, permit_sa_gom, total_vsl_m_by_permit_sa_gom))

names_from_list <- c("vessel_official_number")

compl_clean_sa_vs_gom_m_int_tot_sep_short_wide <-
  get_compl_by(
    compl_clean_sa_vs_gom_m_int_tot_sep_short,
    group_by_for_compl_m,
    names_from_list
  )

## Back to long format ----

not_vessel_id_col_names <-
  c("year_month",
    "permit_sa_gom",
    "total_vsl_m_by_permit_sa_gom")

compl_clean_sa_vs_gom_m_int_tot_sep_short_wide_long <- 
  compl_clean_sa_vs_gom_m_int_tot_sep_short_wide |> 
  tidyr::pivot_longer(
    # all other columns are vessel ids, use them as names
    cols = !any_of(not_vessel_id_col_names),
    values_to = "is_compl_or_both",
    names_to = "vessel_official_number"
  )

# Add count vessels per month, region and compl ----

compl_clean_sa_vs_gom_m_int_tot_sep_short_wide_long__yes_no <-
  compl_clean_sa_vs_gom_m_int_tot_sep_short_wide_long |>
  filter(stats::complete.cases(is_compl_or_both)) %>%
  mutate(compl_or_not =
           case_when(is_compl_or_both == "YES" ~
                       "compliant",
                     .default = "non_compliant")) |> 
  select(-is_compl_or_both)

# compl_clean_sa_vs_gom_m_int_tot_sep_short_wide_long__yes_no |> 
#   filter(permit_sa_gom == "dual") |> 
#   View()

group_by_col <- c("permit_sa_gom", "year_month", "compl_or_not")

compl_clean_sa_vs_gom_m_int_tot_sep__compl_cnt <-
  add_cnt_in_gr(
    compl_clean_sa_vs_gom_m_int_tot_sep_short_wide_long__yes_no,
    group_by_col,
    "cnt_vsl_m_compl"
  )

## test cnts compl per month ----
# tic("test tot cnts per month")
compl_clean_sa_vs_gom_m_int_tot_sep__compl_cnt %>%
  select(-vessel_official_number) %>%
  unique() %>%
  filter(year_month == "Jan 2023") %>%
  glimpse()
  
# > sum(1358, 303, 287, 19)
# [1] 1967
# ok

# $ year_month                   <yearmon> Jan 2023, Jan 2023, Jan 2023, Jan 2023, J…
# $ permit_sa_gom                <chr> "sa_only", "sa_only", "dual", "dual", "gom_on…
# $ total_vsl_m_by_permit_sa_gom <int> 1661, 1661, 306, 306, 675, 675
# $ compl_or_not                 <chr> "compliant", "non_compliant", "compliant", "n…
# $ cnt_vsl_m_compl              <int> 1358, 303, 287, 19, 674, 1

# Month: percent compl vessels per per month ----

compl_clean_sa_vs_gom_m_int_tot_sep__compl_cnt_short <- 
  compl_clean_sa_vs_gom_m_int_tot_sep__compl_cnt |>
  select(-vessel_official_number) |> 
  distinct()

dim(compl_clean_sa_vs_gom_m_int_tot_sep__compl_cnt_short)
# [1] 62  5

compl_clean_sa_vs_gom_m_int_tot_sep__compl_cnt_short_perc <-
  compl_clean_sa_vs_gom_m_int_tot_sep__compl_cnt_short |>
  mutate(cnt_m_compl_perc =
           cnt_vsl_m_compl * 100 / total_vsl_m_by_permit_sa_gom)

# glimpse(compl_clean_sa_vs_gom_m_int_tot_sep__compl_cnt_short_perc)

# Plot non compliant perc by month ----

## split by permit_sa_gom into a list ----
compl_clean_sa_vs_gom_m_int_tot_sep__compl_cnt_short_perc_l <-
  split(compl_clean_sa_vs_gom_m_int_tot_sep__compl_cnt_short_perc,
        as.factor(compl_clean_sa_vs_gom_m_int_tot_sep__compl_cnt_short_perc$permit_sa_gom))

## make % line plots by permit ----
line_df_23_gom_monthly_nc_percent_plot_color = plot_colors$non_compliant_by_month

line_monthly_nc_plot_l <-
  names(compl_clean_sa_vs_gom_m_int_tot_sep__compl_cnt_short_perc_l) |>
# [1] "dual"     "gom_only" "sa_only" 
  purrr::map(
    function(one_permit_sa_gom) {
      one_df <-
        compl_clean_sa_vs_gom_m_int_tot_sep__compl_cnt_short_perc_l[[one_permit_sa_gom]] |>
        filter(compl_or_not == "non_compliant") |>
        mutate(my_label = paste0(round(cnt_m_compl_perc, 0), "%")) |>
        mutate(tot_cnt_label =
                 str_glue("{cnt_vsl_m_compl}/\n{total_vsl_m_by_permit_sa_gom}"))

      one_df |>
        ggplot(
          aes(
            x = as.Date(year_month),
            y = cnt_m_compl_perc,
            color = line_df_23_gom_monthly_nc_percent_plot_color
          )
        ) +
        geom_point(color =
                     line_df_23_gom_monthly_nc_percent_plot_color,
                   size = point_size) +
        geom_line(color =
                    line_df_23_gom_monthly_nc_percent_plot_color,
                  linewidth = 1) +
        theme_bw() +
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
        theme(
          legend.position = "none",
          axis.text.x =
            element_text(size = axis_title_size),
          axis.text.y =
            element_text(size = axis_title_size)
        ) +
        ylim(0, 55) +
        labs(title = str_glue("{one_permit_sa_gom} 2023"),
             x = "Months (2023)",
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
    })

line_monthly_nc_plot_l |> View()

sa_only_line_monthly_nc_plot <- line_monthly_nc_plot_l[[3]]

dual_line_monthly_nc_plot <- line_monthly_nc_plot_l[[1]]

# save to files ----
file_full_name_m_perc_lines_sa <- file.path(plot_file_path,
                            "m_line_perc_23_sa_only_plot.png")

file_full_name_m_perc_lines_dual <- file.path(plot_file_path,
                            "m_line_perc_23_dual_plot.png")

# see the function definition F2
save_plots_list_to_files(file_full_name_m_perc_lines_sa,
                         sa_only_line_monthly_nc_plot)

save_plots_list_to_files(file_full_name_m_perc_lines_dual,
                         dual_line_monthly_nc_plot)
