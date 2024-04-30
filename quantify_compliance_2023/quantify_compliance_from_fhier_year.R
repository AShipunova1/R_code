# by Year: ----
## year add total counts ----
# (both compl. and not, a vsl can be in both)

add_total_cnt_in_gr <- 
  function(my_df, 
           group_by_col, 
           new_col_name = "total_vsl_y_by_year_perm") {
    my_df %>%
    # group by per year and permit
    dplyr::group_by_at(group_by_col) %>%
    # cnt distinct vessels in each group
    dplyr::mutate({{new_col_name}} :=
                    dplyr::n_distinct(vessel_official_number)) %>%
    dplyr::ungroup() %>%
    return()
}

compl_clean_sa_vs_gom_m_int_tot <-
  add_total_cnt_in_gr(compl_clean_sa_vs_gom_m_int__join_metrics, 
                      c("permit_sa_gom_dual", "year"))

# print_df_names(compl_clean_sa_vs_gom_m_int_tot)

# print_df_names(compl_clean_sa_vs_gom_m_int__join_metrics__both_p)

compl_clean_sa_vs_gom_m_int_tot__both <-
  add_total_cnt_in_gr(compl_clean_sa_vs_gom_m_int__join_metrics__both_p__comb, 
                      c("permit_sa_gom_dual_both", "year"))

# check
res1 <-
  compl_clean_sa_vs_gom_m_int__join_metrics |>
  select(vessel_official_number, year, permit_sa_gom_dual) |>
  distinct() |>
  count(year, permit_sa_gom_dual, 
        name = "total_vsl_y_by_year_perm") |>
  arrange(total_vsl_y_by_year_perm)

res1a <-
  compl_clean_sa_vs_gom_m_int__join_metrics__both_p |>
  select(vessel_official_number, year, permit_sa_gom_dual, permit_sa_gom_dual_both) |>
  distinct() |>
  count(year, permit_sa_gom_dual_both, name = "total_vsl_y_by_year_perm") |>
  arrange(total_vsl_y_by_year_perm)

# res1
# 1 2022  dual                                    302
# 2 2023  dual                                    315
# 3 2022  gom_only                                977
# 4 2023  gom_only                               1147
# 5 2023  sa_only                                2177
# 6 2022  sa_only                                2231

# res1a
# 1 2022  dual                                         302
# 2 2022  gom_only                                     977
# 3 2023  gom_only                                    1147
# 4 2022  sa_only                                     2231
# 5 2023  sa_dual                                     2492

res2 <-
  compl_clean_sa_vs_gom_m_int_tot |>
  select(permit_sa_gom_dual, year, total_vsl_y_by_year_perm) |>
  distinct() |>
  arrange(total_vsl_y_by_year_perm)

diffdf::diffdf(res1, res2)
# T

## get vessel counts by compliance (compl_counts) ----
### get compl, no compl, or both per year ----

get_compl_by <- function(my_df, group_by_for_compl) {
  my_df %>%
    dplyr::group_by_at(group_by_for_compl) %>%
    # can unique, because we are looking at vessels, not weeks
    unique() %>%
    # more columns, a column per vessel
    tidyr::pivot_wider(
      names_from = vessel_official_number,
      values_from = compliant_,
      # make it "NO_YES" if both
      values_fn = ~ paste0(sort(.x), collapse = "_")
    ) %>%
    dplyr::ungroup() %>%
    return()
}

# all columns except...
group_by_for_compl <- 
  vars(-c("vessel_official_number", "compliant_"))

# print_df_names(compl_clean_sa_vs_gom_m_int_tot__both)

# using all fields
# compl_clean_sa_vs_gom_m_int_c_cnt_tot_wide <-
#   compl_clean_sa_vs_gom_m_int_tot |>
#   dplyr::select(
#     vessel_official_number,
#     year,
#     permit_sa_gom_dual,
#     compliant_,
#     total_vsl_y_by_year_perm
#   ) |>
#   dplyr::distinct() |>
#   get_compl_by(group_by_for_compl)

# with sa_dual
compl_clean_sa_vs_gom_m_int_c_cnt_tot_wide__both <-
  compl_clean_sa_vs_gom_m_int_tot__both |>
  dplyr::select(
    vessel_official_number,
    year,
    permit_sa_gom_dual_both,
    year_permit_sa_gom_dual,
    compliant_,
    total_vsl_y_by_year_perm
  ) |>
  dplyr::distinct() |>
  get_compl_by(group_by_for_compl)

# [1]    2 3374
names(compl_clean_sa_vs_gom_m_int_c_cnt_tot_wide__both) |> 
  head() |> 
  glimpse()
 # chr [1:6] "permit_sa_gom_dual" "total_vsl_y_by_year_perm" "VI5498TB" "VA9447ZY" ...
 
# View(compl_clean_sa_vs_gom_m_int_c_cnt_tot_wide__both)
### count compl, no compl, or both per year, permit, active status ----

count_by_cols <- function(my_df,
                          cols_names) {
  my_df %>%
    # turn back to a longer format, vessel ids in one column
    tidyr::pivot_longer(
      # all other columns are vessel ids, use them as names
      cols = !any_of(cols_names),
      values_to = "is_compl_or_both",
      names_to = "vessel_official_number"
    ) %>%
    return()
}

cols_names <-
  c("year",
    "permit_sa_gom_dual_both",
    # "permit_sa_gom_dual",
    "total_vsl_y_by_year_perm",
    "year_permit_sa_gom_dual"
    # "perm_exp_y",
    # "exp_y_tot_cnt"
    )

compl_clean_sa_vs_gom_m_int_c_cnt_tot_wide_long_both <-
  count_by_cols(
    compl_clean_sa_vs_gom_m_int_c_cnt_tot_wide__both,
    cols_names
  )

# check compl_clean_sa_vs_gom_m_int_c_cnt_tot_wide_long
compl_clean_sa_vs_gom_m_int_c_cnt_tot_wide_long_both |> 
  arrange(vessel_official_number) |> 
  head()
#   permit_sa_gom_dual   total_vsl_y_by_year_perm vessel_official_number is_compl_or_both
#   <chr>                            <int> <chr>                  <chr>           
# 2022  gom_only                           977  1000042                NO_YES
# 2023 gom_only                      1147 1000042                YES
# 2022  sa_only                           2231 1000042                NA
# 4 2023  sa_dual                                   2436 1000042               

#### save sa_dual filter ----
sa_dual_filter <-
  rlang::quo(permit_sa_gom_dual == "sa_only" |
          (year == "2023" & permit_sa_gom_dual == "dual"))

# compl_clean_sa_vs_gom_m_int_c_cnt_tot_wide_long_sa <-
#   compl_clean_sa_vs_gom_m_int_c_cnt_tot_wide_long_ |>
#   filter(!!sa_dual_filter) |>
#   select(vessel_official_number, is_compl_or_both) |>
#   dplyr::distinct()

compl_clean_sa_vs_gom_m_int_c_cnt_tot_wide_long_sa_both <-
  compl_clean_sa_vs_gom_m_int_c_cnt_tot_wide_long_both |>
  filter(str_detect(permit_sa_gom_dual_both, "^sa_")) |>
  select(vessel_official_number, is_compl_or_both) |>
  dplyr::distinct()

dim(compl_clean_sa_vs_gom_m_int_c_cnt_tot_wide_long_sa_both)
# [1] 5793    2
# [1] 3372    2 sa_dual
# [1] 6614    2 both years
# [1] 7677    2
# [1] 5911    2  (sa_dual)

# View(compl_clean_sa_vs_gom_m_int_c_cnt_tot_wide_long_sa_both)
# [1] 5911    2 (sa_dual)

n_distinct(compl_clean_sa_vs_gom_m_int_c_cnt_tot_wide_long_sa_both$vessel_official_number)
# vessel_official_number 4016

n_distinct(compl_clean_sa_vs_gom_m_int_c_cnt_tot_wide_long_sa_both$is_compl_or_both)
# 4
# compl_clean_sa_vs_gom_m_int_c_cnt_tot_wide_long_sa$is_compl_or_both |> unique()
# [1] "YES"    "NO"     "NO_YES" NA      

compl_clean_sa_vs_gom_m_int_c_cnt_tot_wide_long_sa_non_c_both <-
  compl_clean_sa_vs_gom_m_int_c_cnt_tot_wide_long_sa_both |>
  filter(is_compl_or_both %in% c("NO", "NO_YES"))

dim(compl_clean_sa_vs_gom_m_int_c_cnt_tot_wide_long_sa_non_c_both)
# 370 2
# [1] 1545    2 no_yes
# 1859    both years
# 2210    

### get cnts for compl, no compl, or both per month with exp ----
cnts_for_compl <-
  function(my_df, group_by_cols, cols_to_cnt) {
    my_df %>%
      dplyr::group_by_at(group_by_cols) %>%
      unique() %>%
      # exclude vessel id
      dplyr::select(-vessel_official_number) %>%
      # count grouped by other columns
      dplyr::add_count(!!!syms(cols_to_cnt),
                       name = "compl_or_not_cnt") %>%
      unique() %>%
      dplyr::ungroup() %>%
      return()
  }

# group_by_cols <- c("year", "permit_sa_gom_dual")
# cols_to_cnt <- c("year", "permit_sa_gom_dual", "is_compl_or_both")

group_by_cols <- c("year", "permit_sa_gom_dual_both")
cols_to_cnt <- c("year", "permit_sa_gom_dual_both", "is_compl_or_both")

print_df_names(compl_clean_sa_vs_gom_m_int_c_cnt_tot_wide_long_both)
# [1] "year, permit_sa_gom_dual, total_vsl_y_by_year_perm, vessel_official_number, is_compl_or_both"
# [1] "year, permit_sa_gom_dual_both, year_permit_sa_gom_dual, total_vsl_y_by_year_perm, vessel_official_number, is_compl_or_both"

#TODO: rename to _both from here
compl_clean_sa_vs_gom_m_int_tot_exp_y_short_wide_long_cnt <-
  cnts_for_compl(compl_clean_sa_vs_gom_m_int_c_cnt_tot_wide_long_both,
                 group_by_cols,
                 cols_to_cnt)

dim(compl_clean_sa_vs_gom_m_int_tot_exp_y_short_wide_long_cnt)
# [1] 21  7
# [1] 7 4 (no exp)
# 22 5 both years
# [1] 23  5
# [1] 19  6 sa_dual

# View(compl_clean_sa_vs_gom_m_int_tot_exp_y_short_wide_long_cnt)

#### check counts ----
# print_df_names(compl_clean_sa_vs_gom_m_int_tot_exp_y_short_wide_long_cnt)
# [1] "permit_sa_gom_dual, total_vsl_y_by_year_perm, is_compl_or_both, compl_or_not_cnt"

compl_clean_sa_vs_gom_m_int_tot_exp_y_short_wide_long_cnt %>%
  # remove NAs
  dplyr::filter(stats::complete.cases(is_compl_or_both)) %>%
  dplyr::select(
    year,
    permit_sa_gom_dual_both,
    # permit_sa_gom_dual,
    total_vsl_y_by_year_perm,
    compl_or_not_cnt,
    is_compl_or_both
  ) %>%
  dplyr::group_by(year, permit_sa_gom_dual_both) %>%
  # dplyr::group_by(year, permit_sa_gom_dual) %>%
  # get sums
  dplyr::mutate(sum_cnts = sum(compl_or_not_cnt)) %>% 
  # dplyr::filter(!total_vsl_y_by_year_perm == sum_cnts) %>%
  ungroup() |>
  # nrow()
  # 0 OK
  unique() %>%
  dplyr::group_by(is_compl_or_both) %>%
  dplyr::mutate(sum_compl_or_not_cnt = sum(compl_or_not_cnt)) %>%
  dplyr::select(is_compl_or_both, sum_compl_or_not_cnt) %>%
  unique() %>%
  ungroup() |>
  dplyr::glimpse()
# $ is_compl_or_both     <chr> "YES", "NO", "NO_YES"
# $ sum_compl_or_not_cnt <int> 1752, 742, 2350
# $ sum_compl_or_not_cnt <int> 1825, 370, 1177 (2023)
# $ sum_compl_or_not_cnt <int> 4102, 845, 2202 (both years with processed)
# $ sum_compl_or_not_cnt <int> 4095, 843, 2155 (sa_dual)

### One vessel in 2 groups ----
# The number should be the same as the total number we got earlier. It is not, which means One vessel is in 2 perm_exp_y groups, has both expired and not expired permit in 2023.

### check if a vessel is compliant and not at the same time
# 'n_distinct(is_compl_or_both)' calculates the count of distinct values in the 'is_compl_or_both' column for each row.
# 'n_distinct(.$is_compl_or_both)' calculates the count of distinct values in the 'is_compl_or_both' column for the entire data frame.
# The equality comparison '== n_distinct(.$is_compl_or_both)' checks if the count of distinct values is the same for the entire data frame and each group.

# print_df_names(compl_clean_sa_vs_gom_m_int_tot_exp_y_short_wide_long_cnt)

# compl_clean_sa_vs_gom_m_int_tot_exp_y_short_wide_long %>%
compl_clean_sa_vs_gom_m_int_c_cnt_tot_wide_long_both |> 
  dplyr::filter(!is.na(is_compl_or_both)) %>%
  dplyr::group_by(vessel_official_number) %>%
  dplyr::mutate(shared =
                  dplyr::n_distinct(is_compl_or_both) == dplyr::n_distinct(.$is_compl_or_both)) %>%
  dplyr::filter(shared == TRUE) %>%
  ungroup() |>
  nrow()
# 0 - OK

# check if a vessel permit is expired and not in the same time
# compl_clean_sa_vs_gom_m_int_tot_exp_y_short_wide_long %>%
#   dplyr::filter(!is.na(is_compl_or_both)) %>%
#   dplyr::group_by(vessel_official_number) %>%
#   dplyr::mutate(shared =
#                   dplyr::n_distinct(perm_exp_y) == dplyr::n_distinct(.$perm_exp_y)) %>%
#   dplyr::filter(shared == TRUE) %>%
#   dplyr::arrange(vessel_official_number) %>%
#   ungroup() |> 
#   dim()
# 2
# 1000164 is both exp and not

### check total_vsl_y_by_year_perm vs. sum_cnts (should be equal, see dbl FL7825PU) ----
compl_clean_sa_vs_gom_m_int %>%
  # dplyr::filter(!!sa_dual_filter) %>%
  dplyr::group_by(year, compliant_) %>%
  dplyr::mutate(tota_vsl_m =
                  dplyr::n_distinct(vessel_official_number)) %>%
  dplyr::ungroup() %>%
  dplyr::select(year, tota_vsl_m, compliant_) %>%
  unique() %>%
  head()
#   year  tota_vsl_m compliant_
#   <chr>      <int> <chr>     
# 1 2022        2959 YES       
# 2 2022        1390 NO        
# 3 2023        3177 YES       
# 4 2023        1558 NO        

## add total cnts by compliance ----
# active vs expired per year, permit, compl, permit expiration

add_total_cnts <-
  function(my_df, group_by_compl_cols, group_by_exp_cols) {
    my_df %>%
      # remove NAs
      dplyr::filter(stats::complete.cases(is_compl_or_both)) %>%
      dplyr::mutate(
        compl_or_not =
          dplyr::case_when(is_compl_or_both == "YES" ~
                             "compliant",
                           .default = "non_compliant")
      ) %>%
      dplyr::group_by_at(group_by_compl_cols) %>%
      # add counts by compliant
      dplyr::mutate(cnt_y_p_c = sum(compl_or_not_cnt)) %>%
      dplyr::ungroup() %>%
      return()
  }

# group_by_cols1 <- c("year", "permit_sa_gom_dual", "compl_or_not")
# group_by_cols2 <- c("permit_sa_gom_dual", "perm_exp_y")
group_by_cols3 <- 
  c("year", 
    "permit_sa_gom_dual_both",
    "compl_or_not")

compl_clean_sa_vs_gom_m_int_tot_exp_y_short_wide_long_cnt_tot_y <-
  add_total_cnts(compl_clean_sa_vs_gom_m_int_tot_exp_y_short_wide_long_cnt,
                 group_by_cols3)

# glimpse(compl_clean_sa_vs_gom_m_int_tot_exp_y_short_wide_long_cnt_tot_y)

## add percents of total ----
add_percents_of_total <-
  function(my_df, select_cols) {
    my_df %>%
      dplyr::select(all_of(select_cols)) %>%
      distinct() %>%
      dplyr::mutate(perc_c_or_not = 100 * cnt_y_p_c / total_vsl_y_by_year_perm) %>%
      return()
  }

select_cols <- c(
  "year",
  "permit_sa_gom_dual_both",
  # "permit_sa_gom_dual",
  "year_permit_sa_gom_dual",
  "total_vsl_y_by_year_perm",
  "compl_or_not",
  "cnt_y_p_c"
)

# print_df_names(compl_clean_sa_vs_gom_m_int_tot_exp_y_short_wide_long_cnt_tot_y)

compl_clean_sa_vs_gom_m_int_tot_exp_y_short_wide_long_cnt_tot_y_perc <-
  add_percents_of_total(compl_clean_sa_vs_gom_m_int_tot_exp_y_short_wide_long_cnt_tot_y,
                        select_cols)

# View(compl_clean_sa_vs_gom_m_int_tot_exp_y_short_wide_long_cnt_tot_y_perc)

dim(compl_clean_sa_vs_gom_m_int_tot_exp_y_short_wide_long_cnt_tot_y_perc)
# [1] 11  8
# 7 8 (sa_dual)
# [1] 4 5 no exp
# [1] 12  6
# [1] 10  7 sa_dual

# plots for compl vs. non compl vessels per year ----

# "Permitted SEFHIER Vessels"
gg_all_c_vs_nc_plots <-
  compl_clean_sa_vs_gom_m_int_tot_exp_y_short_wide_long_cnt_tot_y_perc$permit_sa_gom_dual_both %>%
  unique() %>%
  # repeat for each permit_sa_gom_dual and each year
  purrr::map(function(curr_permit_sa_gom_dual) {
    
    c(my_year1, my_year2) |>
      map(\(curr_year) {
        # browser()
        curr_df <-
          compl_clean_sa_vs_gom_m_int_tot_exp_y_short_wide_long_cnt_tot_y_perc %>%
          dplyr::filter(permit_sa_gom_dual_both == curr_permit_sa_gom_dual &
                          year == curr_year)
        
        # for 2022 SA_only if using sa_dual
        if (nrow(curr_df) == 0) {
          return(NULL)
        }
        
        # See function definition F2
        total_vsls <- unique(curr_df$total_vsl_y_by_year_perm)
        
        curr_title_permit <-
          title_permits %>%
          filter(permit_sa_gom_dual == curr_permit_sa_gom_dual)
        
        current_title <-
          str_glue("{curr_year}: {curr_title_permit$title} {curr_title_permit$second_part}")
        
        one_plot <-
          curr_df %>%
          dplyr::select(compl_or_not, perc_c_or_not) %>%
          unique() %>%
          # See function definition F2
          make_one_plot_compl_vs_non_compl(
            current_title,
            is_compliant = "compl_or_not",
            percent = "perc_c_or_not",
            no_legend = TRUE,
            legend_labels = c("Compliant", "Non Compliant")
          )
        
        return(one_plot)
        
      })
  })

gg_all_c_vs_nc_plots

## Make a flat list of plots with names ----
make_flat_plot_list <- function(list_of_plots) {
  flat_plot_list_all <- list_flatten(list_of_plots)
  
  # rm NULLs
  flat_plot_list <- compact(flat_plot_list_all)
}
  
make_flat_plot_list_names <- function(flat_plot_list) {
  flat_plot_list_names <-
    flat_plot_list |>
    map(\(x) {
      x$labels$title |>
        str_replace_all(" ", "_") |>
        tolower() |>
        str_replace("_permitted_vessels", "") |>
        str_replace_all("[^a-z0-9_]", "_")
    })
  
  names(flat_plot_list) <-
    flat_plot_list_names
  
  return(flat_plot_list)
}

flat_plot_list <-
  make_flat_plot_list(gg_all_c_vs_nc_plots) |> 
  make_flat_plot_list_names()

### check the names ---
flat_plot_list |>
  map(\(x) {
    x$labels$title
  })

# View(flat_plot_list)

# sa_only <- gg_all_c_vs_nc_plots[[1]]
# dual <- gg_all_c_vs_nc_plots[[2]]
# gom_only <- gg_all_c_vs_nc_plots[[3]]

# sa_only <- gg_all_c_vs_nc_plots[[1]]
# dual <- gg_all_c_vs_nc_plots[[2]]
# gom_only <- gg_all_c_vs_nc_plots[[3]]

main_title <- "Percent Compliant vs. Noncompliant SEFHIER Vessels"

my_grobs_list <-
  flat_plot_list
# list(gg_all_c_vs_nc_plots[[1]])

# combine plots for 2023
grid.arrange(grobs = my_grobs_list,
             top = main_title)

# see the function definition F2
my_grobs_list |>
  names() |> 
  map(\(plot_name) {
    # browser()
    curr_plot <- my_grobs_list[[plot_name]]
      
    # file_name_part <-
    #   plot_name$labels$title |>
    #   str_replace_all(" ", "_") |>
    #   str_replace("(^[0-9]+):_(.+)", "\\2_\\1") |>
    #   str_replace("_permitted_vessels", "") |>
    #   tolower()
    
    file_full_name_c_nc <-
      file.path(plot_file_path,
                str_glue("compl_vs_nonc_plots_{plot_name}.png"))
    
    save_plots_list_to_files(file_full_name_c_nc,
                             curr_plot,
                             my_width = 20,
                             my_height = 10)
  })

# [1] "C:/Users/anna.shipunova/Documents/R_files_local/my_outputs/quantify_compliance_2023/2024-02-09/compl_vs_nonc_plots_2023__sa___dual.png"

# Non compliant only ----
# compl_clean_sa_vs_gom_m_int_tot |> print_df_names()

# 1) count percents - a given vsl non_compl per counted weeks total ----
## 1a) how many weeks each vessel was present in a year ----

# print_df_names(compl_clean_sa_vs_gom_m_int_tot__both)

compl_clean_sa_vs_gom_m_int_tot_short <-
  compl_clean_sa_vs_gom_m_int_tot__both |>
  select(
    -c(
      name,
      permitgroup,
      permit_groupexpiration,
      week,
      gom_permitteddeclarations__,
      captainreports__,
      negativereports__,
      complianceerrors__,
      set_permits_on_hold_,
      overridden_,
      override_date,
      override_by,
      contactedwithin_48_hours_,
      submittedpower_down_,
      # week_num,
      week_end,
      # year_month,
      year_quarter,
      vessel_name,
      effective_date,
      end_date,
      permits,
      sa_permits_,
      gom_permits_,
      permit_region
    )
  ) |> 
  distinct()

# dim(compl_clean_sa_vs_gom_m_int_tot)
# [1] 535295     31
# dim(compl_clean_sa_vs_gom_m_int_tot__both)
# [1] 535295     33
dim(compl_clean_sa_vs_gom_m_int_tot_short)
# [1] 298147      10

# print_df_names(compl_clean_sa_vs_gom_m_int_tot_short)
# [1] "vessel_official_number, year, compliant_, week_num, week_start, year_month, permit_sa_gom_dual, permit_sa_gom_dual_both, year_permit_sa_gom_dual"

# compl_clean_sa_vs_gom_m_int_tot_short_week <-
#   add_total_cnt_in_gr(compl_clean_sa_vs_gom_m_int_tot_short,
#                       c("permit_sa_gom_dual", "year", "week_start"),
#                       "")

# count weeks per vessel / year
compl_clean_sa_vs_gom_m_int_tot_short_week_cnt <-
  compl_clean_sa_vs_gom_m_int_tot_short |>
  group_by(vessel_official_number, year) |>
  # cnt distinct week_start in each group
  dplyr::mutate(total_weeks_per_vessel =
                  dplyr::n_distinct(week_start)) %>%
  dplyr::ungroup()

# View(compl_clean_sa_vs_gom_m_int_tot_short_week_cnt)

# count weeks per vessel / year / compliance
compl_clean_sa_vs_gom_m_int_tot_short_week_compl_cnt <-
  compl_clean_sa_vs_gom_m_int_tot_short_week_cnt |>
  group_by(vessel_official_number, year, compliant_) |>
  # cnt distinct week_start in each group
  dplyr::mutate(weeks_per_vessel_per_compl =
                  dplyr::n_distinct(week_start)) %>%
  dplyr::ungroup()

# check
# glimpse(compl_clean_sa_vs_gom_m_int_tot_short_week_compl_cnt)

## test 1a ----
# have both comp and not
compl_clean_sa_vs_gom_m_int_tot_short_week_cnt %>% 
  select(vessel_official_number, year, compliant_) |>
  distinct() |> 
    group_by(vessel_official_number) |> 
    filter(n_distinct(compliant_) > 1) |> 
    arrange(vessel_official_number) |> head()

compl_clean_sa_vs_gom_m_int_tot_short_week_compl_cnt %>%
  dplyr::filter(vessel_official_number == "1020822") %>%
  dplyr::select(year,
                compliant_,
                weeks_per_vessel_per_compl,
                total_weeks_per_vessel) %>%
  unique()
# year compliant_ weeks_per_vessel_per_compl total_weeks_per_vessel
# 1 2022 YES 33 52
# 2 2022 NO 19 52
# 3 2023 YES 47 52
# 4 2023 NO 5 52

# compl_clean_sa_vs_gom_m_int_tot_short_week_compl_cnt |> 
# print_df_names()

nc_2023_sa_only_test <-
  compl_clean_sa_vs_gom_m_int_tot_short_week_compl_cnt %>% 
  dplyr::filter(
    # !!sa_dual_filter,
    str_detect(year_permit_sa_gom_dual, "sa_"),
    compliant_ == "NO",
    vessel_official_number %in%
      c("VA9447ZY",
        "VA8261ZY",
        "VA2031CK",
        "VA1460CJ",
        "VA1267CJ")
  ) %>%
  dplyr::select(vessel_official_number,
                weeks_per_vessel_per_compl,
                total_weeks_per_vessel) %>%
  unique()

head(nc_2023_sa_only_test)
#   vessel_official_number weeks_per_vessel_per_compl total_weeks_per_vessel
#   <chr>                                       <int>                  <int>
# 1 VA9447ZY  11                     11
# 2 VA8261ZY  8                     52
# 3 VA2031CK  33                     33
# 4 VA1460CJ  50                     50
# 5 VA1267CJ  2                     48

compl_clean_sa_vs_gom_m_int_tot_short_week_compl_cnt %>%
  dplyr::filter(
    vessel_official_number == "SC8907DF"
  ) %>%
  dplyr::select(vessel_official_number,
                weeks_per_vessel_per_compl,
                compliant_,
                total_weeks_per_vessel) %>%
  unique() %>%
  dplyr::glimpse()
# For 2023
# $ weeks_per_vessel_per_compl <int> 42, 10
# $ compliant_                 <chr> "NO", "YES"
# $ total_weeks_per_vessel     <int> 52, 52

## 1b) percent of compl/non-compl per total weeks each vsl was present ----
# print_df_names(compl_clean_sa_vs_gom_m_int_tot_short_week_compl_cnt)

# compl_clean_sa_vs_gom_m_int_tot_short_week_compl_cnt |> 
#    select(year_permit_sa_gom_dual, total_vsl_y_by_year_perm) |> 
#    distinct() |> 
#    View()

count_weeks_per_vsl_permit_year_compl_p <-
  compl_clean_sa_vs_gom_m_int_tot_short_week_compl_cnt %>%
  group_by(year, compliant_) |>
  mutate(percent_compl =
           weeks_per_vessel_per_compl * 100 / total_weeks_per_vessel) |>
  ungroup()

dim(count_weeks_per_vsl_permit_year_compl_p)
# [1] 185251     32
# [1] 143767     31 (2023)
# [1] 298147     12 both

# test
# count_weeks_per_vsl_permit_year_compl_p$permit_sa_gom_dual |>
#   unique()
# [1] "sa_only"  "dual"     "gom_only"

count_weeks_per_vsl_permit_year_compl_p %>%
  filter(permit_sa_gom_dual %in% c("sa_only", "dual"), 
         year == my_year2) %>%
  select(vessel_official_number) %>%
  unique() %>%
  dim()
# [1] 2178
# 2421 (2023)
# 2436    

count_weeks_per_vsl_permit_year_compl_p %>%
  filter(permit_sa_gom_dual %in% c("sa_only", "dual"), 
         year == my_year2,
         compliant_ == "NO") %>%
  select(vessel_official_number) %>%
  # unique() %>%
  # 1289    Non compliant vsl
  # 1545 (2023)
  # 1549    
  dim()
# [1] 26466 non compliant weeks
# 24302 (2023)
# 25382     

### test 1b ----
count_weeks_per_vsl_permit_year_compl_p %>%
  filter(vessel_official_number == "1020822",
         year == "2023") %>%
  select(
    year,
    permit_sa_gom_dual,
    permit_sa_gom_dual_both,
    year_permit_sa_gom_dual,
    compliant_,
    weeks_per_vessel_per_compl,
    total_weeks_per_vessel,
    percent_compl
  ) %>%
  unique() %>%
  dplyr::glimpse()
# $ year                       <chr> "2023", "2023"
# $ permit_sa_gom_dual              <chr> "sa_only", "sa_only"
# $ year_permit_sa_gom_dual    <chr> "2023 sa_dual", "2023 sa_dual"
# $ compliant_                 <chr> "YES", "NO"
# $ weeks_per_vessel_per_compl <int> 47, 5
# $ total_weeks_per_vessel     <int> 52, 52
# $ percent_compl              <dbl> 90.384615, 9.615385

grep("year_", 
     names(count_weeks_per_vsl_permit_year_compl_p), 
     value = T)
# [1] "year_month"               "year_permit_sa_gom_dual" 
# [3] "total_vsl_y_by_year_perm"
