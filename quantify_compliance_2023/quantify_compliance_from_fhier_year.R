# by Year: ----

## year add total counts ----
# (both compl. and not, a vsl can be in both)

# Explanations:
# The function 'add_total_cnt_in_gr' performs the following operations:
# 1. Groups the data frame by the specified columns using 'group_by_at'.
# 2. Adds a new column 'new_col_name' representing the count of distinct vessel official numbers in each group using `{{new_col_name}} := dplyr::n_distinct(vessel_official_number)`.
# The syntax `{{new_col_name}} :=` is used to create a new column dynamically with the name provided in the new_col_name argument.
# 3. Removes the grouping to return the data to its original structure with 'ungroup'.
# 4. Returns the modified data frame.

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
  add_total_cnt_in_gr(
    compl_clean_sa_vs_gom_m_int__join_metrics__both_p__comb,
    c("permit_sa_gom_dual_both", "year")
  )

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
  select(vessel_official_number,
         year,
         permit_sa_gom_dual,
         permit_sa_gom_dual_both) |>
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

# get_compl_by <- function(my_df, group_by_for_compl) {
#   my_df %>%
#     dplyr::group_by_at(group_by_for_compl) %>%
#     # can unique, because we are looking at vessels, not weeks
#     unique() %>%
#     # more columns, a column per vessel
#     tidyr::pivot_wider(
#       names_from = vessel_official_number,
#       values_from = compliant_,
#       # make it "NO_YES" if both
#       values_fn = ~ paste0(sort(.x), collapse = "_")
#     ) %>%
#     dplyr::ungroup() %>%
#     return()
# }

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
 
### count compl, no compl, or both per year, permit, active status ----
# Explanations:
#   tidyr::pivot_longer(cols = !any_of(cols_names), values_to = "is_compl_or_both", names_to = "vessel_official_number") %>%: Turns the data frame back to a longer format with vessel ids in one column using 'pivot_longer'. It specifies that all other columns except those in 'cols_names' should be used as vessel ids, and the resulting values should be placed in the "is_compl_or_both" column.

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
# Explanations:
# The code uses the `rlang::quo` function to create a quosure named 'sa_dual_filter'. This quosure represents a logical condition that filters rows based on two conditions:
# - Rows where 'permit_sa_gom_dual' is equal to "sa_only".
# - Rows where 'year' is equal to "2023" and 'permit_sa_gom_dual' is equal to "dual".
# These conditions are combined with the logical OR (`|`) operator. The resulting quosure can be used later in dplyr functions for filtering data based on this logical condition.
sa_dual_filter <-
  rlang::quo(permit_sa_gom_dual == "sa_only" |
          (year == "2023" & permit_sa_gom_dual == "dual"))

compl_clean_sa_vs_gom_m_int_c_cnt_tot_wide_long_sa_both <-
  compl_clean_sa_vs_gom_m_int_c_cnt_tot_wide_long_both |>
  filter(str_detect(permit_sa_gom_dual_both, "^sa_")) |>
  select(vessel_official_number, is_compl_or_both) |>
  dplyr::distinct()

dim(compl_clean_sa_vs_gom_m_int_c_cnt_tot_wide_long_sa_both)
# [1] 5652    2

n_distinct(compl_clean_sa_vs_gom_m_int_c_cnt_tot_wide_long_sa_both$vessel_official_number)
# vessel_official_number 4016

n_distinct(compl_clean_sa_vs_gom_m_int_c_cnt_tot_wide_long_sa_both$is_compl_or_both)
# 4
compl_clean_sa_vs_gom_m_int_c_cnt_tot_wide_long_sa_both$is_compl_or_both |> unique()
# [1] "YES"    "NO"     "NO_YES" NA      

## get non compliant only ----
# If was non compliant once a year, then is non compliant the whole year.
compl_clean_sa_vs_gom_m_int_c_cnt_tot_wide_long_sa_non_c_both <-
  compl_clean_sa_vs_gom_m_int_c_cnt_tot_wide_long_sa_both |>
  filter(is_compl_or_both %in% c("NO", "NO_YES"))

dim(compl_clean_sa_vs_gom_m_int_c_cnt_tot_wide_long_sa_non_c_both)
# [1] 2024    2

### get cnts for compl, no compl, or both per month with exp ----

# Explanation:
# 1. **Grouping Data**: The data frame is grouped by the specified columns using 'group_by_at'.
# 2. **Removing Duplicates (Within Groups)**: Duplicates are removed within each group using 'unique'.
# 3. **Excluding Column**: The 'vessel_official_number' column is excluded from the grouped data using 'select'.
# 4. **Counting Occurrences**: The occurrences of combinations of columns specified in 'cols_to_cnt' are counted within each group using 'add_count', and the new column is named "compl_or_not_cnt".

# syms(cols_to_cnt): This function is from the rlang package and is used to quote a list of symbols. In this case, it is quoting the symbols representing column names specified in the cols_to_cnt variable.
# 
# !!!: The bang-bang (!!!) is the unquote-splice operator. It is used within a function argument to unquote and splice the quoted expressions. It takes a list of expressions and splices them into the surrounding call. In this context, it is used to unquote and pass the symbols as separate arguments to the add_count function.
# 
# So, !!!syms(cols_to_cnt) is unquoting and splicing the symbols representing column names into the add_count function, which expects individual column names to count occurrences within groups.
# 
# In simpler terms, it allows you to pass a dynamic list of column names to the add_count function, depending on what is specified in the cols_to_cnt variable.

# 5. **Removing Duplicates (Again)**: Duplicates are removed again to keep only unique combinations.
# 6. **Ungrouping Data**: The grouping is removed to return the data to its original structure using 'ungroup'.
# 7. **Returning Modified Data Frame**: The modified data frame is returned from the function.
cnts_for_compl <-
  function(my_df, group_by_cols, cols_to_cnt) {
    my_df %>%
      dplyr::group_by_at(group_by_cols) %>%
      distinct() %>%
      # exclude vessel id
      dplyr::select(-vessel_official_number) %>%
      # count grouped by other columns
      dplyr::add_count(!!!syms(cols_to_cnt),
                       name = "compl_or_not_cnt") %>%
      distinct() %>%
      dplyr::ungroup() %>%
      return()
  }

group_by_cols <- c("year", "permit_sa_gom_dual_both")
cols_to_cnt <- c("year", "permit_sa_gom_dual_both", "is_compl_or_both")

print_df_names(compl_clean_sa_vs_gom_m_int_c_cnt_tot_wide_long_both)
# [1] "year, permit_sa_gom_dual_both, year_permit_sa_gom_dual, total_vsl_y_by_year_perm, vessel_official_number, is_compl_or_both"

compl_clean_sa_vs_gom_m_int_tot_exp_y_short_wide_long_cnt <-
  cnts_for_compl(compl_clean_sa_vs_gom_m_int_c_cnt_tot_wide_long_both,
                 group_by_cols,
                 cols_to_cnt)

dim(compl_clean_sa_vs_gom_m_int_tot_exp_y_short_wide_long_cnt)
# [1] 23  6

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
# Explanations:
# 1. Filter out rows with missing values in the 'is_compl_or_both' column
# 2. Create a new column 'compl_or_not' based on the values in 'is_compl_or_both'
# 3. Group the data frame by the columns specified in 'group_by_compl_cols'
# 4. Add a new column 'cnt_y_p_c' representing the sum of 'compl_or_not_cnt' within each group
# 5. Ungroup the data frame to remove grouping structure
# 6. Return the modified data frame

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

dim(compl_clean_sa_vs_gom_m_int_tot_exp_y_short_wide_long_cnt_tot_y)
# [1] 17  8

## add percents of total ----
# Explanations:
# 1. Select columns specified in 'select_cols' from the data frame
# 2. Remove duplicate rows to get unique combinations of selected columns
# 3. Add a new column 'perc_c_or_not' representing the percentage of compliant counts
#    relative to the total count ('cnt_y_p_c') within each unique combination
# 4. Return the modified data frame

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

compl_clean_sa_vs_gom_m_int_tot_exp_y_short_wide_long_cnt_tot_y_perc <-
  add_percents_of_total(compl_clean_sa_vs_gom_m_int_tot_exp_y_short_wide_long_cnt_tot_y,
                        select_cols)

dim(compl_clean_sa_vs_gom_m_int_tot_exp_y_short_wide_long_cnt_tot_y_perc)
# [1] 12  7 sa_only
# [1] 10  7 sa_dual

# plots for compl vs. non compl vessels per year ----

# "Permitted SEFHIER Vessels"

# Explanations:

# gg_all_c_vs_nc_plots function generates a list of plots for each combination of
# permit_sa_gom_dual and year from the provided data frame.

# 1. Extract unique values of permit_sa_gom_dual
# 2. Use purrr::map to iterate over each permit_sa_gom_dual
# 3. Inside the outer map, use purrr::map to iterate over each year (my_year1, my_year2)
# 4. Filter the data frame based on the current permit_sa_gom_dual and year
# 5. If the filtered data frame is empty (no rows), return NULL for that combination
# 6. Calculate unique total_vsls using the make_one_plot_compl_vs_non_compl function (F2)
# 7. Extract current title information for the plot based on the permit_sa_gom_dual
# 8. Create a plot using make_one_plot_compl_vs_non_compl function (F2)
# 9. Return the generated plot for the current permit_sa_gom_dual and year

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

# gg_all_c_vs_nc_plots

## Make a flat list of plots with names ----
# Explanations:
# The make_flat_plot_list function takes a list of plots and flattens it, removing NULL values.

# 1. Use list_flatten from the purrr package to flatten the list of plots.
# 2. Remove NULL values from the flattened list using compact.

make_flat_plot_list <- function(list_of_plots) {
  flat_plot_list_all <- list_flatten(list_of_plots)
  
  # rm NULLs
  flat_plot_list <- compact(flat_plot_list_all)
}

# Explanations:
# 1. Use map from the purrr package to iterate over each plot in the flat_plot_list.
# 2. Process the title of each plot: replace spaces with underscores, convert to lowercase, remove specific substrings, and replace non-alphanumeric characters with underscores.
# 3. Set the processed titles as names for the flat_plot_list.
# 4. Return the flat_plot_list with updated names.

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

main_title <- "Percent Compliant vs. Noncompliant SEFHIER Vessels"

my_grobs_list <-
  flat_plot_list

# combine plots for 2023
grid.arrange(grobs = my_grobs_list,
             top = main_title)

# see the function definition F2
# This code iterates over a list of ggplot2 objects (`my_grobs_list`), retrieves each plot by name, constructs a file name, and saves each plot as a PNG file using the `save_plots_list_to_files` function. The file names are constructed based on the `plot_file_path` and a formatted plot name. The width and height of the saved plots are specified as 20 and 10, respectively.

# 1. Use the names() function to obtain the names of the plots in my_grobs_list.
# 2. Use map from the purrr package to iterate over each plot name.
# 3. Inside the map function, retrieve the current plot using my_grobs_list[[plot_name]].
# 4. Construct the file_full_name_c_nc by combining plot_file_path and a formatted plot name.
# 5. Use save_plots_list_to_files to save the current plot to a PNG file with specified width and height.

my_grobs_list |>
  names() |> 
  map(\(plot_name) {
    # browser()
    curr_plot <- my_grobs_list[[plot_name]]
      
    file_full_name_c_nc <-
      file.path(plot_file_path,
                str_glue("compl_vs_nonc_plots_{plot_name}.png"))
    
    save_plots_list_to_files(file_full_name_c_nc,
                             curr_plot,
                             my_width = 20,
                             my_height = 10)
  })

# [1] "~/R_files_local/my_outputs/quantify_compliance_2023/2024-02-09/compl_vs_nonc_plots_2023__sa___dual.png"

# Non compliant only ----
# compl_clean_sa_vs_gom_m_int_tot |> print_df_names()

# 1) count percents - a given vsl non_compl per counted weeks total ----
## 1a) how many weeks each vessel was present in a year ----

# print_df_names(compl_clean_sa_vs_gom_m_int_tot__both)

### fewer columns ----
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

dim(compl_clean_sa_vs_gom_m_int_tot_short)
# [1] 298147      10

# count weeks per vessel / year
compl_clean_sa_vs_gom_m_int_tot_short_week_cnt <-
  compl_clean_sa_vs_gom_m_int_tot_short |>
  group_by(vessel_official_number, year) |>
  # cnt distinct week_start in each group
  dplyr::mutate(total_weeks_per_vessel =
                  dplyr::n_distinct(week_start)) %>%
  dplyr::ungroup()

# count weeks per vessel / year / compliance
compl_clean_sa_vs_gom_m_int_tot_short_week_compl_cnt <-
  compl_clean_sa_vs_gom_m_int_tot_short_week_cnt |>
  group_by(vessel_official_number, year, compliant_) |>
  # cnt distinct week_start in each group
  dplyr::mutate(weeks_per_vessel_per_compl =
                  dplyr::n_distinct(week_start)) %>%
  dplyr::ungroup()

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

count_weeks_per_vsl_permit_year_compl_p <-
  compl_clean_sa_vs_gom_m_int_tot_short_week_compl_cnt %>%
  group_by(year, compliant_) |>
  mutate(percent_compl =
           weeks_per_vessel_per_compl * 100 / total_weeks_per_vessel) |>
  ungroup()

dim(count_weeks_per_vsl_permit_year_compl_p)
# [1] 298147     13

# test
count_weeks_per_vsl_permit_year_compl_p$permit_sa_gom_dual |>
  unique()
# [1] "sa_only"  "dual"     "gom_only"

count_weeks_per_vsl_permit_year_compl_p %>%
  filter(permit_sa_gom_dual %in% c("sa_only", "dual"), 
         year == my_year2) %>%
  select(vessel_official_number) %>%
  unique() %>%
  dim()
# 2436    

count_weeks_per_vsl_permit_year_compl_p %>%
  filter(permit_sa_gom_dual %in% c("sa_only", "dual"), 
         year == my_year2,
         compliant_ == "NO") %>%
  select(vessel_official_number) %>%
  # unique() %>%
  # 1549    
  dim()
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
