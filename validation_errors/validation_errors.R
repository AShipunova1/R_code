library(data.table)
# install.packages("xlsx")
library(xlsx)
library(viridis)
source("~/R_code_github/useful_functions_module.r")
my_paths <- set_work_dir()
source("~/R_code_github/validation_errors/validation_errors_get_data.r")

## From DB ====
### From db by year ====
by_year <- function(my_df, fields_to_select_list) {
  my_df %>%
    select(all_of(fields_to_select_list)) %>%
    group_by(arr_year) %>%
    summarise(n = n()) %>%
    return()
}

by_year(dat_pending_date, c("trip_report_id", "arr_year"))
# by year
# A tibble: 4 × 2
#   arr_year     n
#   <chr>    <int>
# 1 2021        41
# 2 2022     45376
# 3 2023      2827
# 4 NA         175

dat_pending_date %>%
  select(trip_report_id, overridden, arr_year) %>%
  group_by(overridden, arr_year) %>%
  summarise(n = n())

### From db by year_month ====
by_year_month <- function(my_df, fields_to_select_list) {
  my_df %>%
    select(all_of(fields_to_select_list)) %>%
    group_by(arr_year_month) %>%
    summarise(n = n()) %>%
    return()
}

db_pending_by_year_month <-
  by_year_month(dat_pending_date, c("trip_report_id", "arr_year_month"))

View(db_pending_by_year_month)
# A tibble: 17 × 2

by_year_month_wide <- function(my_df, fields_to_select_list) {
  my_df %>%
    select(all_of(fields_to_select_list)) %>%
    group_by(overridden, arr_year_month) %>%
    summarise(n = n()) %>%
    # A tibble: 23 × 3
    # ungroup() %>%
    pivot_wider(names_from = overridden, values_from = n) %>%
    # NAs to 0
    mutate(pending = coalesce(pending, 0),
           overridden = coalesce(overridden, 0)) %>%
    arrange(arr_year_month) %>%
    # tail()
    mutate(total = overridden + pending) %>%
    return()
}

dat_pending_date_by_ym <-
  by_year_month_wide(dat_pending_date,
                     c("trip_report_id", "overridden", "arr_year_month"))

# all.equal(dat_pending_date_by_ym, dat_pending_date_by_ym1)
View(dat_pending_date_by_ym)

## Query parameterization ====

make_sql_parameters <- function(my_param_df, sql_text) {
  param_list <- paste0("(",
                       paste0("?parameter", seq_along(my_param_df),
                              collapse = ",\n  "),
                       ")")
  sql_text <- paste0(sql_text_in, param_list)
  cat(sql_text)
  
  sql_params <-
    setNames(as.list(my_param_df), paste0("parameter", seq_along(my_param_df)))
  str(sql_params)
  
  sql <- sqlInterpolate(ANSI(),
                        sql = sql_text,
                        .dots = sql_params)
  
  return(sql)
}

### Split by assignment and validation error ====

#### Split from DB ====
dat_pending_data_22_plus <- dat_pending_date %>%
  filter(departure_date >= "2022-01-01" &
           is_enabled == 1)

# names(dat_pending_data_22)

# View(dat_pending_data_22)

fields_to_keep <-
  c(
    "val_param_name",
    "trip_report_id",
    "ovr_flag",
    "vessel_name",
    "official_number",
    "departure_date",
    "trip_start_time",
    "arrival_date",
    "start_date",
    "end_date",
    "val_param_yr",
    "val_is_ovr",
    "val_err_type",
    "program_type",
    "arr_year",
    "arr_year_month",
    "overridden",
    "asg_info"
  )

db_data_22_plus <-
  dat_pending_data_22_plus %>%
  select(all_of(fields_to_keep))

## db_data_22_plus to xls ---- 
# do once
# db_data_22_plus %>%
#   # dim()
#   # [1] 48571    18
#   count(val_param_name, arr_year_month) %>%
#   arrange(arr_year_month) %>%
#   pivot_wider(names_from = arr_year_month, values_from = n) %>%
#   as.data.frame() %>%
#   write.xlsx(
#     file.path(
#       my_paths$inputs,
#       "validation_errors",
#       "validation_errors.xlsx"
#     )
#     ,
#     sheetName = "is_enabled, 2022_",
#     row.names = FALSE,
#     append = TRUE
#   )

  # View()

# ====

db_data_22_plus_overr <-
  db_data_22_plus %>%
  count(arr_year_month, val_param_name, overridden)

names(db_data_22_plus_overr)

db_data_22_plus_overr %>%
  pivot_longer(c(val_param_name, overridden)) %>%
  #   dplyr::group_by(n, name, arr_year_month) %>%
  # dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  # dplyr::filter(n > 1L)
  pivot_wider(names_from = arr_year_month,
              values_from = value) %>%
    View()


  # pivot_longer(-c(Species,num,ID)) %>%
  # pivot_wider(names_from = ID,values_from=value)

db_data_22_plus_overr_wide <-
  db_data_22_plus_overr %>%
  pivot_wider(names_from = c(arr_year_month, overridden),
              values_from = n) %>%
  as.data.frame()

# run once
# db_data_22_plus_overr_wide %>%#   
#   write.xlsx(
#       file.path(
#         my_paths$inputs,
#         "validation_errors",
#         "db_data_22_plus_overr.xlsx"
#       ),
#       sheetName = "month_overr",
#       row.names = FALSE,
#       append = TRUE
#     )
# 
  # View()

# plots ----
# iris %>% mutate(sumVar = rowSums(.[1:4]))
# str(db_data_22_plus_overr_wide[,2:23])

db_data_22_plus_overr_wide_tot <-
  db_data_22_plus_overr_wide %>% 
  # add total, exclude param names
  mutate(total_by_param = rowSums(.[2:23], na.rm = TRUE)) %>%
  arrange(desc(total_by_param))

# [1,]
names(db_data_22_plus_overr_wide_tot)

# ggplot(db_data_22_plus_overr_wide_tot,
#        aes(x = Fields, y = Errors))
# facet_grid( ~ Hospital)
# geom_bar(width = 1,
#          stat = "identity",
#          position = "fill")
# coord_polar(theta = "y")

db_data_22_plus_overr_wide_tot1_long <-
  # transpose all columns except param_name and total
  t(db_data_22_plus_overr_wide_tot[1,2:23]) %>%
  as.data.frame() %>%
  set_names("number_of_err") %>%
  # Jan 2022_overridden etc.
  tibble::rownames_to_column("month_overridden")
# %>% View()

#### palette by val_param_name ----
val_err_param_indexes <- sort(unique(db_data_22_plus_overr$val_param_name))
my_colors = length(val_err_param_indexes)
mypalette_params = viridis(my_colors, option = "D")
# mypalette <- rainbow(length(gom_all_cnt_indexes))
names(mypalette_params) <- val_err_param_indexes
mypalette_params

#### palette by month/overridden ----
val_err_month_indexes <- sort(unique(db_data_22_plus_overr_wide_tot1_long$month_overridden))
my_colors = length(val_err_month_indexes)
mypalette_month = viridis(my_colors, option = "D")
# mypalette <- rainbow(length(gom_all_cnt_indexes))
names(mypalette_month) <- val_err_month_indexes
mypalette_month

### 1 pie chart ----
# View(db_data_22_plus_overr)
View(db_data_22_plus_overr_wide_tot1_long)
# df$derma <- factor(df$derma, levels = df$derma)

db_data_22_plus_overr_wide_tot1_long_fact <-
  db_data_22_plus_overr_wide_tot1_long %>%
  # to keep the order
  mutate(month_overridden = factor(month_overridden,
                                   levels = month_overridden))

# replace nas with zeros
db_data_22_plus_overr_wide_tot1_long %<>%
  replace(is.na(.), 0)

pie(db_data_22_plus_overr_wide_tot1_long_fact$number_of_err,
    labels = db_data_22_plus_overr_wide_tot1_long_fact$month_overridden)

ggplot(data = db_data_22_plus_overr_wide_tot1_long_fact,
       aes(
         x = month_overridden,
         y = number_of_err,
         fill = factor(number_of_err)
       )) +
    # scale_fill_manual(values = mypalette_month) +
    geom_col(position = "dodge") +
    labs(title = "Landing location inconsistency with trip",
        # remove x and y axes titles
         x = "",
         y = ""
    ) +
  theme(
    # turn x text
    axis.text.x = element_text(angle = 45)
    # ,
    # change text size
    # plot.title = element_text(size = 9),
    # legend.title = element_text(size = 8),
    # legend.text = element_text(size = 8)
  )
  # geom_bar(stat = "identity", width = 1) +
  # coord_polar("y", start = 0) +
  # theme_void() +

# ggplot(db_data_22_plus_overr_wide_tot[1,], 
#        aes(x = Fields, y = Errors))
# facet_grid( ~ Hospital)
# geom_bar(width = 1,
#          stat = "identity",
#          position = "fill")
# coord_polar(theta = "y")

## num of errors to percents
  # t(db_data_22_plus_overr_wide_tot[1,2:23]) %>%
  # as.data.frame() %>%
  # set_names("number_of_err") %>%
  # # Jan 2022_overridden etc.
  # tibble::rownames_to_column("month_overridden")
my_row <- db_data_22_plus_overr_wide_tot[1,]


a = c("1", "2")
length(a)

get_percent_plot_for_1param <-
  function(my_entry, no_legend = TRUE){
    
    # browser()
    
    # save in variables for future usage
    # save the row number
    all_rows_n <- length(my_entry)
    # save the param name
    val_param_name <- my_entry[1]
    # save the total
    total_by_param <- my_entry[all_rows_n]
    
    transformed_entry <-
      # remove values saved separately, leave only what is needed for a plot
      my_entry[2:(all_rows_n - 1)] %>%
      as.data.frame() %>%
      set_names("number_of_err") %>%
      mutate(number_of_err = as.numeric(number_of_err)) %>%
      mutate(percentage = round(100 * number_of_err / sum(number_of_err),
                                digits = 2))
    
    # View(row_as_col)
    # result example
    # Jul 2022_overridden 1651
    # Jul 2022_pending 0
    
    months_overridden_short <-
      months_overridden[2:(all_rows_n - 1)]
    
    plot_1_param <-
      ggplot(data = transformed_entry,
             aes(
               x = months_overridden_short,
               y = percentage,
               fill = factor(percentage)
             )) +
      geom_col(position = "dodge") +
      labs(title = val_param_name,
           # remove x and y axes titles
           x = "",
           y = "") +
      theme(# turn x text
        axis.text.x = element_text(angle = 45))
    
    # By default the "no_legend" parameter is TRUE
    if (no_legend) {
      plot_1_param <- plot_1_param +
        theme(legend.position = "none")
    }
    
    return(plot_1_param)
  }

db_data_22_plus_overr_wide_tot_transposed <-
  t(db_data_22_plus_overr_wide_tot) %>%
  as.data.frame() %>%
  # # Jan 2022_overridden etc.
  tibble::rownames_to_column("month_overridden") %>%
  replace(is.na(.), 0) %>%
  # preserve the year/month order
  mutate(month_overridden =
           factor(month_overridden,
                  levels = month_overridden))

months_overridden <- db_data_22_plus_overr_wide_tot_transposed$month_overridden

all_plots <-
  db_data_22_plus_overr_wide_tot_transposed %>%
  # use only the val err numbers
  select(-month_overridden) %>%
  map(function(x) {
        get_percent_plot_for_1param(x)
      })

all_plots[2]

