library(data.table)
# install.packages("xlsx")
library(xlsx)
library(viridis)
library(gridExtra)
library(grid)

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
    # columns are side by side (not stacked)
    geom_col(position = "dodge") +
    labs(title = "Landing location inconsistency with trip",
        # remove x and y axes titles
         x = "",
         y = ""
    ) +
  theme(
    # turn x text
    axis.text.x = element_text(angle = 45),
    # change text size
    plot.title = element_text(size = 9),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 8)
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
my_row <- db_data_22_plus_overr_wide_tot[1, ]

# a = c("1", "2")
# length(a)

# margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")
my_theme_narrow <-
  # theme_bw() +
  theme_classic() +
  theme(# plot.margin = unit(rep(.5, 4), "lines")
    # plot.margin = margin(rep(0, 4))
    # plot.margin = unit(c(5, 0, -30, -10), "pt")
    plot.margin = unit(c(0, 0, 0,-5), "pt")) 
# +
  # theme(axis.text.x = element_text(vjust = -12))

# ---- theme with transparent background ----
# theme_transparent <- theme(
#   legend.background = element_rect(fill = "transparent"),
#   legend.title = element_blank(),
#   #no legend background color or title
#   legend.box.background = element_rect(fill = "transparent", colour = NA),
#   #no fill color
#   legend.key = element_rect(fill = "transparent"),
#   #no fill color
#   legend.spacing = unit(-1, "lines")
# )

get_plot_for_1param <-
  function(my_entry, no_legend = TRUE, percent = TRUE) {
    browser()
    
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
      mutate(number_of_err = as.numeric(number_of_err)) 
    
    if (percent) {
      transformed_entry <- transformed_entry %>%
        mutate(y_values =
                 round(100 * number_of_err / sum(number_of_err),
                       digits = 0))
    } else {
      transformed_entry <- transformed_entry %>%
        mutate(y_values = number_of_err)
    }
    
    plot_1_param <-
      ggplot(data = transformed_entry,
             aes(
               x = months_overridden_short,
               y = y_values,
               fill = factor(y_values)
             )) +
      geom_col(position = "dodge") +
      geom_text(aes(label = y_values)) +
      labs(title = val_param_name,
           # remove x and y axes titles
           x = "",
           y = "") +
      my_theme_narrow +
      theme(
        # turn x text and change text size
        axis.text.x = element_text(angle = 45,
                                   size = 8,
                                   # lower text
                                   hjust = 1),
        axis.text.y = element_text(size = 7),
        plot.title = element_text(size = 8)
        # ,
        # legend.title = element_text(size = 8),
        # legend.text = element_text(size = 8)
      )
    # +
    #   ylim(0, 70)
    
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
  # remove repeating parts
  mutate(
    month_overridden_short_name =
      gsub(
        month_overridden,
        pattern = "(.+) \\d\\d(\\d\\d)_(.).+",
        replacement = "\\1_\\2_\\U\\3",
        perl = TRUE
      )
  ) %>%
  mutate(
    month_overridden_short_name =
      str_replace(month_overridden_short_name,
                  "NA_overridden",
                  "NA_O")
  ) %>%
  # preserve the year/month order
  mutate(
    month_overridden_short_name =
      factor(month_overridden_short_name,
             levels = month_overridden_short_name)
  )

# prepare month names for plots, remove the first and last values

prepare_month_names_for_plots <- function(my_df, month_col_name) {
  # browser()
  tot_length <-
    dim(my_df[month_col_name])[1]
  
  months_short <-
    my_df[month_col_name][2:(tot_length - 1), 1]
  
  return(months_short)
}

# a <- "month_overridden_short_name"
# db_data_22_plus_overr_wide_tot_transposed[a]

db_data_22_plus_overr_wide_tot_transposed$month_overridden_short_name
mm <- prepare_month_names_for_plots(db_data_22_plus_overr_wide_tot_transposed, "month_overridden_short_name")

identical(mm, months_overridden_short)
# T
# use only the val err numbers
db_data_22_plus_overr_wide_tot_transposed_short <-
  db_data_22_plus_overr_wide_tot_transposed %>%
  select(-all_of(starts_with("month_overridden")))

# names(db_data_22_plus_overr_wide_tot_transposed_short)
all_plots_p <-
  map(db_data_22_plus_overr_wide_tot_transposed_short,
      get_plot_for_1param)

# all_plots[1]

# combine plots ----

#### separate a legend ----
# only if creating a list of all percent values
# plot_w_legend <- get_percent_plot_for_1param(db_data_22_plus_overr_wide_tot_transposed_short[,1],
#                                              # keep the legend
#                                              FALSE)
# use an aux function to pull out the legend
# my_legend <- legend_for_grid_arrange(plot_w_legend)
super_title_p = "Percentage of Validation Errors by Month and Overridden or Pending"

super_title_n = "Number of Validation Errors by Month and Overridden or Pending"

## footnote with an explanation ----
footnote_text_p = "The Percentage calculated for each validation error independently, as 100 * number_of_err / sum(number_of_err). Plots are aranged by sum(number_of_err)."
footnote_text_n = "Plots are aranged by sum(number_of_err)." 

footnote <- function(footnote_text) {
  textGrob(
    footnote_text,
    gp = gpar(fontface = 3, fontsize = 10),
    # justify left
    hjust = 0,
    x = 0.01,
    y = 0.99,
    vjust = 1
  ) %>%
    return()
}

# combine all plots ----
grid.arrange(
  grobs = all_plots_p,
  top = super_title_p,
  bottom = footnote(footnote_text_p),
  # left = my_legend,
  ncol = 4
)

# plots by numbers ----

all_plots_n <-
  map(db_data_22_plus_overr_wide_tot_transposed_short,
      function(x) {
        get_plot_for_1param(x, no_legend = TRUE, percent = FALSE)
      })

# all_plots_n[[1]]

# combine all number plots ----
grid.arrange(
  grobs = all_plots_n,
  top = super_title_n,
  bottom = footnote(footnote_text_n),
  ncol = 4
)

# separate overridden and pending ----
# View(db_data_22_plus_overr)
db_data_22_plus_sep <-
db_data_22_plus_overr %>%
  split(db_data_22_plus_overr$overridden)

# View(db_data_22_plus_sep$overridden)
# 'data.frame':	215 obs. of  4 variables:

transform_to_plot <- function(my_df) {
  my_df %>%
    # short format for months
    mutate(arr_year_month =
             format(arr_year_month, "%m %y")) %>%
    select(-overridden) %>%
    pivot_wider(names_from = c(arr_year_month),
                values_from = n) %>%
    # count total by row in all columns except param names
    mutate(total_by_param = rowSums(.[2:dim(.)[2]], na.rm = TRUE)) %>%
    # sort
    arrange(desc(total_by_param)) %>%
    # transpose
    t() %>%
    as.data.frame() %>%
    # NAs to zeros
    replace(is.na(.), 0) %>%
    # add a column with rownames
    tibble::rownames_to_column("month") %>%
    # keep the order
    mutate(month = factor(month,
                          levels = month)) %>%
    return()
}
db_data_22_plus_overr_only_wide <-
    transform_to_plot(db_data_22_plus_sep$overridden)

View(db_data_22_plus_overr_only_wide)

# plots by numbers for overridden and pending ----

all_plots_n_overridden <-
  map(db_data_22_plus_sep$overridden,
      function(x) {
        get_plot_for_1param(x, no_legend = TRUE, percent = FALSE)
      })

all_plots_n_overridden[[1]]

# combine all number plots ----
grid.arrange(
  grobs = all_plots_n,
  top = super_title_n,
  bottom = footnote(footnote_text_n),
  ncol = 4
)
