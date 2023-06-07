# get_data_from_param <- "csv"

source("~/R_code_github/quantify_compliance/quantify_compliance_start.R")

# ---- separate SA and GOM permits ----
compl_clean_sa_vs_gom <- separate_permits_into_3_groups(compl_clean)

# View(compl_clean_sa_vs_gom)

# ---- add columns for month and quarter ----
compl_clean_sa_vs_gom_m <-
  compl_clean_sa_vs_gom %>%
  # add month
  mutate(year_month = as.yearmon(week_start)) %>%
  # add quarter
  mutate(year_quarter = as.yearqtr(week_start))

# ---- convert report numbers to numeric ----
compl_clean_sa_vs_gom_m_int <-
  compl_clean_sa_vs_gom_m %>%
  mutate(
    captainreports__ = as.integer(captainreports__),
    negativereports__ = as.integer(negativereports__),
    gom_permitteddeclarations__ = as.integer(gom_permitteddeclarations__)
  )

dim(compl_clean_sa_vs_gom)
# [1] 208893     22

# SA only ----

# at least 1 logbook or no fish report per week
# "No REPORT" err
# How many non-compliant in each week?
# For a given month:
# 100% - the total # of non-compl. vessels
# x%   - submitted 1 week, 2 weeks etc.
# proportion of weeks rep. are missing

# View(compl_clean_sa_vs_gom_m_int)

## get SA only ----
sa_compl_clean_sa_vs_gom_m_int <-
  compl_clean_sa_vs_gom_m_int %>% 
  filter(permit_sa_gom == "sa_only")

dim(sa_compl_clean_sa_vs_gom_m_int)
# 123,453

## SA only non compliant ----
sa_compl_clean_sa_vs_gom_m_int %>% 
  count(compliant_)
#     compliant_     n
# 1 NO         38259
# 2 YES        85194

sa_compl_clean_sa_vs_gom_m_int %>% 
  count(compliant_, overridden_)
#   compliant_ overridden_     n
# 1 NO         NO          33322
# TODO: check if the override comment it is somewhere else
# 2 NO         YES          4937 
# 3 YES        NO          85194

# compl_clean_sa_vs_gom_m_int %>% 
#   count(compliant_, overridden_)

sa_compl_clean_sa_vs_gom_m_int_non_c <-
  sa_compl_clean_sa_vs_gom_m_int %>% 
  filter(compliant_ == "NO")

### test one month ----
test_one_month <- function() {
  non_compliant_dec_2022 <-
    compl_clean_sa_vs_gom_m_int %>%
    filter(compliant_ == "NO") %>%
    filter(year_month == "Dec 2022")
  
  #### check how many unique vessel_official_numbers ----
  
  non_compliant_dec_2022_vessel_num <-
    non_compliant_dec_2022 %>%
    select(vessel_official_number) %>% unique() %>% dim()
  # [1] 467   1
  
  non_compliant_dec_2022_count_nc_weeks_per_vessel <-
    non_compliant_dec_2022 %>%
    count(vessel_official_number, name = "id_n") %>%
    # how many non_compliant this month
    count(id_n, name = "non_compl_weeks_in_month")
  #    id_n non_compl_weeks_in_month
  # 1     1                       25
  # 2     2                       25
  # 3     3                       38
  # 4     4                      379
  
  #### test if the number of unique vessel official numbers is equal to the sum of vessels in the non_compl_weeks_in_month categories
  sum(non_compliant_dec_2022_count_nc_weeks_per_vessel$non_compl_weeks_in_month) ==
    non_compliant_dec_2022_vessel_num[1]
  # TRUE
  
  #### check a vessel (a vessel in "1" is not anywhere else) ----
  non_compliant_dec_2022 %>%
    count(vessel_official_number, name = "id_n") %>%
    filter(vessel_official_number == "MI1381CC")
  # filter(id_n == 1)
}

# get percentage ----
# View(sa_compl_clean_sa_vs_gom_m_int_non_c)
sa_compl_clean_sa_vs_gom_m_int_non_c_perc_0 <-
  get_non_compl_week_counts_percent(sa_compl_clean_sa_vs_gom_m_int_non_c,
                              "vessel_official_number")

sa_compl_clean_sa_vs_gom_m_int_non_c_perc <-
  sa_compl_clean_sa_vs_gom_m_int_non_c_perc_0 %>% 
  # don't show if no data
  filter(non_compl_in_month > 0)

View(sa_compl_clean_sa_vs_gom_m_int_non_c_perc)

## SA only plots ----
### one plot ----
# gg_22_01 <- 
  # sa_compl_clean_sa_vs_gom_m_int_non_c_perc %>% 
  # filter(year_month == "Jan 2022") %>%
  # ggplot(aes(non_compl_weeks, percent_nc)) +
  # geom_col()

gg_sa_compl_clean_sa_vs_gom_m_int_non_c_perc <-
  unique(sa_compl_clean_sa_vs_gom_m_int_non_c_perc$year_month) |>
  map(
    \(current_year_month)
    perc_plots_by_month(
      sa_compl_clean_sa_vs_gom_m_int_non_c_perc,
      current_year_month
    )
  )

# one plot
# gg_sa_compl_clean_sa_vs_gom_m_int_non_c_perc[[12]]

super_title = "SA only from csvs: how many weeks vessels were non_compliant"

grid.arrange(grobs = gg_sa_compl_clean_sa_vs_gom_m_int_non_c_perc,
             top = super_title,
             # left = my_legend,
             ncol = 4)

## SA non compliant and not overridden ----
sa_compl_clean_sa_vs_gom_m_int_nc_no <-
  sa_compl_clean_sa_vs_gom_m_int %>% 
  filter(compliant_ == "NO" & overridden_ == "NO")

sa_compl_clean_sa_vs_gom_m_int_nc_no_perc <-
  get_non_compl_week_counts_percent(sa_compl_clean_sa_vs_gom_m_int_nc_no,                                    "vessel_official_number")
# View(sa_compl_clean_sa_vs_gom_m_int_nc_no_perc)

gg_sa_compl_clean_sa_vs_gom_m_int_nc_no_perc <-
  unique(sa_compl_clean_sa_vs_gom_m_int_nc_no_perc$year_month) |>
  map(
    \(current_year_month)
    perc_plots_by_month(
      sa_compl_clean_sa_vs_gom_m_int_nc_no_perc,
      current_year_month
    )
  )

# one plot
gg_sa_compl_clean_sa_vs_gom_m_int_nc_no_perc[[12]]

super_title = "SA only from csvs: how many weeks vessels were non_compliant and not overridden"

grid.arrange(grobs = gg_sa_compl_clean_sa_vs_gom_m_int_nc_no_perc,
             top = super_title,
             # left = my_legend,
             ncol = 4)

## Same using only report counts without "compliant__" ----

# GOM + dual ----
gom_all_compl_clean_sa_vs_gom_m_int <-
  compl_clean_sa_vs_gom_m_int %>% 
  filter(!(permit_sa_gom == "sa_only"))

dim(gom_all_compl_clean_sa_vs_gom_m_int)
# [1] 85440    24

gom_all_compl_clean_sa_vs_gom_m_int_even <-
  gom_all_compl_clean_sa_vs_gom_m_int %>%
  mutate(
    logb_n_decl = captainreports__ + gom_permitteddeclarations__,
    even_num_rep = dplyr::if_else(((logb_n_decl %% 2) == 0),
                                  "even", "odd")
  )
# View(gom_all_compl_clean_sa_vs_gom_m_int_even)

gom_all_compl_clean_sa_vs_gom_m_int_even %>% 
  count(even_num_rep, compliant_)
#   even_num_rep compliant_     n
#   <chr>        <chr>      <int>
# 1 even         NO          3542 ?
# 2 even         YES        76908
# 3 odd          NO           373
# 4 odd          YES         4617 ?

## investigate  odd/YES ----
# gom_all_compl_clean_sa_vs_gom_m_int_even %>% 
#   filter(compliant_ == "YES" & even_num_rep == "odd") %>% View()

gom_all_compl_clean_sa_vs_gom_m_int_even %>% 
  filter(compliant_ == "YES" & even_num_rep == "odd") %>% head(1) %>% glimpse()
# $ vessel_official_number      <chr> "TX3416RA"
# $ name                        <chr> "CONTROLLED CHAOS"
# $ permitgroup                 <chr> "(CHG)885,(RCG)836"
# $ year_month                  <yearmon> Dec 2022
# $ week_num                    <int> 52
# $ week                        <chr> "52: 12/26/2022 - 01/01/2023"
# $ week_start                  <date> 2022-12-26
# $ week_end                    <date> 2023-01-01


## GOM by "compliant?" ----
gom_all_compl_clean_sa_vs_gom_m_int_non_comp <-
  gom_all_compl_clean_sa_vs_gom_m_int %>% 
  filter(compliant_ == "NO")

# gom_all_compl_clean_sa_vs_gom_m_int_non_comp_perc <-
#   get_non_compl_week_counts(gom_all_compl_clean_sa_vs_gom_m_int_non_comp)
# numbers are too low

# GOM + dual non compl ----

 # e.comp_error_type_cd = 'DECL_NO_TRIP'
 # AND TRUNC(SYSDATE) > TRUNC(tn.trip_start_date)
 # )
 # OR -- Past activity date
 # (
 # e.comp_error_type_cd = 'TRIP_NO_DECL'

 # AND TRUNC(SYSDATE) < (TRUNC(c.comp_week_end_dt) + 14) THEN -- Not past allotted correction time

# SA:
#    TRUNC(SYSDATE) >= (TRUNC(c.comp_week_end_dt) + 24) THEN -- Past allotted correction time

# select * from srh.srfh_for_hire_type@secapxdv_dblk.sfsc.noaa.gov
# 1	HEADBOAT
# 2	CHARTER
# 42	RECREATIONAL
# 81	COMMERCIAL
# 62	UNKNOWN

## get not SA only ----
gom_d_compl_clean_sa_vs_gom_m_int <-
  compl_clean_sa_vs_gom_m_int %>% 
  filter(!(permit_sa_gom == "sa_only"))

dim(gom_d_compl_clean_sa_vs_gom_m_int)
# [1] 85440    24

## Gom and dual non compliant ----
gom_d_compl_clean_sa_vs_gom_m_int_nc <-
  gom_d_compl_clean_sa_vs_gom_m_int %>% 
  filter(toupper(compliant_) == "NO")

dim(gom_d_compl_clean_sa_vs_gom_m_int_nc)
# [1] 3915   24

## Gom and dual non compliant and not overridden----
gom_d_compl_clean_sa_vs_gom_m_int_nc_no <-
  gom_d_compl_clean_sa_vs_gom_m_int %>% 
  filter(toupper(compliant_) == "NO" &
           toupper(overridden_) == "NO")

dim(gom_d_compl_clean_sa_vs_gom_m_int_nc_no)
# [1] 2935   24

gom_d_compl_clean_sa_vs_gom_m_int_nc_no_22 <-
  gom_d_compl_clean_sa_vs_gom_m_int_nc_no %>%
  filter(year == '2022')

# TODO 2023 separately for "both" permits

## get percentage of nc weeks ----
perc_gom_d_compl_clean_sa_vs_gom_m_int_nc_no_22 <-
  get_non_compl_week_counts_percent(gom_d_compl_clean_sa_vs_gom_m_int_nc_no_22,
                                    "vessel_official_number") %>% 
  filter(non_compl_in_month > 0)

View(perc_gom_d_compl_clean_sa_vs_gom_m_int_nc_no_22)

## GOM + dual 22 csv plots ----
### one plot
# gg_22_01_gom <- 
  # perc_gom_d_compl_clean_sa_vs_gom_m_int_nc_no_22 %>%
  # filter(year_month == "Jan 2022") %>%
  # ggplot(aes(non_compl_weeks, percent_nc)) +
  # geom_col()

gg_perc_gom_d_compl_clean_sa_vs_gom_m_int_nc_no_22 <-
  unique(perc_gom_d_compl_clean_sa_vs_gom_m_int_nc_no_22$year_month) |>
  map(
    \(current_year_month)
    perc_plots_by_month(
      perc_gom_d_compl_clean_sa_vs_gom_m_int_nc_no_22,
      current_year_month
    )
  )

# one plot
# gg_perc_gom_d_compl_clean_sa_vs_gom_m_int_nc_no_22[[12]]

super_title = "GOM & dual from csvs: how many weeks vessels were non_compliant"
grid.arrange(grobs =
               gg_perc_gom_d_compl_clean_sa_vs_gom_m_int_nc_no_22,
             top = super_title,
             ncol = 4)

# percent of non_compliant and compliant ----
compl_clean_sa_vs_gom_m_int %>% count(permit_sa_gom, compliant_)
#   permit_sa_gom compliant_     n
# 1 both          NO          3383
# 2 both          YES        19770
# 3 gom_only      NO           532
# 4 gom_only      YES        61755
# 5 sa_only       NO         38259
# 6 sa_only       YES        85194

compl_vs_non_compl_per_year_cnt <-
  compl_clean_sa_vs_gom_m_int %>% count(permit_sa_gom, compliant_, year, name = "cnt_compl_per_perm_year")

glimpse(compl_vs_non_compl_per_year_cnt)

total_per_permit_year <-
  compl_vs_non_compl_per_year_cnt %>%
  count(permit_sa_gom,
        year,
        wt = cnt_compl_per_perm_year,
        name = "total_compl_per_perm_year")

counts_join <-
  full_join(compl_vs_non_compl_per_year_cnt,
            total_per_permit_year)
# Joining with `by = join_by(permit_sa_gom, year)`

### Percent compliant per year and permit region ----
compl_percent_per_permit_year <-
  counts_join %>%
  mutate(percent_compl = cnt_compl_per_perm_year * 100 / total_compl_per_perm_year)

# View(compl_percent_per_permit_year)

## plot percent compl vs. non-compl ----

compl_percent_per_permit_year %>%
  filter(year == "2022") %>%
  ggplot(aes(x = "",
             # y = percent_compl,
             y = "",
             fill = compliant_)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  scale_fill_discrete(name = "Compliant")

compl_pie_chart <- function(my_df, y_p_title) {
  ggplot(my_df,
         aes(x = "",
             y = percent_compl,
             fill = compliant_)) +
    geom_bar(width = 1, stat = "identity") +
    coord_polar("y", start = 0) +
          labs(title = y_p_title,
           x = "",
           # x = "Compliant",
           y = "") +
    geom_text(aes(label = paste0(round(percent_compl, 1), "%")),
              position = position_stack(vjust = 0.5)) +
    scale_fill_discrete(name = "Compliant") %>%
    return()
}

## split by year_permit_region without nesting ----
compl_percent_per_permit_year_1col <-
  compl_percent_per_permit_year %>%
  # compute on a data frame a row-at-a-time
  dplyr::rowwise() %>%
  mutate(year_reg = 
           paste(year, permit_sa_gom)) %>% 
  # return to the default colwise operations
  dplyr::ungroup()
  
compl_percent_per_permit_year_spl1 <-
  compl_percent_per_permit_year_1col %>% 
  split(as.factor(compl_percent_per_permit_year_1col$year_reg))

# View(compl_percent_per_permit_year_spl1)

## make all plots ----
all_gg_compl_percent_per_permit_year_spl <-
  names(compl_percent_per_permit_year_spl1) %>%
  map(function(year_region) {
    # browser()
    total_non_compl_df <-
      compl_percent_per_permit_year_spl1[[year_region]] %>% 
      filter(compliant_ == "NO") %>% 
      select(cnt_compl_per_perm_year)
    
    y_p_title <- 
      paste(year_region, total_non_compl_df[[1]], "non compl")
    # y_p_title <- year_region
    data_by_year <-
      compl_percent_per_permit_year_spl1[[year_region]] %>%
      compl_pie_chart(y_p_title)
  })
# purrr::map_df

super_title = "Percent compliant per year and permit region"

grid.arrange(grobs = all_gg_compl_percent_per_permit_year_spl,
             top = super_title,
             # left = my_legend,
             ncol = 3)

# Percent nc weeks per year and region ----
# df <- data %>% 
#   group_by(answer) %>% # Variable to be transformed
#   count() %>% 
#   ungroup() %>% 
#   mutate(perc = `n` / sum(`n`)) %>% 
#   arrange(perc) %>%
#   mutate(labels = scales::percent(perc))

count_weeks_per_permit_year <-
  compl_clean_sa_vs_gom_m_int %>%
    filter(compliant_ == "NO") %>%
    count(vessel_official_number, year, permit_sa_gom,
        name = "nc_weeks_per_vsl")

# View(count_weeks_per_permit_year)

non_compl_weeks_per_year <-
  count_weeks_per_permit_year %>%
  count(year, permit_sa_gom, nc_weeks_per_vsl,
        name = "nc_weeks_cnt") %>% 
  arrange(nc_weeks_per_vsl)
  
# View(non_compl_weeks_per_year)

perc_non_compl_weeks_per_year <-
  non_compl_weeks_per_year %>%
  group_by(year, permit_sa_gom) %>%
  mutate(sum_nc_weeks_cnt_per_year_reg = sum(`nc_weeks_cnt`)) %>%
  mutate(perc = nc_weeks_cnt * 100 / sum(nc_weeks_cnt)) %>%
  arrange(perc) %>%
  mutate(perc_labels = paste0(round(perc, 1), "%"))

View(perc_non_compl_weeks_per_year)

### test one category ----
non_compl_weeks_per_year_22_sa <-
  non_compl_weeks_per_year %>% 
  filter(year == "2022", permit_sa_gom == "sa_only")

# str(non_compl_weeks_per_year_22_sa)

non_compl_weeks_per_year_22_sa %>% count(wt = nc_weeks_cnt)
# 1   117 (22 both)
# 1289 (22 sa)

View(non_compl_weeks_per_year_22_sa)

non_compl_weeks_per_year_22_sa %>%
  mutate(nc_cnt = nc_weeks_per_vsl * nc_weeks_cnt) %>%
  count(wt = nc_cnt, name = "total_nc_22_sa_per_year") %>%
  glimpse()
# 26466 - correct, see "Percent compliant per year and permit region"

non_compl_weeks_per_year_22_sa %>% summarise(sum(nc_weeks_cnt))
# 1289 (22 sa)

non_compl_weeks_per_year_22_sa_perc <-
  non_compl_weeks_per_year_22_sa %>%
  mutate(perc = nc_weeks_cnt * 100 / sum(nc_weeks_cnt)) %>%
  arrange(desc(perc)) %>%
  mutate(perc_labels = paste0(round(perc, 1), "%"))

View(non_compl_weeks_per_year_22_sa_perc)
#   `sum(nc_weeks_per_vsl)`
#                     <int>
# 1                    1378

# ggplot(df, aes(x = "", y = perc, fill = answer)) +
#   geom_col() +
#   geom_text(aes(label = labels),
#             position = position_stack(vjust = 0.5)) +
#   
# okabe <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# mutate(dates_index = as.factor(year_wave),
#     year_wave = factor(year_wave, levels = unique(dates_index)) %>%
#   ggplot(aes(x = year_wave,
#            y = cnt_index) +
#          geom_point())

current_title <- "Percentage of non compliant weeks per year for SA 2022"

non_compl_weeks_per_year_22_sa_perc %>%
  mutate(nc_weeks_per_vsl_order = 
           fct_reorder(as.factor(nc_weeks_per_vsl),
                       perc)) %>% 
  # glimpse()
  ggplot(aes(x = nc_weeks_per_vsl_order, y = perc)) +
  geom_col(fill = 'lightblue') +
  # ylim(0, 100) +
  geom_text(aes(label = perc_labels),
            position = position_stack(vjust = 1),
            size = 3) +
  theme(plot.title = element_text(size = 10),
        axis.title = element_text(size = 9)) +
  labs(title = current_title,
       # x = "",
       x = "Num of nc weeks",
       y = "Percent of num of nc weeks of all nc weeks SA 2022")

# +
#   scale_fill_manual(okabe)
# withr::with_options(
#   list(ggplot2.discrete.fill = okabe),


## get percents ---- 
non_compl_weeks_per_year %>%
  group_by(year, permit_sa_gom) %>%
  mutate(sum_nc_weeks_cnt_per_year_reg = sum(`nc_weeks_cnt`)) %>%
  # mutate(perc = `nc_weeks_cnt` / sum(`nc_weeks_cnt`)) %>%
  mutate(total_by_year_reg = sum(sum_nc_weeks_cnt_per_year_reg)) %>%
  head(1) %>% 
  glimpse()
# 2022  both 3% from 
# 117*100/3627
# [1] 3.225806

# Rows: 1
# Columns: 6
# Groups: year, permit_sa_gom [1]
# $ year                          <chr> "2022"
# $ permit_sa_gom                 <chr> "both"
# $ nc_weeks_per_vsl              <int> 1
# $ nc_weeks_cnt                  <int> 27
# $ sum_nc_weeks_cnt_per_year_reg <int> 117
# $ total_by_year_reg             <int> 3627
# 27*100/117
# [1] 23.07692
# 2022 - both - 1 w nc - 23%

# # A tibble: 6 × 3
# # Groups:   year [2]
#   year  permit_sa_gom `sum(sum_nc_weeks_cnt_per_year_reg)`
#   <chr> <chr>                                        <int>
# 1 2022  both                                          3627
# 2 2022  gom_only                                      2805
# 3 2022  sa_only                                      67028
# 4 2023  both                                          4880
# 5 2023  gom_only                                         6
# 6 2023  sa_only                                      29064