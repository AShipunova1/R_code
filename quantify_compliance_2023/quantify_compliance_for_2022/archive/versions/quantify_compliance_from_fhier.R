# get_data_from_param <- "csv"

source("~/R_code_github/quantify_compliance/quantify_compliance_start.R")

# ---- separate SA and GOM permits ----
compl_clean_sa_vs_gom <- separate_permits_into_3_groups(compl_clean)

# View(compl_clean_sa_vs_gom)

# ---- add columns for month and quarter ----
compl_clean_sa_vs_gom_m <-
  compl_clean_sa_vs_gom %>%
  # add month
  dplyr::mutate(year_month = as.yearmon(week_start)) %>%
  # add quarter
  dplyr::mutate(year_quarter = as.yearqtr(week_start))

# ---- convert report numbers to numeric ----
compl_clean_sa_vs_gom_m_int <-
  compl_clean_sa_vs_gom_m %>%
  dplyr::mutate(
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
  dplyr::count(compliant_)
#     compliant_     n
# 1 NO         38259
# 2 YES        85194

sa_compl_clean_sa_vs_gom_m_int %>% 
  dplyr::count(compliant_, overridden_)
#   compliant_ overridden_     n
# 1 NO         NO          33322
# TODO: check if the override comment it is somewhere else
# 2 NO         YES          4937 
# 3 YES        NO          85194

# compl_clean_sa_vs_gom_m_int %>% 
#   dplyr::count(compliant_, overridden_)

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
    dplyr::count(vessel_official_number, name = "id_n") %>%
    # how many non_compliant this month
    dplyr::count(id_n, name = "non_compl_weeks_in_month")
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
    dplyr::count(vessel_official_number, name = "id_n") %>%
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

# View(sa_compl_clean_sa_vs_gom_m_int_non_c_perc)

## SA only plots ----
### one plot ----
# gg_22_01 <- 
  # sa_compl_clean_sa_vs_gom_m_int_non_c_perc %>% 
  # filter(year_month == "Jan 2022") %>%
  # ggplot(aes(non_compl_weeks, percent_nc)) +
  # geom_col()

gg_sa_compl_clean_sa_vs_gom_m_int_non_c_perc <-
  unique(sa_compl_clean_sa_vs_gom_m_int_non_c_perc$year_month) |>
  purrr::map(
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
  purrr::map(
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
  dplyr::mutate(
    logb_n_decl = captainreports__ + gom_permitteddeclarations__,
    even_num_rep = dplyr::if_else(((logb_n_decl %% 2) == 0),
                                  "even", "odd")
  )
# View(gom_all_compl_clean_sa_vs_gom_m_int_even)

gom_all_compl_clean_sa_vs_gom_m_int_even %>% 
  dplyr::count(even_num_rep, compliant_)
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
  filter(compliant_ == "YES" & even_num_rep == "odd") %>% head(1) %>% dplyr::glimpse()
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

# View(perc_gom_d_compl_clean_sa_vs_gom_m_int_nc_no_22)

## GOM + dual 22 csv plots ----
### one plot
# gg_22_01_gom <- 
  # perc_gom_d_compl_clean_sa_vs_gom_m_int_nc_no_22 %>%
  # filter(year_month == "Jan 2022") %>%
  # ggplot(aes(non_compl_weeks, percent_nc)) +
  # geom_col()

gg_perc_gom_d_compl_clean_sa_vs_gom_m_int_nc_no_22 <-
  unique(perc_gom_d_compl_clean_sa_vs_gom_m_int_nc_no_22$year_month) |>
  purrr::map(
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
compl_clean_sa_vs_gom_m_int %>% dplyr::count(permit_sa_gom, compliant_)
#   permit_sa_gom compliant_     n
# 1 both          NO          3383
# 2 both          YES        19770
# 3 gom_only      NO           532
# 4 gom_only      YES        61755
# 5 sa_only       NO         38259
# 6 sa_only       YES        85194

compl_vs_non_compl_per_year_cnt <-
  compl_clean_sa_vs_gom_m_int %>% 
  dplyr::count(permit_sa_gom, compliant_, year, name = "cnt_compl_per_perm_year")

glimpse(compl_vs_non_compl_per_year_cnt)

total_per_permit_year <-
  compl_vs_non_compl_per_year_cnt %>%
  dplyr::count(permit_sa_gom,
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
  dplyr::mutate(percent_compl = cnt_compl_per_perm_year * 100 / total_compl_per_perm_year)

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
  dplyr::mutate(year_reg = 
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
  purrr::map(function(year_region) {
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
#   dplyr::group_by(answer) %>% # Variable to be transformed
#   dplyr::count() %>% 
#   dplyr::ungroup() %>% 
#   dplyr::mutate(perc = `n` / sum(`n`)) %>% 
#   dplyr::arrange(perc) %>%
#   dplyr::mutate(labels = scales::percent(perc))

count_weeks_per_permit_year <-
  compl_clean_sa_vs_gom_m_int %>%
    filter(compliant_ == "NO") %>%
    dplyr::count(vessel_official_number, year, permit_sa_gom,
        name = "nc_weeks_per_vsl")

# View(count_weeks_per_permit_year)

non_compl_weeks_per_year <-
  count_weeks_per_permit_year %>%
  dplyr::count(year, permit_sa_gom, nc_weeks_per_vsl,
        name = "nc_weeks_cnt") %>% 
  dplyr::arrange(nc_weeks_per_vsl)
  
# View(non_compl_weeks_per_year)

perc_non_compl_weeks_per_year <-
  non_compl_weeks_per_year %>%
  dplyr::group_by(year, permit_sa_gom) %>%
  dplyr::mutate(sum_nc_weeks_cnt_per_year_reg = sum(`nc_weeks_cnt`)) %>%
  dplyr::mutate(perc = nc_weeks_cnt * 100 / sum(nc_weeks_cnt)) %>%
  dplyr::arrange(perc) %>%
  dplyr::mutate(perc_labels = paste0(round(perc, 1), "%"))

# View(perc_non_compl_weeks_per_year)

### test one category ----
count_weeks_per_permit_year %>% filter(year == "2022", permit_sa_gom == "sa_only", nc_weeks_per_vsl == 31) %>% dim()
# 15
# MI9152BZ

compl_clean %>% 
  filter(vessel_official_number == "MI9152BZ",
         year == "2022") %>% 
  dplyr::count(compliant_)
# 1 NO            31
# 31

non_compl_weeks_per_year_22_sa <-
  non_compl_weeks_per_year %>% 
  filter(year == "2022", permit_sa_gom == "sa_only")

# str(non_compl_weeks_per_year_22_sa)

non_compl_weeks_per_year_22_sa %>% dplyr::count(wt = nc_weeks_cnt)
# 1   117 (22 both)
# 1289 (22 sa)

# View(non_compl_weeks_per_year_22_sa)

non_compl_weeks_per_year_22_sa %>%
  dplyr::mutate(nc_cnt = nc_weeks_per_vsl * nc_weeks_cnt) %>%
  dplyr::count(wt = nc_cnt, name = "total_nc_22_sa_per_year") %>%
  dplyr::glimpse()
# 26466 - correct, see "Percent compliant per year and permit region"

non_compl_weeks_per_year_22_sa %>% summarise(sum(nc_weeks_cnt))
# 1289 (22 sa)

# View(non_compl_weeks_per_year_22_sa)

non_compl_weeks_per_year_22_sa_perc <-
  non_compl_weeks_per_year_22_sa %>%
  dplyr::mutate(perc = nc_weeks_cnt * 100 / sum(nc_weeks_cnt)) %>%
  dplyr::arrange(desc(perc)) %>%
  dplyr::mutate(perc_labels = paste0(round(perc, 1), "%"))

# View(non_compl_weeks_per_year_22_sa_perc)
#   `sum(nc_weeks_per_vsl)`
#                     <int>
# 1                    1378

# ggplot(df, aes(x = "", y = perc, fill = answer)) +
#   geom_col() +
#   geom_text(aes(label = labels),
#             position = position_stack(vjust = 0.5)) +
#   
# okabe <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# dplyr::mutate(dates_index = as.factor(year_wave),
#     year_wave = factor(year_wave, levels = unique(dates_index)) %>%
#   ggplot(aes(x = year_wave,
#            y = cnt_index) +
#          geom_point())

current_title <- "Percentage of non compliant weeks per year for SA 2022"

non_compl_weeks_per_year_22_sa_perc %>%
  dplyr::mutate(nc_weeks_per_vsl_order = 
           fct_reorder(as.factor(nc_weeks_per_vsl),
                       perc)) %>% 
  # dplyr::glimpse()
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
  dplyr::group_by(year, permit_sa_gom) %>%
  dplyr::mutate(sum_nc_weeks_cnt_per_year_reg = sum(`nc_weeks_cnt`)) %>%
  # dplyr::mutate(perc = `nc_weeks_cnt` / sum(`nc_weeks_cnt`)) %>%
  dplyr::mutate(total_by_year_reg = sum(sum_nc_weeks_cnt_per_year_reg)) %>%
  head(1) %>% 
  dplyr::glimpse()
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

# percent should be by total entries for each vessel ----
## per year ----

# count_weeks_per_permit_year %>% filter(year == "2022", permit_sa_gom == "sa_only", nc_weeks_per_vsl == 31) %>% dim()
# # 15
# # MI9152BZ
# 
# compl_clean %>% 
#   filter(vessel_official_number == "MI9152BZ",
#          year == "2022") %>% 
#   dplyr::count(compliant_)

compl_clean_sa_vs_gom_m_int_cnt_w <-
  compl_clean_sa_vs_gom_m_int %>% 
  dplyr::count(year, permit_sa_gom, vessel_official_number, compliant_, name = "weeks_per_vessel")

compl_clean_sa_vs_gom_m_int_cnt_w %>% 
    filter(vessel_official_number == "MI9152BZ",
         year == "2022") %>%
  dplyr::glimpse()
# $ year                   <chr> "2022"
# $ permit_sa_gom          <chr> "sa_only"
# $ vessel_official_number <chr> "MI9152BZ"
# $ compliant_             <chr> "NO"
# $ weeks_per_vessel       <int> 31

### test one category ----
# compl_clean_sa_vs_gom_m_int_cnt_w %>%
#   filter(year == "2022", permit_sa_gom == "sa_only", weeks_per_vessel == 31) %>% dplyr::glimpse()
# # 36

compl_clean_sa_vs_gom_m_int_cnt_w %>%
  filter(vessel_official_number == "1020822",
         year == "2022") %>% dplyr::glimpse()
# $ compliant_             <chr> "NO", "YES"
# $ weeks_per_vessel       <int> 19, 33

compl_clean %>%
  filter(vessel_official_number == "1020822",
         year == "2022") %>% dim()
# 52 == 33 + 19

# 19*100/52 = 36.53846 % non compl

compl_clean_sa_vs_gom_m_int_cnt_w1 <-
  compl_clean_sa_vs_gom_m_int %>%
  dplyr::add_count(year, permit_sa_gom, vessel_official_number, compliant_, name = "weeks_per_vessel_per_compl") %>%
  dplyr::add_count(year, permit_sa_gom, vessel_official_number, name = "total_weeks_per_vessel")

compl_clean_sa_vs_gom_m_int_cnt_w1 %>%
  filter(vessel_official_number == "1020822",
         year == "2022") %>%
  select(compliant_,
         weeks_per_vessel_per_compl,
         total_weeks_per_vessel) %>%
  unique() %>%
  dplyr::glimpse()
# $ compliant_                 <chr> "YES", "NO"
# $ weeks_per_vessel_per_compl <int> 33, 19
# $ total_weeks_per_vessel     <int> 52, 52

compl_clean_sa_vs_gom_m_int_cnt_w1_perc <-
  compl_clean_sa_vs_gom_m_int_cnt_w1 %>%
  dplyr::mutate(percent_compl =
           weeks_per_vessel_per_compl * 100 / total_weeks_per_vessel)

compl_clean_sa_vs_gom_m_int_cnt_w1_perc %>%
  filter(vessel_official_number == "1020822",
         year == "2022") %>%
  select(percent_compl, 
         compliant_,
         weeks_per_vessel_per_compl,
         total_weeks_per_vessel) %>%
  unique() %>%
  dplyr::glimpse()

compl_clean_sa_vs_gom_m_int_cnt_w1_perc %>%
  filter(permit_sa_gom == "sa_only",
         year == "2022") %>%
  View()

compl_clean_sa_vs_gom_m_int_cnt_w1_perc %>%
  filter(permit_sa_gom == "sa_only",
         year == "2022") %>%
  filter(compliant_ == "NO") %>%
  select(
    vessel_official_number,
    weeks_per_vessel_per_compl,
    total_weeks_per_vessel,
    percent_compl
  ) %>%
  unique() %>%
  dplyr::count(percent_compl, name = "cnt_percent_nc") %>%
  dplyr::arrange(desc(cnt_percent_nc)) %>%
  # dplyr::count(wt = cnt_percent_nc)
  # 1289 - total nc weeks
  dplyr::mutate(pp = cnt_percent_nc * 100 / sum(cnt_percent_nc)) %>%
  # dplyr::mutate(pp = cnt_percent_nc * 100 / 284) %>%
  View()

# by quantile
compl_clean_sa_vs_gom_m_int_cnt_w1_perc_short <-
  compl_clean_sa_vs_gom_m_int_cnt_w1_perc %>%
  filter(permit_sa_gom == "sa_only",
         year == "2022") %>%
  # filter(year_month == "Dec 2022") %>%
  filter(compliant_ == "NO") %>%
  select(
    vessel_official_number,
    weeks_per_vessel_per_compl,
    total_weeks_per_vessel,
    percent_compl
  ) %>%
  unique()
# percent_compl_only_c
# %>%
  # dplyr::mutate(quantile1 = quantile(x <- percent_compl)) %>%
  # dplyr::mutate(quantile = dplyr::ntile(percent_compl, 4)) %>%
  # str()
  # View()
  
# View(percent_compl_only_c)
# 
  # summary()
# quantile(x <- percent_compl_only_c$percent_compl)
#       0%        25%        50%        75%       100% 
# 1.923077  24.264706  52.000000  81.101190 100.000000 

# percent_compl_only_c %>% 
#   dplyr::mutate(quantile1 = ntile(percent_compl, 4)) %>% 
#   dplyr::count(quantile1)
#   quantile1     n
#       <int> <int>
# 1         1    27
# 2         2    27
# 3         3    27
# 4         4    26

split(percent_compl_only_c$percent_compl,
      cut(
        percent_compl_only_c$percent_compl,
        quantile(percent_compl_only_c$percent_compl,
                 prob = 0:4 / 4, names = FALSE),
        include = TRUE
      ))

# ---
# View(compl_clean_sa_vs_gom_m_int_cnt_w1_perc_short)
# https://stackoverflow.com/questions/66404334/how-to-divide-column-in-dataset-into-three-groups-tertiles-based-on-another-co
q_limits <-
  quantile(x <-
             compl_clean_sa_vs_gom_m_int_cnt_w1_perc_short$percent_compl)
# ,
    # 
    # prob = 0:4 / 4,
    # names = T
  # )
           # seq(0, 4, 1/4), na.rm = TRUE)

# compl_clean_sa_vs_gom_m_int_cnt_w1_perc_short %>%
  # dplyr::mutate(qq <-
cut(compl_clean_sa_vs_gom_m_int_cnt_w1_perc_short$percent_compl, q_limits, include.lowest = TRUE)
           # )


compl_clean_sa_vs_gom_m_int_cnt_w1_perc_short %>%
  dplyr::mutate(quantile_gr = ntile(percent_compl, 4)) %>% 
  # dplyr::mutate(quantile_gr = 
  #          quantile(compl_clean_sa_vs_gom_m_int_cnt_w1_perc_short$percent_compl,
  #                prob = 0:4 / 4, names = FALSE)) %>% 
  str()

# ---
# https://stackoverflow.com/questions/66404334/how-to-divide-column-in-dataset-into-three-groups-tertiles-based-on-another-co
# cars <- compl_clean_sa_vs_gom_m_int_cnt_w1_perc_short
breaks <- quantile(compl_clean_sa_vs_gom_m_int_cnt_w1_perc_short$percent_compl, c(0, .25, .5, .75))
# length(breaks)
# breaks <- c(0, breaks)
labels <- c('0-25', '25-50', '50-75')
# , '75-100'
# length(labels)
cuts <- cut(compl_clean_sa_vs_gom_m_int_cnt_w1_perc_short$percent_compl, breaks = breaks, labels = labels)
cars <- cbind(compl_clean_sa_vs_gom_m_int_cnt_w1_perc_short, cuts)
# View(cars)

# ---
percent_compl_limits <-
  quantile(x <- percent_compl_only_c$percent_compl)
percent_compl_cut_labels <- c('1-25', '25-52', '52-81', '81-100')
cuts <-
  cut(
    compl_clean_sa_vs_gom_m_int_cnt_w1_perc_short$percent_compl,
    breaks = percent_compl_limits,
    labels = percent_compl_cut_labels
  )

compl_clean_sa_vs_gom_m_int_cnt_w1_perc_short_cuts <-
  cbind(compl_clean_sa_vs_gom_m_int_cnt_w1_perc_short, cuts)

# View(compl_clean_sa_vs_gom_m_int_cnt_w1_perc_short_cuts)

compl_clean_sa_vs_gom_m_int_cnt_w1_perc_short_cuts %>% 
  filter(cuts == '81-100') %>% 
  dplyr::count(percent_compl, name = "amount_of_occurences") %>% 
  dplyr::count(wt = amount_of_occurences)
# 616
#    percent_compl   amount_of_occurences
# 1       81.25000   1
# 2       82.22222   1
# 3       83.33333   1
# ...
# 36      97.91667   1
# 37      98.07692   2
# 38     100.00000 561

# the Workflow
# 1)
# count percents - a given vsl non_compl per counted weeks total
# a vsl "A" has 52 w on record for 2022, was nc - 19
# 19*100/52 = 36.53846
# the vsl "A" was non-compliant 36.5% of all its records in 2022

compl_clean_sa_vs_gom_m_int_cnt_w %>%
  filter(vessel_official_number == "1020822",
         year == "2022") %>% dplyr::glimpse()
# $ compliant_             <chr> "NO", "YES"
# $ weeks_per_vessel       <int> 19, 33

compl_clean %>%
  filter(vessel_official_number == "1020822",
         year == "2022") %>% dim()
# 52 == 33 + 19

# 2) split nc_percentage into 4 buckets 
# 3) count how many in each bucket
# 4) cnt percents of (3)

# run workflow ----
# 1) count percents - a given vsl non_compl per counted weeks total ----
## 1a) how many weeks each vessel was present ----
count_weeks_per_vsl_permit_year_compl <-
  compl_clean_sa_vs_gom_m_int %>%
  dplyr::add_count(year, permit_sa_gom, vessel_official_number, compliant_, name = "weeks_per_vessel_per_compl") %>%
  dplyr::add_count(year, permit_sa_gom, vessel_official_number, name = "total_weeks_per_vessel")

# View(count_weeks_per_vsl_permit_year_compl)

## 1b) percent of compl/non-compl per total weeks each vsl was present ----
count_weeks_per_vsl_permit_year_compl_p <-
  count_weeks_per_vsl_permit_year_compl %>%
  dplyr::mutate(percent_compl =
           weeks_per_vessel_per_compl * 100 / total_weeks_per_vessel)

# test
# View(count_weeks_per_vsl_permit_year_compl_p)
count_weeks_per_vsl_permit_year_compl_p %>%
  filter(permit_sa_gom == "sa_only", year == "2022") %>%
  select(vessel_official_number) %>%
  unique() %>%
  dim()
# [1] 2178 

count_weeks_per_vsl_permit_year_compl_p %>%
  filter(permit_sa_gom == "sa_only", 
         year == "2022",
         compliant_ == "NO") %>%
  select(vessel_official_number) %>%
  # unique() %>%
# 1289    Non compliant vsl
  dim()
# [1] 26466 non compliant weeks

### test ----
count_weeks_per_vsl_permit_year_compl_p %>%
  filter(vessel_official_number == "1020822",
         year == "2022") %>%
  select(
    year,
    permit_sa_gom,
    compliant_,
    weeks_per_vessel_per_compl,
    total_weeks_per_vessel,
    percent_compl
  ) %>%
  unique() %>%
  dplyr::glimpse()
# $ compliant_                 <chr> "YES", "NO"
# $ weeks_per_vessel_per_compl <int> 33, 19
# $ total_weeks_per_vessel     <int> 52, 52
# $ percent_compl              <dbl> 63.46154, 36.53846

# 2) split nc_percentage into 4 buckets ----
## 2a Only non-compl and fewer cols ----
count_weeks_per_vsl_permit_year_compl_p_short <-
  count_weeks_per_vsl_permit_year_compl_p %>% 
  filter(compliant_ == "NO") %>%
  select(
    year,
    permit_sa_gom,
    vessel_official_number,
    weeks_per_vessel_per_compl,
    total_weeks_per_vessel,
    percent_compl
  ) %>%
  unique()

# add a column
count_weeks_per_vsl_permit_year_compl_p_short_y_p <-
  count_weeks_per_vsl_permit_year_compl_p_short %>%
  # compute on a data frame a row-at-a-time
  dplyr::rowwise() %>%
  dplyr::mutate(year_reg = 
           paste(year, permit_sa_gom)) %>% 
  # return to the default colwise operations
  dplyr::ungroup()
  
## 2b) get percentage "buckets" ----
# View(count_weeks_per_vsl_permit_year_compl_p_short_y_p)

count_weeks_per_vsl_permit_year_compl_p_short_y_p_cuts <-
  count_weeks_per_vsl_permit_year_compl_p_short_y_p %>%
  dplyr::mutate(
    percentage_rank =
      dplyr::case_when(
        percent_compl < 25 ~ '0-24%',
        25 <= percent_compl &
          percent_compl < 50 ~ '25-49%',
        50 <= percent_compl &
          percent_compl < 75 ~ '50-74%',
        75 <= percent_compl ~ '75-100%'
      )
  )

# View(count_weeks_per_vsl_permit_year_compl_p_short_cuts)

### test 2 ----
count_weeks_per_vsl_permit_year_compl_p_short_cuts %>% 
  filter(percentage_rank == '75-100%') %>%
  filter(year_reg == "2023 sa_only") %>% 
  dplyr::count(percent_compl, year_reg,
        name = "amount_of_occurences") %>%
  dplyr::arrange(desc(percent_compl)) %>% 
  View()
  dplyr::count(wt = amount_of_occurences)
# 499

# 3) count how many in each bucket ----
count_weeks_per_vsl_permit_year_compl_p_short_cuts_cnt_in_b <-  
  count_weeks_per_vsl_permit_year_compl_p_short_cuts %>%
    dplyr::add_count(year_reg, 
              percentage_rank,
              name = "cnt_in_bucket")

# test
    # View()
    #add_count(percentage_rank, year_reg, amount_of_occurences) %>% 
  #       dplyr::group_by(percentage_rank, year_reg) %>% 
  # summarise(n = n()) %>% 

count_weeks_per_vsl_permit_year_compl_p_short_cuts_cnt_in_b %>% 
   filter(year_reg == "2022 both") %>%
      select(year_reg, percentage_rank, cnt_in_bucket) %>%
      unique() %>% 
  dplyr::add_count(wt = cnt_in_bucket, name = "total_per_y_r") %>% 
    View()  
# "2022 both"
# 12+17+85+3
# [1] 117

## 3a) total per year / region ----
count_weeks_per_vsl_permit_year_compl_p_short_cuts_cnt_in_b_tot <-
  count_weeks_per_vsl_permit_year_compl_p_short_cuts_cnt_in_b %>%
  select(year_reg,
         percentage_rank,
         cnt_in_bucket) %>%
  unique() %>%
  # total cnt per year, region
  dplyr::add_count(year_reg, wt = cnt_in_bucket, name = "total_per_y_r")
# %>%
#   filter(year_reg == "2022 both")
  
# View(count_weeks_per_vsl_permit_year_compl_p_short_cuts_cnt_in_b_tot)

# 4) cnt percents of (3) ----
count_weeks_per_vsl_permit_year_compl_p_short_cuts_cnt_in_b_perc <-
  count_weeks_per_vsl_permit_year_compl_p_short_cuts_cnt_in_b %>%
  dplyr::add_count(year, permit_sa_gom, name = "vsls_per_y_r") %>%
  dplyr::mutate(perc_vsls_per_y_r_b = cnt_in_bucket * 100 / vsls_per_y_r) %>%
  dplyr::mutate(perc_labels = paste0(round(perc_vsls_per_y_r_b, 1), "%"))

# test
count_weeks_per_vsl_permit_year_compl_p_short_cuts_cnt_in_b_perc %>% 
  # filter(year == "2023", permit_sa_gom == "gom_only") %>%
    filter(year_reg == "2023 gom_only") %>%
  select(perc_vsls_per_y_r_b, percentage_rank) %>%
  unique() %>%
  View()

# count_weeks_per_vsl_permit_year_compl_p_short_cuts_cnt_in_b_tot_perc <-
#   count_weeks_per_vsl_permit_year_compl_p_short_cuts_cnt_in_b_tot %>%
#   dplyr::mutate(percent_per_y_r = cnt_in_bucket * 100 / total_per_y_r) %>%
#   dplyr::mutate(perc_labels = paste0(round(percent_per_y_r, 1), "%"))

# count_weeks_per_vsl_permit_year_compl_p_short_cuts_cnt_in_b_tot_perc %>%
#   filter(year_reg  == "2022 both" |
#            year_reg  ==  "2022 gom_only") %>% View()

# dplyr::glimpse(count_weeks_per_vsl_permit_year_compl_p_short_cuts_cnt_in_b_tot_perc)

# plots ----
gg_count_weeks_per_vsl_permit_year_compl_p_short_cuts_cnt_in_b_tot_perc <-
  count_weeks_per_vsl_permit_year_compl_p_short_cuts_cnt_in_b_perc$year_reg %>%
  unique() %>%
  purrr::map(function(curr_year_region) {
    # browser()
    total_non_compl_df <-
      count_weeks_per_vsl_permit_year_compl_p_short_cuts_cnt_in_b_perc %>%
      filter(year_reg == curr_year_region) %>%
      select(perc_vsls_per_y_r_b,
             percentage_rank,
             perc_labels) %>%
      unique()
    
    # y_p_title <-
    #   paste(curr_year_region, total_non_compl_df, "non compl")
    y_p_title <- curr_year_region
    one_plot <-
      ggplot(total_non_compl_df,
             aes(x = percentage_rank,
                 y = perc_vsls_per_y_r_b)) +
      geom_col(fill = "deepskyblue") +
      labs(title = y_p_title,
           x = "Been non compliant",
           y = "% nc vsls per year & region") +
      geom_text(aes(label = perc_labels),
                position = position_stack(vjust = 0.5)) +
      ylim(0, 100)
    
    return(one_plot)
  })

gg_count_weeks_per_vsl_permit_year_compl_p_short_cuts_cnt_in_b_tot_perc[[5]]

super_title = "Percent non compliant vessels per year & region"

grid.arrange(grobs =
               gg_count_weeks_per_vsl_permit_year_compl_p_short_cuts_cnt_in_b_tot_perc,
             top = super_title,
             # left = my_legend,
             ncol = 3)
# percent_distribution.png

# check again, by a vessel ----
# vessel_official_number, permit_sa_gom, compliant_, year, weeks_per_vessel_per_compl, total_weeks_per_vessel
# 1) count percents - a given vsl non_compl per counted weeks total
# uniq vessels:
compl_clean_sa_vs_gom_m_int %>% 
  select(vessel_official_number) %>% 
  unique() %>% 
  dim()
# 4017

compl_clean_sa_vs_gom_m_int %>% 
  dplyr::count(vessel_official_number, compliant_) %>% 
  dplyr::count(compliant_, wt = n) %>% 
  View()
# weeks
# NO
# 42174
# YES
# 166719

compl_clean_sa_vs_gom_m_int %>% 
  filter(compliant_ == "NO") %>% 
  select(vessel_official_number) %>% 
  unique() %>% 
  dim()
# not compliant
# 2441

compl_clean_sa_vs_gom_m_int %>% 
  dplyr::count(vessel_official_number, year, permit_sa_gom) %>% 
  View()

count_weeks_per_vsl_permit_year_compl_p_short %>%
  select(vessel_official_number) %>%
  unique() %>%
  dim()
# 2441 ok

# View(count_weeks_per_vsl_permit_year_compl_p_short)

# 2) split nc_percentage into 4 buckets 

count_weeks_per_vsl_permit_year_compl_p_short_y_p_cuts %>% 
    select(vessel_official_number) %>% 
  unique() %>% 
  dim()
# 2441    ok

# 3) count how many in each bucket

count_weeks_per_vsl_permit_year_compl_p_short_cuts_cnt_in_b %>%
  select(vessel_official_number) %>%
  unique() %>%
  dim()
# 2441    ok

count_weeks_per_vsl_permit_year_compl_p_short_cuts_cnt_in_b %>%
  select(vessel_official_number) %>%
  unique() %>%
  dim()
# 2441    ok

count_weeks_per_vsl_permit_year_compl_p_short_cuts_cnt_in_b %>%
    filter(year == "2022", permit_sa_gom == "gom_only") %>%
  select(vessel_official_number) %>%
  unique() %>%
  dim()
# 187   

count_weeks_per_vsl_permit_year_compl_p_short_cuts_cnt_in_b %>%
  dplyr::count(year, permit_sa_gom) %>% 
#   year  permit_sa_gom     n
#   <chr> <chr>         <int>
# 1 2022  both            117
# 2 2022  gom_only        187
# 3 2022  sa_only        1289
# 4 2023  both            244
# 5 2023  gom_only          3
# 6 2023  sa_only        1384
count(year, wt = n)
# 1 2022   1593
# 2 2023   1631

# View(count_weeks_per_vsl_permit_year_compl_p_short_cuts_cnt_in_b)

# 4) cnt percents of (3)
count_weeks_per_vsl_permit_year_compl_p_short_cuts_cnt_in_b %>%
  filter(year == "2022", permit_sa_gom == "gom_only") %>%
  dplyr::count(vessel_official_number, percentage_rank) %>%
  dplyr::glimpse()

count_weeks_per_vsl_permit_year_compl_p_short_cuts_cnt_in_b %>%
  filter(year == "2023", permit_sa_gom == "gom_only") %>%
  dplyr::add_count(year, permit_sa_gom, name = "vsls_per_y_r") %>%
  dplyr::mutate(perc1 = cnt_in_bucket * 100 / vsls_per_y_r) %>%
  select(perc1, percentage_rank) %>%
  unique() %>%
  dplyr::glimpse()
# Rows: 2
# Columns: 2
# $ perc1           <dbl> 33.33333, 66.66667
# $ percentage_rank <chr> "50-74%", "0-24%"
# ==

compl_clean_sa_vs_gom_m_int %>% 
  filter(year == "2023", permit_sa_gom == "gom_only") %>%
  select(vessel_official_number) %>% 
  unique() %>% 
  dim()
# 998

compl_clean_sa_vs_gom_m_int %>% 
  filter(year == "2023", permit_sa_gom == "gom_only") %>%
  select(vessel_official_number, compliant_) %>% 
  unique() %>% 
  dplyr::count(compliant_)
# not uniq
# 1 NO            13
# 2 YES        18429

# uniq
#   compliant_     n
#   <chr>      <int>
# 1 NO             3
# 2 YES          998

compl_clean_sa_vs_gom_m_int %>% 
  filter(year == "2023",
         permit_sa_gom == "gom_only",
         compliant_ == "NO") %>%
  dplyr::count(vessel_official_number) %>% dplyr::glimpse()
# $ vessel_official_number <chr> "1247024", "1298355", "FL4749LH"
# $ n                      <int> 11, 1, 1

# tot_nc_weeks 13
# perc 13:100% 11:x% 11*100/13
# 84.61538%  7.692308%  7.692308%
# "1247024", "1298355", "FL4749LH"
# 1*100/13

# ===
compl_clean_sa_vs_gom_m_int_cnt_1 <-
  compl_clean_sa_vs_gom_m_int %>%
  select(year,
         permit_sa_gom,
         vessel_official_number,
         compliant_) %>%
  dplyr::add_count(year,
            permit_sa_gom,
            vessel_official_number,
            compliant_,
            name = "weeks_per_vessel_per_compl_per_y") %>%
  dplyr::add_count(year,
            permit_sa_gom,
            vessel_official_number,
            name = "total_weeks_per_vessel_per_y")

compl_clean_sa_vs_gom_m_int_cnt_1 %>% 
  filter(vessel_official_number == "1020822",
         year == "2022",
         permit_sa_gom == "sa_only") %>% 
  # dplyr::count(vessel_official_number)
# 52
  View()
  dim()
# 52

compl_clean_sa_vs_gom_m_int_cnt_1 %>%
  filter(vessel_official_number == "1020822",
         year == "2022") %>%
  unique() %>%
  dplyr::glimpse()
# $ compliant_                 <chr> "YES", "NO"
# $ weeks_per_vessel_per_compl <int> 33, 19
  

  # dplyr::add_count(year, permit_sa_gom, vessel_official_number, compliant_, name = "weeks_per_vessel_per_compl") %>%
  # filter(compliant_ == "NO",
  #        year == "2022",
  #        permit_sa_gom == "gom_only") %>%
  # View()

# count_weeks_per_vsl_permit_year_compl <-
#   compl_clean_sa_vs_gom_m_int %>%
#   dplyr::add_count(year, permit_sa_gom, vessel_official_number, compliant_, name = "weeks_per_vessel_per_compl") %>%

compl_clean_sa_vs_gom_m_int_cnt_1 %>% 
  select(vessel_official_number) %>% 
  unique() %>% 
  dim()
4017
