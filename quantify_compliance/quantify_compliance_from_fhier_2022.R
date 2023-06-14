# get_data_from_param <- "csv"
# 2022
# dual + GOM vs. SA
# 2023
# dual + SA
library(grid)
source("~/R_code_github/quantify_compliance/quantify_compliance_start.R")

# remove 2023 gom_only ----
compl_clean_sa_vs_gom_m_int_filtered <-
  compl_clean_sa_vs_gom_m_int %>%
  filter(!(year == '2023' & permit_sa_gom == "gom_only"))

# add year_permit column ----
compl_clean_sa_vs_gom_m_int_c <-
  compl_clean_sa_vs_gom_m_int_filtered %>%
  mutate(
    year_permit =
      case_when(
        year == "2022" & permit_sa_gom == "sa_only" ~
          paste(year, "sa_only"),
        year == "2022" & (permit_sa_gom %in% c("gom_only",
                                               "both")) ~
          paste(year, "gom_dual"),
        year == "2023" & (permit_sa_gom %in% c("sa_only",
                                               "both")) ~
          paste(year, "sa_dual")
      )
  )

# View(compl_clean_sa_vs_gom_m_int_c)

# save vsl count ----

count_all_vessels <-
  compl_clean_sa_vs_gom_m_int %>%
  select(vessel_official_number) %>%
  unique() %>%
  dim()
# 4017 vessels

count_not_gom23_vessels <-
compl_clean_sa_vs_gom_m_int_filtered %>%
  select(vessel_official_number) %>%
  unique() %>%
  dim()
# 3887 vessels

vessels_compl_or_not_per_y_r_all <-
  compl_clean_sa_vs_gom_m_int %>%
  select(vessel_official_number, compliant_, year, permit_sa_gom) %>%
  unique() %>%
  count(compliant_, year, permit_sa_gom)

vessels_compl_or_not_per_y_r_not_gom23 <-
  compl_clean_sa_vs_gom_m_int_c %>%
  select(vessel_official_number, compliant_, year_permit) %>%
  unique() %>%
  count(compliant_, year_permit)
# vessels
# 1 NO         2022 gom_dual   304
# 2 NO         2022 sa_only   1289
# 4 YES        2022 gom_dual  1482
# 5 YES        2022 sa_only   1617
# 3 NO         2023 sa_dual   1628
# 6 YES        2023 sa_dual   2125


# compl vs. non-compl vessels per year, region ----

## fewer fields ----
compl_clean_sa_vs_gom_m_int_c_short <-
  compl_clean_sa_vs_gom_m_int_c %>%
  select(vessel_official_number, year_permit, compliant_) %>%
  unique()

# glimpse(compl_clean_sa_vs_gom_m_int_c_short)

# separate vessels non-compliant at least once per year ----
non_compl_vessel_ids_per_y_r <-
  compl_clean_sa_vs_gom_m_int_c_short %>%
  filter(compliant_ == "NO") %>%
  select(vessel_official_number, year_permit) %>%
  unique()

# View(non_compl_vessel_ids_per_y_r)
# 3221

## split by year_permit ----
all_compl_vs_non_compl_per_year_cnt_list <-
  split(compl_clean_sa_vs_gom_m_int_c_short,
        as.factor(compl_clean_sa_vs_gom_m_int_c_short$year_permit))

# View(all_compl_vs_non_compl_per_year_cnt_list)

### test ----
all_compl_vs_non_compl_per_year_cnt_list[["2022 sa_only"]] %>%
  unique() %>% dim()
# [1] 2906 3

# If a vessel was non-compliant even once during a year, it is non_compliant for that year.
# remove non-compl vessels from compliant, to count each vessel once per year
# total unique vessels number vs. non-compl vessels

compl_only <-
  names(all_compl_vs_non_compl_per_year_cnt_list) %>%
  # for each year_permit
  map_df(
    function(current_year_permit) {
      # browser()
      # get a df
      curr_df <-
        all_compl_vs_non_compl_per_year_cnt_list[[current_year_permit]]

      # get non compliant vessel ids for the current_year_permit
      curr_non_compl_vsl_ids <-
        non_compl_vessel_ids_per_y_r %>%
        filter(year_permit == current_year_permit) %>%
        select(vessel_official_number)

      # keep only vessels from the current df which a not "non-compliant"
      curr_df_compl_only <-
        curr_df %>%
        filter(!(vessel_official_number %in% curr_non_compl_vsl_ids$vessel_official_number))

      return(curr_df_compl_only)
    }
)

### test ----
compl_only %>%
  filter(year_permit == "2022 sa_only") %>%
  dim()
# 889  3

# get total unique vessel_ids per year_permit ----

## test

compl_only %>%
  filter(year_permit == "2022 sa_only") %>%
  count(compliant_)
# YES 889

compl_only_cnts <-
  compl_only %>%
  add_count(year_permit, compliant_, name = "compl_vsls") %>%
  select(-c(vessel_official_number, compliant_)) %>%
  unique()

head(compl_only_cnts)
# 1 2022 gom_dual       1191
# 2 2022 sa_only         889
# 3 2023 sa_dual         608

non_compl_only_cnts <-
  non_compl_vessel_ids_per_y_r %>%
  add_count(year_permit, name = "non_compl_vsls") %>%
  select(-vessel_official_number) %>%
  unique()

head(non_compl_only_cnts)
# 1 2022 sa_only            1289
# 2 2022 gom_dual            304
# 3 2023 sa_dual            1628

vessels_cnt_per_year_reg_compl <-
  full_join(compl_only_cnts, non_compl_only_cnts)
# Joining with `by = join_by(year_permit)`

head(vessels_cnt_per_year_reg_compl)
#   year_permit   compl_vsls non_compl_vsls
# 1 2022 gom_dual       1191            304
# 2 2022 sa_only         889           1289
# 3 2023 sa_dual         608           1628

# add total vessels_cnt_per_year_reg ----
vessels_cnt_per_year_reg_compl_tot <-
  vessels_cnt_per_year_reg_compl %>%
  # compute on a data frame a row-at-a-time
  dplyr::rowwise() %>%
  mutate(total_vsl_ids_per_y_r = sum(compl_vsls, non_compl_vsls)) %>%
  # return to the default colwise operations
  dplyr::ungroup()

# without rowwise:
# $ total_vsl_ids_per_y_r <int> 5909, 5909, 5909

glimpse(vessels_cnt_per_year_reg_compl_tot)
# $ year_permit           <chr> "2022 gom_dual", "2022 sa_…
# $ compl_vsls            <int> 1191, 889, 608
# $ non_compl_vsls        <int> 304, 1289, 1628
# $ total_vsl_ids_per_y_r <int> 1495, 2178, 2236

# get perc non_compl vs. total for each year_permit ----
# names(vessels_cnt_per_year_reg_compl_tot)

vessels_cnt_per_year_reg_compl_tot_perc <-
  vessels_cnt_per_year_reg_compl_tot %>%
  mutate(percent_compl = compl_vsls * 100 / total_vsl_ids_per_y_r) %>%
  mutate(percent_non_compl = non_compl_vsls * 100 / total_vsl_ids_per_y_r)

glimpse(vessels_cnt_per_year_reg_compl_tot_perc)

make_one_plot_compl_vs_non_compl <-
  function(my_df, current_title = "", no_legend = FALSE) {
  # browser()
  one_plot <-
    my_df %>%
    ggplot(aes(x = is_compliant,
               y = percent,
               fill = is_compliant)) +
    geom_col() +
    ylim(0, 100) +
    geom_text(aes(label = paste0(round(percent, 1), "%")),
              position = position_stack(vjust = 0.5)) +
    labs(title = current_title,
         x = "",
         y = "") +
    scale_x_discrete(labels = c("Yes", "No")) +

    scale_fill_manual(
      values =
        c(
          "percent_compl" = "lightgreen",
          "percent_non_compl" = "red"
        ),
      name = "Is compliant?",
      labels = c("Yes", "No")
    )

  if (no_legend) {
    one_plot <- one_plot +
      theme(legend.position = "none")
  }

  return(one_plot)
}

# plots_for_c_vs_nc_vsls <- function(my_df, y_r_title, total_vsls) {
#   current_title <-
#     paste0(y_r_title, " permitted (Total vsls: ", total_vsls, ")")
#   one_plot <-
#     make_one_plot_compl_vs_non_compl(my_df, current_title)
#   return(one_plot)
# }

make_year_permit_label <- function(curr_year_permit) {
  stringr::str_replace(toupper(curr_year_permit),
                       "_", " ") %>%
    return()
}

gg_all_c_vs_nc_plots <-
  vessels_cnt_per_year_reg_compl_tot_perc$year_permit %>%
  map(function(curr_year_permit) {
    # browser()
    curr_df <-
      vessels_cnt_per_year_reg_compl_tot_perc %>%
      filter(year_permit == curr_year_permit) %>%
      pivot_longer(
        cols = c(percent_compl,
                 percent_non_compl),
        names_to = "is_compliant",
        values_to = "percent"
      )

    y_r_title <-
      make_year_permit_label(curr_year_permit)

    total_vsls <- unique(curr_df$total_vsl_ids_per_y_r)

    current_title <-
      paste0(y_r_title, " permitted (Total vsls: ", total_vsls, ")")
    one_plot <-
      make_one_plot_compl_vs_non_compl(curr_df, current_title)

    return(one_plot)

  })

# % of non-compliant South Atlantic Only Permitted Vessels by month (2022)
# % of non-compliant Gulf + Dual permitted vessels by month (2022)

gg_all_c_vs_nc_plots[[3]]

main_title = "Percent unique compliant vs. non compliant vessels for 2022"

grid.arrange(gg_all_c_vs_nc_plots[[1]],
             gg_all_c_vs_nc_plots[[2]],
             top = main_title)

# TODO:
# keep only one legend

# add percentage for whole 2022 ----
# View(vessels_cnt_per_year_reg_compl_tot_perc)

total_p_2022 <-
  vessels_cnt_per_year_reg_compl_tot_perc %>%
  filter(startsWith(year_permit, "2022")) %>%
  mutate(
    tot_compl = sum(compl_vsls),
    tot_non_compl = sum(non_compl_vsls),
    tot_v_2022 = sum(total_vsl_ids_per_y_r)
  ) %>%
  mutate(
    perc_compl_2022 = tot_compl * 100 / tot_v_2022,
    perc_non_compl_2022 = tot_non_compl * 100 / tot_v_2022
  )
# View(total_p_2022)

total_p_2022_longer <-
  total_p_2022 %>%
  select(perc_compl_2022,
         perc_non_compl_2022) %>%
  unique() %>%
  # rename fields for the plot
  dplyr::rename(percent_non_compl = perc_non_compl_2022,
                percent_compl = perc_compl_2022
                ) %>%
  pivot_longer(
    cols = c(percent_compl,
             percent_non_compl),
    names_to = "is_compliant",
    values_to = "percent"
  ) %>%
  unique()

# View(total_p_2022_longer)
# View(total_p_2022)

total_vsls <- total_p_2022$tot_v_2022 %>%
  unique()

y_2022_title <-
  paste0(
        "Compliant vs. non_compliant vessels in 2022 (",
    total_p_2022$tot_v_2022,
    " total vessels)"
  ) %>%
  unique()

one_2022_plot <-
      make_one_plot_compl_vs_non_compl(total_p_2022_longer, y_2022_title)
#
#
#   (total_p_2022_longer, y_r_title, total_vsls)

one_2022_plot

# Non compliant only ----
# names(compl_clean_sa_vs_gom_m_int_c)
# 1) count percents - a given vsl non_compl per counted weeks total ----
## 1a) how many weeks each vessel was present ----
count_weeks_per_vsl_permit_year_compl <-
  compl_clean_sa_vs_gom_m_int_c %>%
  add_count(year_permit, vessel_official_number, compliant_, name = "weeks_per_vessel_per_compl") %>%
  add_count(year_permit, vessel_official_number, name = "total_weeks_per_vessel")

# View(count_weeks_per_vsl_permit_year_compl)

## test 1a ----
count_weeks_per_vsl_permit_year_compl %>%
 filter(vessel_official_number == "1000042" &
          year == "2022") %>%
 select(year, compliant_, weeks_per_vessel_per_compl, total_weeks_per_vessel) %>%
  unique()
#   year  compliant_ weeks_per_vessel_per_compl total_weeks_per_vessel
# 1 2022  YES                                50                     52
# 2 2022  NO                                  2                     52

nc_2022_sa_only_test <-
  count_weeks_per_vsl_permit_year_compl %>%
  filter(year_permit == "2022 sa_only",
         compliant_ == "NO") %>%
  select(vessel_official_number,
         weeks_per_vessel_per_compl,
         total_weeks_per_vessel) %>%
  unique()

head(nc_2022_sa_only_test)
            # weeks_per_vessel_per…¹ total_weeks_per_vessel
# 1 VA9236AV 52 52
# 2 VA6784AD 24 24
# 3 VA4480ZY 44 44
# 4 SC9207BX 26 50
# 5 SC8907DF 14 40
# 6 SC8298DH 45 45

count_weeks_per_vsl_permit_year_compl %>%
  filter(year_permit == "2022 sa_only",
         compliant_ == "YES",
         vessel_official_number == "SC8907DF") %>%
  select(vessel_official_number,
         weeks_per_vessel_per_compl,
         total_weeks_per_vessel) %>%
  unique()
# 26  40

## 1b) percent of compl/non-compl per total weeks each vsl was present ----
count_weeks_per_vsl_permit_year_compl_p <-
  count_weeks_per_vsl_permit_year_compl %>%
  mutate(percent_compl =
           weeks_per_vessel_per_compl * 100 / total_weeks_per_vessel)

View(count_weeks_per_vsl_permit_year_compl_p)

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

### test 1b ----
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
  glimpse()
# $ compliant_                 <chr> "YES", "NO"
# $ weeks_per_vessel_per_compl <int> 33, 19
# $ total_weeks_per_vessel     <int> 52, 52
# $ percent_compl              <dbl> 63.46154, 36.53846

# 2) split nc_percentage into 4 buckets ----
## 2a Only non-compl and fewer cols ----
count_weeks_per_vsl_permit_year_n_compl_p_short <-
  count_weeks_per_vsl_permit_year_compl_p %>%
  filter(compliant_ == "NO") %>%
  select(
    year_permit,
    vessel_official_number,
    weeks_per_vessel_per_compl,
    total_weeks_per_vessel,
    percent_compl
  ) %>%
  unique()

str(count_weeks_per_vsl_permit_year_n_compl_p_short)
# tibble [3,224 × 5] (S3: tbl_df/tbl/data.frame)
 # $ weeks_per_vessel_per_compl: int [1:3221] 52 24 44 26 14 45 5 41 52 27 ...
 # $ total_weeks_per_vessel    : int [1:3221] 52 24 44 50 40 45 41 45 52 52 ...
 # $ percent_compl             : num [1:3221] 100 100 100 52 35 ...

## 2b) get percentage "buckets" ----
# View(count_weeks_per_vsl_permit_year_n_compl_p_short_y_p)

get_p_buckets <- function(my_df, field_name) {

  my_df %>%
    mutate(
      percent_n_compl_rank =
        case_when(
          !!sym(field_name) < 25 ~ '0<= & <25%',
          25 <= !!sym(field_name) &
            !!sym(field_name) < 50 ~ '25<= & <50%',
          50 <= !!sym(field_name) &
            !!sym(field_name) < 75 ~ '50<= & <75%',
          75 <= !!sym(field_name) ~ '75<= & <=100%'
        )
    ) %>%
    return()
}

count_weeks_per_vsl_permit_year_n_compl_p_short_cuts <-
  get_p_buckets(count_weeks_per_vsl_permit_year_n_compl_p_short,
                "percent_compl")

### test 2 ----
count_weeks_per_vsl_permit_year_n_compl_p_short_cuts %>%
  filter(percent_n_compl_rank == '75<= & <=100%') %>%
  filter(year_permit == "2022 sa_only") %>%
  count(percent_compl, year_permit,
        name = "amount_of_occurences") %>%
  arrange(desc(percent_compl)) %>%
  # View()
  count(wt = amount_of_occurences)
# 633

# 3) count how many in each bucket ----

# View(count_weeks_per_vsl_permit_year_n_compl_p_short_cuts)

count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_cnt_in_b <-
  count_weeks_per_vsl_permit_year_n_compl_p_short_cuts %>%
    add_count(year_permit,
              percent_n_compl_rank,
              name = "cnt_v_in_bucket")

### test 3 ----
count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_cnt_in_b %>%
   filter(year_permit == "2022 sa_only") %>%
      select(year_permit, percent_n_compl_rank, cnt_v_in_bucket) %>%
      unique() %>%
  add_count(wt = cnt_v_in_bucket, name = "total_per_y_r") %>%
  arrange(percent_n_compl_rank) %>%
    str()
# $ percent_n_compl_rank: chr [1:4] "0<= & <25%" "25<= & <50%" "50<= & <75%" "75<= & <=100%"
# $ cnt_v_in_bucket     : int [1:4] 399 172 85 633
# $ total_per_y_r       : int [1:4] 1289 1289 1289 1289

# "2022 sa_only"
# 633+85+172+399
# [1] 1289

# 4) cnt percents of (3) ----
# View(count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_cnt_in_b)

count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_cnt_in_b_perc <-
  count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_cnt_in_b %>%
  add_count(year_permit, name = "vsls_per_y_r") %>%
  mutate(perc_vsls_per_y_r_b = cnt_v_in_bucket * 100 / vsls_per_y_r) %>%
  mutate(perc_labels = paste0(round(perc_vsls_per_y_r_b, 1), "%"))

### test 4 ----
count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_cnt_in_b_perc %>%
  filter(year_permit == "2022 sa_only") %>%
  select(percent_n_compl_rank, perc_vsls_per_y_r_b) %>%
  unique() %>%
  arrange(percent_n_compl_rank) %>%
  head()
#   percent_n_compl_rank perc_vsls_per_y_r_b
#   <chr>                              <dbl>
# 1 0<= & <25%                         31.0
# 2 25<= & <50%                        13.3
# 3 50<= & <75%                         6.59
# 4 75<= & <=100%                      49.1

# 5) plots ----

# View(count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_cnt_in_b_perc)

names(count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_cnt_in_b_perc)

gg_count_weeks_per_vsl_permit_year_compl_p_short_cuts_cnt_in_b_tot_perc <-
  count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_cnt_in_b_perc$year_permit %>%
  unique() %>%
  sort() %>%
  map(function(curr_year_permit) {
    # browser()
    total_non_compl_df <-
      count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_cnt_in_b_perc %>%
      filter(year_permit == curr_year_permit) %>%
      select(perc_vsls_per_y_r_b,
             percent_n_compl_rank,
             perc_labels,
             vsls_per_y_r) %>%
      unique()

    curr_title_y_p <- make_year_permit_label(curr_year_permit)
    y_p_title <- paste0(curr_title_y_p,
                       " (Total non compliant vessels: ",
                       total_non_compl_df$vsls_per_y_r,
                       ")"
                       )

    one_plot <-
      ggplot(total_non_compl_df,
             aes(x = percent_n_compl_rank,
                 y = perc_vsls_per_y_r_b)) +
      geom_col(fill = "deepskyblue") +
      labs(title = y_p_title,
           x = "",
           y = "% nc vsls per year & permit") +
      geom_text(aes(label = perc_labels),
                position = position_stack(vjust = 0.5)) +
      ylim(0, 100)

    "% of missing reports for non-compliant vessels"

    return(one_plot)
  })

gg_count_weeks_per_vsl_permit_year_compl_p_short_cuts_cnt_in_b_tot_perc[[1]]

super_title = "% of non-compliant vessels by permit (2022)"

footnote = textGrob(
  "X axes is % of missing reports for non-compliant vessels",
  gp = gpar(fontface = 3, fontsize = 10),
  # justify left
  hjust = 0,
  x = 0.01, y = 0.99,
  vjust = 1
)

grid.arrange(gg_count_weeks_per_vsl_permit_year_compl_p_short_cuts_cnt_in_b_tot_perc[[1]],
             gg_count_weeks_per_vsl_permit_year_compl_p_short_cuts_cnt_in_b_tot_perc[[2]],
             top = super_title,
             bottom = footnote)


grid.arrange(gg_count_weeks_per_vsl_permit_year_compl_p_short_cuts_cnt_in_b_tot_perc[[3]],
             top = super_title,
             bottom = footnote)

# TODO:
# all 2022

# Per month, region ----
# test
View(compl_clean_sa_vs_gom_m_int_c)

count_weeks_per_vsl_permit_year_compl_month <-
  compl_clean_sa_vs_gom_m_int_c %>%
  add_count(year_permit,
            year_month,
            vessel_official_number,
            compliant_,
            name = "weeks_per_vessel_per_compl_m") %>%
  add_count(year_permit,
            year_month,
            vessel_official_number,
            name = "total_weeks_per_vessel_per_compl_m")

View(count_weeks_per_vsl_permit_year_compl_month)

# test
# count_weeks_per_vsl_permit_year_compl_month %>%
#   select(
#     year_permit,
#     year_month,
#     compliant_,
#     weeks_per_vessel_per_compl_m,
#     total_weeks_per_vessel_per_compl_m
#   ) %>%
#   unique() %>%
#   filter(year_permit == "2022 sa_only") %>%
#   View()

count_weeks_per_vsl_permit_year_compl_m <-
  compl_clean_sa_vs_gom_m_int_c %>%
  add_count(year_permit,
            year_month,
            vessel_official_number,
            compliant_,
            name = "weeks_per_vessel_per_compl_m")

# test
count_weeks_per_vsl_permit_year_compl_m %>%
filter(year_permit == "2022 sa_only" &
           compliant_ == "NO") %>%
  select(vessel_official_number,
         compliant_,
         year_month,
         weeks_per_vessel_per_compl_m) %>%
  unique() %>%
  glimpse()
# $ vessel_official_number       <chr> "VA9236AV", "VA6784AD", "VA4480…
# $ compliant_                   <chr> "NO", "NO", "NO", "NO", "NO", "…
# $ year_month                   <yearmon> Dec 2022, Dec 2022, Dec 202…
# $ weeks_per_vessel_per_compl_m <int> 4, 4, 4, 4, 4, 4, 3, 4, 4, 4, 4…

count_weeks_per_vsl_permit_year_compl_m_tot <-
  count_weeks_per_vsl_permit_year_compl_m %>%
  add_count(year_permit,
            year_month,
            vessel_official_number,
            name = "total_weeks_per_vessel_m")

## 1) Month: percent compl weeks per vsl per month ----

count_weeks_per_vsl_permit_year_compl_m_tot_p <-
  count_weeks_per_vsl_permit_year_compl_m_tot %>%
  mutate(percent_compl_m =
           weeks_per_vessel_per_compl_m * 100 / total_weeks_per_vessel_m)

### test 1, by month ----
count_weeks_per_vsl_permit_year_compl_m_tot_p %>%
  filter(year_permit == "2022 sa_only" &
           vessel_official_number == "VA6784AD") %>%
  # compliant_ == "NO") %>%
  select(
    vessel_official_number,
    compliant_,
    year_month,
    weeks_per_vessel_per_compl_m,
    total_weeks_per_vessel_m,
    percent_compl_m
  ) %>%
  unique() %>%
  arrange(year_month) %>%
  View()

## 2a) Month: Only non-compl and fewer cols ----

nc_count_weeks_per_vsl_permit_year_compl_m_tot_p_sort <-
  count_weeks_per_vsl_permit_year_compl_m_tot_p %>%
  filter(compliant_ == "NO") %>%
  select(
    year_permit,
    year_month,
    vessel_official_number,
    weeks_per_vessel_per_compl_m,
    total_weeks_per_vessel_m,
    percent_compl_m,
    compliant_
  ) %>%
  unique()

## 2b) Month: get percentage "buckets" ----

nc_count_weeks_per_vsl_permit_year_compl_m_tot_p_sort_b <-
  get_p_buckets(nc_count_weeks_per_vsl_permit_year_compl_m_tot_p_sort,
                "percent_compl_m")

View(nc_count_weeks_per_vsl_permit_year_compl_m_tot_p_sort_b)

### test 2, by month ----
nc_count_weeks_per_vsl_permit_year_compl_m_tot_p_sort_b %>%
  filter(percent_n_compl_rank == '75<= & <=100%') %>%
  filter(year_permit == "2022 sa_only" &
           vessel_official_number == "VA9236AV") %>%
  add_count(percent_compl_m, year_permit,
        name = "amount_of_occurences") %>%
  arrange(desc(percent_compl_m)) %>%
  add_count(wt = amount_of_occurences) %>%
  View()

## 3) Month: count how many in each bucket ----

nc_count_weeks_per_vsl_permit_year_compl_m_tot_p_sort_b_cnt_in_b <-
  nc_count_weeks_per_vsl_permit_year_compl_m_tot_p_sort_b %>%
    add_count(year_permit,
              year_month,
              percent_n_compl_rank,
              name = "cnt_v_in_bucket")

nc_count_weeks_per_vsl_permit_year_compl_m_tot_p_sort_b_cnt_in_b_tot <-
  nc_count_weeks_per_vsl_permit_year_compl_m_tot_p_sort_b_cnt_in_b %>%
  select(year_month,
         year_permit,
         percent_n_compl_rank,
         cnt_v_in_bucket) %>%
  unique() %>%
  add_count(year_month, year_permit, wt = cnt_v_in_bucket,
            name = "tot_v_per_m_y_r")

### tests 3, by month ----

compl_clean_sa_vs_gom_m_int_c %>%
  filter(year_permit == "2022 sa_only") %>%
  filter(year_month == "Jan 2022") %>%
  filter(compliant_ == "NO") %>%
  select(vessel_official_number) %>%
  unique() %>% str()
# total 703 nc vsls in "Jan 2022 sa_only"

# still true
nc_count_weeks_per_vsl_permit_year_compl_m_tot_p_sort_b_cnt_in_b %>%
  filter(year_permit == "2022 sa_only") %>%
  filter(year_month == "Jan 2022") %>%
  select(vessel_official_number) %>%
  unique() %>% dim()
#  703

# View(nc_count_weeks_per_vsl_permit_year_compl_m_tot_p_sort_b_cnt_in_b)

nc_count_weeks_per_vsl_permit_year_compl_m_tot_p_sort_b_cnt_in_b_tot %>%
  filter(year_permit == "2022 sa_only") %>%
  filter(year_month == "Jan 2022") %>%
  arrange(percent_n_compl_rank) %>%
  str()
 # $ percent_n_compl_rank: chr [1:4] "0<= & <25%" "25<= & <50%" "50<= & <75%" "75<= & <=100%"
 # $ cnt_v_in_bucket     : int [1:4] 33 29 29 612
 # $ tot_v_per_m_y_r     : int [1:4] 703 703 703 703

# 33+29+29+612
# 703
# T

## 4) Month: cnt percents of (3) ----
# View(nc_count_weeks_per_vsl_permit_year_compl_m_tot_p_sort_b_cnt_in_b_tot)

nc_count_weeks_per_vsl_permit_year_compl_m_tot_p_sort_b_cnt_in_b_tot_p <-
  nc_count_weeks_per_vsl_permit_year_compl_m_tot_p_sort_b_cnt_in_b_tot %>%
  mutate(perc_vsls_per_y_r_b = cnt_v_in_bucket * 100 / tot_v_per_m_y_r) %>%
  mutate(perc_labels = paste0(round(perc_vsls_per_y_r_b, 1), "%"))

### test 4, by month ----

# names(nc_count_weeks_per_vsl_permit_year_compl_m_tot_p_sort_b_cnt_in_b_tot_p)
nc_count_weeks_per_vsl_permit_year_compl_m_tot_p_sort_b_cnt_in_b_tot_p %>%
  filter(year_permit == "2022 sa_only") %>%
  filter(year_month == "Jan 2022") %>%
  select(percent_n_compl_rank, perc_vsls_per_y_r_b) %>%
  unique() %>%
  arrange(percent_n_compl_rank) %>%
  head()
#   percent_n_compl_rank perc_vsls_per_y_r_b
# 1 0<= & <25%                          4.69
# 2 25<= & <50%                         4.13
# 3 50<= & <75%                         4.13
# 4 75<= & <=100%                      87.1

# 612*100/703 == 87.05548

# 5) Month plots ----

# View(nc_count_weeks_per_vsl_permit_year_compl_m_tot_p_sort_b_cnt_in_b_tot_p)

nc_count_weeks_per_vsl_permit_year_compl_m_tot_p_sort_b_cnt_in_b_tot_p_y_r <-
  split(nc_count_weeks_per_vsl_permit_year_compl_m_tot_p_sort_b_cnt_in_b_tot_p,
        as.factor(nc_count_weeks_per_vsl_permit_year_compl_m_tot_p_sort_b_cnt_in_b_tot_p$year_permit))

# View(nc_count_weeks_per_vsl_permit_year_compl_m_tot_p_sort_b_cnt_in_b_tot_p_y_r)

get_one_plot_by_month <-
  function(my_df, curr_year_month) {
    # browser()
    curr_data <- my_df %>%
      filter(year_month == curr_year_month)

    curr_year_permit <- curr_data$year_permit %>%
      unique()

    curr_tot_v_per_m_y_r <- curr_data$tot_v_per_m_y_r %>%
      unique()

    curr_title <- paste0(
      # curr_year_permit,
                        curr_year_month,
                        " (",
                        curr_tot_v_per_m_y_r,
                        " non compliant v)")

    one_plot <-
      ggplot(curr_data,
             aes(x = percent_n_compl_rank,
                 y = perc_vsls_per_y_r_b)) +
      geom_col(fill = "skyblue") +
      labs(title = curr_title,
           x = "",
           y = "") +
           # y = "% nc vsls") +
      geom_text(aes(label = perc_labels),
                position = position_stack(vjust = 0.5)) +
      ylim(0, 100)

    return(one_plot)
  }


sorted_year_permits <- names(nc_count_weeks_per_vsl_permit_year_compl_m_tot_p_sort_b_cnt_in_b_tot_p_y_r) %>%
  sort()
# [1] "2022 gom_dual" "2022 sa_only"  "2023 sa_dual"

year_permit_titles <-
  data.frame(
             super_title_gom = "% of non-compliant Gulf + Dual permitted vessels by month (2022)",
             super_title_sa = "% of non-compliant South Atlantic Only Permitted Vessels by month (2022)",
             super_title_2023 = "% of non-compliant South Atlantic + Dual permitted vessels by month (2023)")

names(year_permit_titles) <- sorted_year_permits


gg_month_nc_perc <-
  sorted_year_permits %>%
  map(
    function(current_year_permit) {
      # browser()
      curr_df <-
        nc_count_weeks_per_vsl_permit_year_compl_m_tot_p_sort_b_cnt_in_b_tot_p_y_r[[current_year_permit]]

      curr_year_months <-
        curr_df %>%
        select(year_month) %>%
        unique() %>%
        as.data.frame()

      list_of_plots <-
        curr_year_months$year_month %>%
        sort() %>%
        # repeat the function for each year_month
        # see the function definition F2
        map(~ get_one_plot_by_month(curr_df,
                                    curr_year_month = .))

      # add correct names instead of 1, 2...
      names(list_of_plots) <-
        sort(curr_year_months$year_month)

      # put the name and the plots into a list to return
      res <- list(current_year_permit, list_of_plots)
      return(res)
    }
  )

# gg_month_nc_perc[[1]][[2]]
# "2022 both"
# gg_month_nc_perc[[5]][[5]]

all_plots <-
  gg_month_nc_perc %>%
  # repeat for each entry
  map(function(curr_year_reg_list) {

    # browser()
    curr_year_permit <- curr_year_reg_list[[1]]


    curr_super_title <- year_permit_titles[[curr_year_permit]]

    gridExtra::arrangeGrob(grobs =
                             curr_year_reg_list[[2]],
                           top = curr_super_title,
                           ncol = 3) %>%
      return()
  })

# draw one plot to test
gridExtra::grid.arrange(all_plots[[2]])

# all plots per month to files ----
save_plots_list_to_pdf <-
  function(file_full_name,
           plots_list) {
    ggsave(
      file_full_name,
      plots_list,
      width = 20,
      height = 20,
      units = "cm"
    )
  }


gg_month_nc_perc %>%
  # repeat for each entry
  purrr::map(function(curr_year_reg_list) {
    # browser()
    super_title <- paste(super_title,
                         curr_year_reg_list[[1]])

    # arrangeGrob creates an object to use later
    all_plots_curr_year_reg <-
      gridExtra::arrangeGrob(grobs =
                    curr_year_reg_list[[2]],
                  top = super_title,
                  ncol = 3)

    file_name_base <- paste0(curr_year_reg_list[[1]],
                        "_percent_distribution_per_month",
                        ".pdf")
    file_path <-
      r"(quantify_compliance\jun_9_2023_uniq_vsls\per_month)"

    # file.path adds the correct concatenation
    file_full_name <- file.path(my_paths$outputs,
                           file_path,
                           file_name_base)

    # see the function definition F2
    save_plots_list_to_pdf(file_full_name,
           all_plots_curr_year_reg)
  })

# [[1]]
# [1] "C:/Users/anna.shipunova/Documents/R_files_local/my_outputs/quantify_compliance\\jun_9_2023_uniq_vsls\\per_month/2022 both_percent_distribution_per_month.pdf"
#
# [[2]]
# [1] "C:/Users/anna.shipunova/Documents/R_files_local/my_outputs/quantify_compliance\\jun_9_2023_uniq_vsls\\per_month/2022 gom_only_percent_distribution_per_month.pdf"
#
# [[3]]
# [1] "C:/Users/anna.shipunova/Documents/R_files_local/my_outputs/quantify_compliance\\jun_9_2023_uniq_vsls\\per_month/2022 sa_only_percent_distribution_per_month.pdf"
#
# [[4]]
# [1] "C:/Users/anna.shipunova/Documents/R_files_local/my_outputs/quantify_compliance\\jun_9_2023_uniq_vsls\\per_month/2023 both_percent_distribution_per_month.pdf"
#
# [[5]]
# [1] "C:/Users/anna.shipunova/Documents/R_files_local/my_outputs/quantify_compliance\\jun_9_2023_uniq_vsls\\per_month/2023 gom_only_percent_distribution_per_month.pdf"
#
# [[6]]
# [1] "C:/Users/anna.shipunova/Documents/R_files_local/my_outputs/quantify_compliance\\jun_9_2023_uniq_vsls\\per_month/2023 sa_only_percent_distribution_per_month.pdf"

# test numbers ----
compl_clean_sa_vs_gom_m_int_c %>%
  filter(compliant_ == "NO") %>%
  filter(year_permit == "2022 gom_only") %>%
  filter(year_month == "Jan 2022") %>%
  select(vessel_official_number) %>%
  unique() %>% dim()
# [1] 10  1
# ===
# 1)
# "% Non-Compliant Vessels in Jan 2022 (12345 permitted; 125 expired permits)". I realize that is a long title, so perhaps we can push the % non-compliant vessels to the main title, and those smaller titles over the figure could just start at "jan...". Having the # of expired permits (compared to the # of permits) in each figure would better explain if they haven't tried to renew, and therefore haven't had to submit reports in order to renew. That is pretty much our only means to get them to comply, in the SA.

# 2)
# do you see a progression through the months of 2022 of increasing non-compliance for SA vessels? ----

# View(compl_clean_sa_vs_gom_m_int_c)

compl_data_sa_2022 <-
  compl_clean_sa_vs_gom_m_int_c %>%
  filter(year_permit == "2022 sa_only")

## add month only for plots ----
compl_data_sa_2022_m <-
  compl_data_sa_2022 %>%
  dplyr::mutate(month_name = format(year_month, "%B")) %>%
  dplyr::mutate(month_num = format(year_month, "%m"))

compl_data_sa_2022_m_short <-
  compl_data_sa_2022_m %>%
  select(vessel_official_number,
         compliant_,
         permitgroupexpiration,
         month_name,
         month_num)

# check
# compl_clean_sa_vs_gom_m_int_c %>%
#   select(permit_groupexpiration,  permitgroupexpiration) %>%
#   unique() %>%
#   str()
# permit_groupexpiration: chr [1:190] "06/30/2023" "08/31/2023" "09/30/2023" "05/31/2024" ...
#  $ permitgroupexpiration : POSIXct[1:190], format: "2023-06-30"

  #79 count(compliant_, year, permit_sa_gom)

## compl/nc per month ----
compl_data_sa_2022_m_short_compl_vs_nc_per_m <-
  compl_data_sa_2022_m_short %>%
  select(vessel_official_number,
         compliant_,
         # permitgroupexpiration,
         month_name,
         month_num) %>%
  unique() %>%
  add_count(compliant_, month_name) %>%
  select(compliant_, month_name, month_num, n) %>%
  arrange(month_num) %>%
  unique()
# %>%
#   View()

## Month: get nc vessel_ids ----
compl_data_sa_2022_m_short_nc_v <-
  compl_data_sa_2022_m_short %>%
  filter(compliant_ == "NO") %>%
  select(vessel_official_number, month_num) %>%
  unique()

# check counts
# %>%
  # count(month_num, name = "nc_v_per_month")
# %>%
  # select(month_num, nc_v_per_month) %>%

## Month: get compl only vessel_ids ----

# df %>% group_by(a) %>% mutate(d = +(b %in% c))

View(compl_data_sa_2022_m_short)

compl_data_sa_2022_m_short %>%
  unique() %>%
  select(-permitgroupexpiration) %>%
  group_by(month_num) %>%
  tidyr::pivot_wider(names_from = vessel_official_number, values_from = compliant_) %>%
  View()
# Warning in View :
#   Values from `compliant_` are not uniquely identified; output will
# contain list-cols.
# • Use `values_fn = list` to suppress this warning.
# • Use `values_fn = {summary_fun}` to summarise duplicates.
# • Use the following dplyr code to identify duplicates.
#   {data} %>%
#   dplyr::group_by(permitgroupexpiration, month_name, month_num,
#   vessel_official_number) %>%
#   dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
#   dplyr::filter(n > 1L)

# check
# compl_data_sa_2022_m_short %>%
#   unique() %>%
#   dplyr::group_by(permitgroupexpiration,
#                   month_name,
#                   month_num,
#                   vessel_official_number) %>%
#   dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
#   dplyr::filter(n > 1L) %>%
#   View()

## get compl, no compl, or both per month ----
compl_data_sa_2022_m_short_is_compl_wide <-
  compl_data_sa_2022_m_short %>%
  unique() %>%
  dplyr::select(-permitgroupexpiration) %>%
  dplyr::group_by(month_num) %>%
  tidyr::pivot_wider(
    names_from = vessel_official_number,
    values_from = compliant_,
    # make it "NO_YES" if both
    values_fn = ~ paste0(sort(.x), collapse = "_")
  )

### check compl_data_sa_2022_m_short_is_compl_wide ----
compl_data_sa_2022_m_short_is_compl_wide %>%
  arrange(month_num) %>%
  select(month_name, SC9087BU) %>%
  tail()
# 1 07        July       YES
# 2 08        August     NO_YES
# 3 09        September  NO
# 4 10        October    NO_YES
# 5 11        November   YES
# 6 12        December   YES

## check before ----
compl_data_sa_2022_m %>%
  select(vessel_official_number, compliant_, month_name, month_num) %>%
  unique() %>%
  filter(vessel_official_number == "SC9087BU") %>%
  arrange(month_num) %>%
  tail(10)
#  3 SC9087BU               YES        July       07
#  4 SC9087BU               NO         August     08
#  5 SC9087BU               YES        August     08
#  6 SC9087BU               NO         September  09
#  7 SC9087BU               YES        October    10
#  8 SC9087BU               NO         October    10
#  9 SC9087BU               YES        November   11
# 10 SC9087BU               YES        December   12
# correct

# View(compl_data_sa_2022_m_short_is_compl_wide)
compl_data_sa_2022_m_short_is_compl <-
  compl_data_sa_2022_m_short_is_compl_wide %>%
  pivot_longer(
    cols = -c(month_name, month_num),
    values_to = "is_compl_or_both",
    names_to = "vessel_official_number"
  )

names(compl_data_sa_2022_m_short_is_compl)

compl_data_sa_2022_m_short_is_compl_cnt <-
  compl_data_sa_2022_m_short_is_compl %>%
  ungroup() %>%
  select(-vessel_official_number) %>%
  add_count(month_name, is_compl_or_both,
            name = "count_by_m_c") %>%
  unique() %>%
  arrange(month_num)

  View(compl_data_sa_2022_m_short_is_compl_cnt)

## get total counts per month ----
compl_data_sa_2022_m_short_is_compl_cnt_tot <-
  compl_data_sa_2022_m_short_is_compl_cnt %>%
  group_by(month_num) %>%
  pivot_wider(names_from = is_compl_or_both, values_from = count_by_m_c) %>%
  mutate(total_vsls_m = sum(YES, NO, NO_YES),
         tot_not_compl_m = sum(NO, NO_YES))

names(compl_data_sa_2022_m_short_is_compl_cnt_tot)
# get percentage ----
compl_data_sa_2022_m_short_is_compl_cnt_tot_perc <-
  compl_data_sa_2022_m_short_is_compl_cnt_tot %>%
  mutate(
    percent_yes = YES * 100 / total_vsls_m,
    percent_tot_not = tot_not_compl_m * 100 / total_vsls_m,
    percent_no = NO * 100 / total_vsls_m,
    percent_no_yes = NO_YES * 100 / total_vsls_m
  )

write_csv(
  compl_data_sa_2022_m_short_is_compl_cnt_tot_perc,
  file.path(
    my_paths$outputs,
    r"(quantify_compliance\by_dual)",
    "compl_data_sa_2022_m_short_is_compl_cnt_tot_perc.csv"
  )
)

plot(compl_data_sa_2022_m_short_is_compl_cnt_tot_perc$month_num, compl_data_sa_2022_m_short_is_compl_cnt_tot_perc$percent_yes)

# x = fct_rev(fct_reorder(
             # common_name,!!sym(count_field_name),
             # .fun = max
           # )),

plot_perc_compl_per_m <-
  compl_data_sa_2022_m_short_is_compl_cnt_tot_perc %>%
  ungroup() %>%
  mutate(month_name_order = fct_reorder(month_name,
                                        as.numeric(month_num))) %>%
  ggplot(aes(x = month_name_order,
             y = percent_yes,
             color = "blue")) +
  geom_point() +
  # geom_text(aes(label = paste0(round(percent, 1), "%")),
  #           position = position_stack(vjust = 0.5)) +
  labs(title = "% of compliant South Atlantic Only Permitted Vessels by month (2022)",
       x = "",
       y = "Percent") +
  # footnote
  labs(caption = "Counted for unique vessels per month as (compliant only) * 100 / (total)") +
  geom_point(aes(x = month_name_order,
                 y = percent_no_yes,
             color = "yellow")) +
  geom_point(aes(x = month_name_order,
                 y = percent_no,
             color = "red"))

plot_perc_compl_per_m

names(compl_data_sa_2022_m_short_is_compl_cnt_tot_perc)
# compl_data_sa_2022_m_short_is_compl_cnt_tot_perc %>%
#   ungroup() %>%
#   select(month_name, YES, tot_not_compl_m,  total_vsls_m, percent_yes) %>% View()

# check numbers
compl_data_sa_2022_m %>% 
        filter(year_month == "Jan 2022" & year_permit == "2022 sa_only") %>% select(vessel_official_number) %>% 
        unique() %>% 
        dim()
# 1635

# 932+703
# [1] 1635


names(compl_data_sa_2022_m_short_is_compl_cnt_tot_perc)
plot_perc_non_compl_per_m <-
  compl_data_sa_2022_m_short_is_compl_cnt_tot_perc %>%
  ungroup() %>%
  mutate(month_name_order = fct_reorder(month_name,
                                        as.numeric(month_num))) %>%
  ggplot(aes(x = month_name_order,
             y = percent_tot_not)) +
  geom_point(color = "red") +
  # geom_text(aes(label = paste0(round(percent, 1), "%")),
  #           position = position_stack(vjust = 0.5)) +
  labs(title = "% of compliant South Atlantic Only Permitted Vessels by month (2022)",
       x = "",
       y = "Percent") +
  # footnote
  labs(caption = "Counted for unique vessels per month as (compliant only) * 100 / (total)")

# do you see a progression through the months of 2022 of increasing non-compliance for SA vessels?
    
# check counts
# %>%
  # count(month_num, name = "nc_v_per_month")
# %>%
  # select(month_num, nc_v_per_month) %>%


compl_data_sa_2022_m_short_compl_vs_nc_per_m %>%
  mutate(month_name_order = fct_reorder(month_name,
                                        as.numeric(month_num)))




  ggplot()
  # x = fct_rev(fct_reorder(common_name,
  #                                  !!sym(count_field_name),
  #                                  .fun = max)),


      pivot_longer(
        cols = c(percent_compl,
                 percent_non_compl),
        names_to = "is_compliant",
        values_to = "percent"
      )
