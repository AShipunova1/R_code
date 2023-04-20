
##| echo: false
library(zoo)
library(gridExtra)

# include auxilary functions
source("~/R_code_github/useful_functions_module.r")
my_paths <- set_work_dir()

##| echo: false

source("~/R_code_github/compare_catch/get_data.R")

# There are different formats in different available files.
# Find a column name with "itis" in it
itis_field_name <- grep("itis", names(logbooks_content), value = T)
# catch_species_itis

# Same for "vessel.*official"
vessel_id_field_name <-
  grep("vessel.*official", names(logbooks_content), value = T)
# vessel_official_nbr

fhier_logbooks_content <-
  logbooks_content  %>%
  # create a new column
  mutate(trip_start_date_time =
    # trip start: combine a date without time, a space and a time
    paste(substr(trip_start_date, 1, 10),
    trip_start_time)) %>%
  # Same for the trip end
  mutate(trip_end_date_time = paste(substr(trip_end_date, 1, 10), trip_end_time)) %>%
  # change the new column types to a date
  change_to_dates("trip_start_date_time", "%Y-%m-%d %H%M") %>%
  change_to_dates("trip_end_date_time", "%Y-%m-%d %H%M") %>%
  # change the column type to a number
  mutate(reported_quantity = as.integer(reported_quantity))

# view
fhier_logbooks_content %>% select(starts_with("trip")) %>% str()

fhier_logbooks_content_date_fixed_tmp <-
  fhier_logbooks_content %>%
  # if a "trip_end_date" is before 2020 - use "notif_trip_end_date" column instead
  mutate(trip_end_date1 = ifelse(
    trip_end_date < "2020-01-01",
    notif_trip_end_date,
    trip_end_date
  ))

fhier_logbooks_content_date_fixed <-
  fhier_logbooks_content_date_fixed_tmp %>%
  # manually change the wrong value
  mutate(trip_end_date2 = ifelse(
    # find it
    grepl("1992", fhier_logbooks_content_date_fixed_tmp$trip_end_date1),
    # change it
    "2022-10-16 01:00:00",
    # don't change anything else
    trip_end_date1
  ))

fhier_logbooks_content_date_fixed %<>%
  filter(year(trip_end_date) == "2022")

fhier_logbooks_content_waves <-
  fhier_logbooks_content_date_fixed %>%
  # add a new column with a trip end Month
  mutate(end_month = as.yearmon(trip_end_date2)) %>%
  # add a new column with a trip end Year
  mutate(end_year =
           year(trip_end_date2)) %>%
  # add a new column with a number for each trip end Month
  mutate(end_month_num = month(trip_end_date2)) %>%
  # add a new column with a Wave
  mutate(end_wave  = floor((end_month_num + 1) / 2))

#| classes: test

# show the new columns
fhier_logbooks_content_waves %>%
  select(end_month, end_year, end_month_num, end_wave) %>%
  unique() %>%
  # sort by end_month_num
  arrange(end_month_num)

# Florida counties by region (from the Internet)
fl_counties <- list(
  "SA" = c(
    "Brevard",
    "Broward",
    "Duval",
    "Flagler",
    "Indian River",
    "Martin",
    "Miami-Dade",
    "Nassau",
    "Palm Beach",
    "St. Johns",
    "St. Lucie",
    "Volusia"
  ),
  "GOM" = c(
    "Bay",
    "Charlotte",
    "Citrus",
    "Collier",
    "Dixie",
    "Escambia",
    "Franklin",
    "Gulf",
    "Hernando",
    "Hillsborough",
    "Lee",
    "Levy",
    "Manatee",
    "Monroe",
    "Okaloosa",
    "Pasco",
    "Pinellas",
    "Santa Rosa",
    "Sarasota",
    "Taylor",
    "Wakulla",
    "Walton"
  )
)

fhier_logbooks_content_waves_fl_county <-
  fhier_logbooks_content_waves %>%
  # create a new column "end_port_fl_reg" with SA, GOM or whatever else left
  mutate(
    end_port_fl_reg = case_when(
      # check in the list
      fix_names(end_port_county) %in% fix_names(fl_counties$SA) ~ "sa",
      fix_names(end_port_county) %in% fix_names(fl_counties$GOM) ~ "gom",
      # if not on the list - keep it
      .default = end_port_county
    )
  )

fhier_logbooks_content_waves_fl_county %>%
  # get FL only
  filter(end_port_state == "FL") %>%
  # sort by county
  arrange(end_port_county) %>%
  distinct() %>%
  # 37 counties
  select(end_port_fl_reg) %>%
  # what else is in the new column beside sa and gom
  filter(!(end_port_fl_reg %in% c("sa", "gom"))) %>% unique()

# NOT-SPECIFIED

# list of states in the South Atlantic region (from the Internet)
states_sa <- data.frame(
  state_name = c(
    "Delaware",
    "District of Columbia",
    # "Florida", # exclude, we have it separated by county
    "Georgia",
    "Maryland",
    "North Carolina",
    "South Carolina",
    "Virginia",
    "West Virginia"
  )
)

sa_state_abb <-
  # a default R table
  state_tbl %>%
  # get only these in our list
  filter(state_name %in% tolower(states_sa$state_name)) %>%
  # get abbreviations
  select(state_abb)

fhier_logbooks_content_waves__sa_gom <-
  fhier_logbooks_content_waves_fl_county %>%
  # add a new column "end_port_sa_gom" with sa or gom for each state
  # use fix_name aux function to unify state names (lower case, no spaces etc.)
  mutate(end_port_sa_gom = case_when(
    # if a name is in our SA list - "sa", otherwise - "gom"
    fix_names(end_port_state) %in% fix_names(sa_state_abb$state_abb) ~ "sa",
    .default = "gom"
  )) %>%
  # go through the new column again
  # if an end port state is Florida - use the region from the previous step (column "end_port_fl_reg")
  # otherwise don't change
  mutate(end_port_sa_gom = ifelse(
    tolower(end_port_state) == "fl",
    end_port_fl_reg,
    end_port_sa_gom
  )) %>%
  # remove this column, we don't need it anymore
  select(-end_port_fl_reg)

#| classes: test
fhier_logbooks_content_waves__sa_gom %>%
  # look at states and regions
  select(end_port_state, end_port_sa_gom) %>%
  unique() %>%
  glimpse()

glimpse(fhier_logbooks_content_waves__sa_gom)

# combine dolphin and dolphinfish for FHIER data
fhier_logbooks_content_waves__sa_gom_dolph <-
  fhier_logbooks_content_waves__sa_gom %>%
  mutate(is_dolph = if_else(
    tolower(common_name) %in% c("dolphin", "dolphinfish"),
    "DOLPHIN",
    common_name
    )
  )

fhier_logbooks_content_waves__sa_gom_dolph %>%
  filter(grepl("dolphin", common_name, ignore.case = TRUE)) %>%
  # filter(tolower(common_name) %in% c("dolphin", "dolphinfish")) %>%
  select(common_name, is_dolph) %>%
  unique()

fhier_logbooks_content_waves__sa_gom_dolph %>%
  rename(common_name_orig = common_name,
         is_dolph = common_name)

glimpse(fhier_logbooks_content_waves__sa_gom_dolph)
# filter(tolower(common_name) %in% c("dolphin", "dolphinfish")) %>%
  # mutate(dolph_cnts = case_when(grepl("dolphin", common_name, ignore.case = TRUE) ~ sum()
  #   common_name starts_with() )
  #          )
  # head()

# ---

# select common names and itis in a separate data frame
fhier_common_names <-
  fhier_logbooks_content %>%
  # names()
  select(catch_species_itis, common_name) %>%
  unique()

# add column names
names(fhier_common_names) <- c("species_itis", "common_name")

fhier_catch_by_species_state_region_waves <-  
  fhier_logbooks_content_waves__sa_gom_dolph %>%
  # select only relevant columns
  select(
    is_dolph,
    end_port_state,
    end_port_sa_gom,
    end_year,
    end_wave,
    reported_quantity
  ) %>%
  # group by all of them but "reported_quantity"
  group_by(
    is_dolph,
    end_port_state,
    end_port_sa_gom,
    end_year,
    end_wave
  ) %>%
  # save a sum of reported_quantity in each group in fhier_quantity_by_4
  # remove NAs
  summarise(fhier_quantity_by_4 = sum(as.integer(reported_quantity), na.rm = TRUE)) %>%
  as.data.frame()

# str(fhier_catch_by_species_state_region_waves)
# str(fhier_common_names)

# add itis_codes back
fhier_catch_by_species_state_region_waves <-
  inner_join(fhier_catch_by_species_state_region_waves,
             fhier_common_names,
             join_by(is_dolph == common_name),
             relationship = "many-to-many"
             )

#| classes: test

fhier_catch_by_species_state_region_waves %>%
  # get the same species
  filter(is_dolph == "SNAPPER, RED") %>%
  # group by region
  group_by(is_dolph, end_port_sa_gom) %>%
  # sum the FHIER catch
  summarise(snapper_r_fhier_cnt = sum(fhier_quantity_by_4, na.rm = TRUE))

## MRIP data

mrip_estimate %<>%
  mutate(ab1 = as.integer(ab1))

mrip_estimate_catch_by_species_state_region_waves <-
  mrip_estimate %>%
  # select the relevan columns only
  select(itis_code, new_sta, sub_reg, year, wave, ab1) %>%
  # group by all except the counts
  group_by(itis_code, new_sta, sub_reg, year, wave) %>%
  # save the sum of "ab1" for each group in "mrip_estimate_catch_by_4"
  # remove NAs
  summarise(mrip_estimate_catch_by_4 = sum(as.integer(ab1), na.rm = TRUE)) %>%
  as.data.frame()

glimpse(mrip_estimate_catch_by_species_state_region_waves)
# 'data.frame':	878 obs. of  6 variables

# "year" and "wave" to numbers
mrip_estimate_catch_by_species_state_region_waves1 <-
  mrip_estimate_catch_by_species_state_region_waves %>%
  mutate(year = as.double(year)) %>%
  mutate(wave = as.double(wave))

mrip_estimate_catch_by_species_state_region_waves <-
  mrip_estimate_catch_by_species_state_region_waves1 %>%
  # change a 6 to "sa" and a 7 "gom", leave everything else in place
  mutate(sa_gom = case_when(sub_reg == "6" ~ "sa",
                            sub_reg == "7" ~ "gom",
                            .default = sub_reg),
                            # put the new column after sub_reg (by default at the end)
                            .after = sub_reg) %>%
  # drop sub_reg
  select(-sub_reg)

# str(mrip_estimate_catch_by_species_state_region_waves)

mrip_estimate_catch_by_species_state_region_waves_ren <-
  mrip_estimate_catch_by_species_state_region_waves %>%
  rename(species_itis = itis_code,
         state = new_sta
         )
# names(fhier_catch_by_species_state_region_waves) %>% cat()

fhier_catch_by_species_state_region_waves %<>%
  rename(
    state = end_port_state,
    sa_gom = end_port_sa_gom,
    year = end_year,
    wave = end_wave,
    fhier_catch_by_4 = fhier_quantity_by_4
  )

# # common field names
# wave_data_names_common <- c("species_itis",
#                      "state",
#                      "sa_gom",
#                      "year",
#                      "wave"
#                     )
# 
# # to be sure columns are in the same order
# names(mrip_estimate_catch_by_species_state_region_waves)
# 
# # mrip_names <- c("itis_code",
# #                 "new_sta",
# #                 "sa_gom",
# #                 "year",
# #                 "wave",
# #                 "mrip_estimate_catch_by_4"
# # )
# 
# # specific names
# wave_data_names_f <- c(wave_data_names_common, c("fhier_catch_by_4"))
# wave_data_names_m <- c(wave_data_names_common, c("mrip_estimate_catch_by_4"))
# 
# # change names
# names(fhier_catch_by_species_state_region_waves) <- wave_data_names_f
# names(mrip_estimate_catch_by_species_state_region_waves) <- wave_data_names_m
# 
# identical(names(fhier_catch_by_species_state_region_waves)[1:5],
          # names(mrip_estimate_catch_by_species_state_region_waves)[1:5])

# View(mrip_estimate_catch_by_species_state_region_waves)

str(fhier_catch_by_species_state_region_waves)

fhier_mrip_catch_by_species_state_region_waves <-
  full_join(fhier_catch_by_species_state_region_waves,
             mrip_estimate_catch_by_species_state_region_waves,
              by = join_by(species_itis, state, sa_gom, year, wave)
             )
# default Joining with `by = join_by(species_itis, state, sa_gom, year, wave)`
# final$S1 <- dplyr::coalesce(final[[" S1 .x"]],  final[[" S1 .y"]])

# look at the first 20 entries for snapper_r
fhier_mrip_catch_by_species_state_region_waves %>%
  filter(species_itis == "168853") %>% head(20)

#| classes: test
mrip_cnts_sa_gom_snapper_r <-
  mrip_estimate_catch_by_species_state_region_waves %>%
  # get one species
  filter(species_itis == "168853") %>%
  # group by region
  group_by(species_itis, sa_gom) %>%
  # sum the MRIP catch
  summarise(snapper_r_mrip_cnt = sum(mrip_estimate_catch_by_4, na.rm = TRUE))

mrip_cnts_sa_gom_snapper_r

#| classes: test

fhier_cnts_sa_gom_snapper_r <-
  fhier_catch_by_species_state_region_waves %>%
  # arrange(desc(fhier_catch_by_4)) %>% head()
  # get the same species
  filter(species_itis == "168853") %>%
  # group by region
  group_by(species_itis, sa_gom) %>%
  # sum the FHIER catch
  summarise(snapper_r_fhier_cnt = sum(fhier_catch_by_4, na.rm = TRUE))

fhier_cnts_sa_gom_snapper_r

#| classes: test

# compare the above numbers with those in the join, they should be the same
fhier_join_cnts_sa_gom_snapper_r <- 
  fhier_mrip_catch_by_species_state_region_waves %>%
  filter(species_itis == "168853") %>%
  group_by(species_itis, sa_gom) %>%
  summarise(snapper_r_fhier_cnt = sum(fhier_catch_by_4, na.rm = TRUE))

identical(fhier_cnts_sa_gom_snapper_r, fhier_join_cnts_sa_gom_snapper_r)

mrip_join_cnts_sa_gom_snapper_r <-
  fhier_mrip_catch_by_species_state_region_waves %>%
  filter(species_itis == "168853") %>%
  group_by(species_itis, sa_gom) %>%
  summarise(snapper_r_mrip_cnt = sum(mrip_estimate_catch_by_4, na.rm = TRUE))

identical(mrip_cnts_sa_gom_snapper_r, mrip_join_cnts_sa_gom_snapper_r)

# grep("grouper, black", fhier_common_names$common_name, value = T, ignore.case = T)

sa_top <- c(
"SNAPPER, RED",
"DOLPHIN",
"DOLPHINFISH",
"GROUPER, BLACK",
"GROUPER, GAG",
"GROUPER, RED",
"GROUPER, SCAMP",
"MACKEREL, SPANISH",
"SNAPPER, RED",
"TRIGGERFISH, GRAY"
)

sa_top_spp <-
  fhier_common_names %>%
  filter(common_name %in% sa_top)

# View(sa_top)
# intersect(sa_top, fhier_common_names$common_name)

gom_top <- c(
"AMBERJACK, GREATER",
"COBIA",
"GROUPER, BLACK",
"GROUPER, GAG",
"GROUPER, RED",
"GROUPER, SCAMP",
"MACKEREL, KING",
"MACKEREL, SPANISH",
"SNAPPER, GRAY",
"SNAPPER, RED",
"TRIGGERFISH, GRAY"
)

gom_top_spp <-
  fhier_common_names %>%
  filter(common_name %in% gom_top)

# View(gom_top_spp)
  
# intersect(sa_top_spp$common_name, gom_top_spp$common_name)
fhier_common_names %>%
  filter(common_name == "SNAPPER, RED")
# 168853

# A function to make a plot by spp.
plot_by_spp <- function(com_name, my_df, no_legend = TRUE) {
  # browser()

 one_plot <-
  my_df %>%
    # only the com name from the parameters
    filter(is_dolph == !!com_name) %>%
  ggplot(
         aes(x = year_wave,
             y = CATCH_CNT,
            # color by the agency and
            # make a legend if no_legend is FALSE
             fill = AGENCY)
  ) +
    # manually cange default colours
    scale_fill_manual(values = c("MRIP" = "deepskyblue", "FHIER" = "red")) +
    # columns are side by side (not stacked)
    geom_col(position = "dodge") +
    labs(title = com_name,
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

  # By default the "no_legend" parameter is TRUE
  if(no_legend) {
    one_plot <- one_plot +
      theme(legend.position = "none")
  }
  return(one_plot)
}

glimpse(fhier_mrip_catch_by_species_state_region_waves)
# Rows: 6,327
# Columns: 7
# ?Rows: 5,728

# make a new column "year_wave"
fhier_mrip_catch_by_species_state_region_waves_tmp1 <-
  mutate(fhier_mrip_catch_by_species_state_region_waves,
         year_wave = paste(year, wave, sep = "_"))

# Add the fhier_common_names we made earlier
fhier_mrip_catch_by_species_state_region_waves_tmp2 <-
  inner_join(fhier_mrip_catch_by_species_state_region_waves_tmp1,
           fhier_common_names,
           by = join_by(species_itis))

#| warning: false

# Make separate data frames
fhier_mrip_catch_by_species_state_region_waves_list_for_plot <-
  fhier_mrip_catch_by_species_state_region_waves_tmp2 %>%
  # split by sa_gom column
    split(as.factor(fhier_mrip_catch_by_species_state_region_waves$sa_gom)) %>%
  # remove extra columns in each df
    map(
      .f = list(. %>% dplyr::select(-one_of("year", "wave", "sa_gom")
                                    )
                )
  )

glimpse(fhier_mrip_catch_by_species_state_region_waves_list_for_plot)

#| classes: test

# For each region sum counts for one species,
# should be the same as before

# GOM fhier test
gom_fhier_test <-
  fhier_mrip_catch_by_species_state_region_waves_list_for_plot$gom %>%
  filter(species_itis == "168853") %>%
  group_by(species_itis) %>%
  summarise(snapper_r_fhier_cnt = sum(fhier_catch_by_4, na.rm = TRUE))

to_compare_gom <-
  fhier_cnts_sa_gom_snapper_r %>%
  filter(sa_gom == "gom")
  
identical(gom_fhier_test$snapper_r_fhier_cnt, to_compare_gom$snapper_r_fhier_cnt)

# SA test
sa_mrip_test <-
  fhier_mrip_catch_by_species_state_region_waves_list_for_plot$sa %>%
  filter(species_itis == "168853") %>%
  group_by(species_itis) %>%
  summarise(snapper_r_mrip_cnt = sum(mrip_estimate_catch_by_4, na.rm = TRUE))

to_compare_sa_mrip <-
  mrip_cnts_sa_gom_snapper_r %>%
  filter(sa_gom == "sa")
  
identical(sa_mrip_test$snapper_r_mrip_cnt, to_compare_sa_mrip$snapper_r_mrip_cnt)

# keep only entries for spp. in the top ten list,
# separately for each region
fhier_mrip_catch_by_species_state_region_waves_list_for_plot_gom10 <-
  fhier_mrip_catch_by_species_state_region_waves_list_for_plot$gom %>%
  filter(species_itis %in% gom_top_spp$species_itis)
# 231  
# filter(species_itis %in% n_most_frequent_fhier_10_list$gom$species_itis)
# Rows: 217
# str(fhier_mrip_catch_by_species_state_region_waves_list_for_plot_gom10)
# ?'data.frame':	196 obs. of  6 variables
# 'data.frame':	238 obs. of  6 variables (new list)

fhier_mrip_catch_by_species_state_region_waves_list_for_plot$sa %>%
  filter(common_name == "DOLPHIN") %>% head()

fhier_mrip_catch_by_species_state_region_waves_list_for_plot_sa10 <-
  fhier_mrip_catch_by_species_state_region_waves_list_for_plot$sa %>%
  filter(species_itis %in% sa_top_spp$species_itis) %>%
  mutate()
  # filter(species_itis %in% n_most_frequent_fhier_10_list$sa$species_itis)

glimpse(fhier_mrip_catch_by_species_state_region_waves_list_for_plot_sa10)
# Rows: 300
# Rows: 274
# 173
# 228 (new list)

#| classes: test

# For the top 10, for each region sum separately MRIP and FHIER counts for one species,
# should be the same as before

# SA, FHIER counts
to_compare_sa_fhier <-
  fhier_cnts_sa_gom_snapper_r %>%
  filter(sa_gom == "sa")

fhier_mrip_catch_by_species_state_region_waves_list_for_plot_sa10 %>%
  filter(species_itis == "168853") %>%
  group_by(species_itis) %>%
  summarise(snapper_r_fhier_cnt = sum(fhier_catch_by_4, na.rm = TRUE)) %>%
  select(snapper_r_fhier_cnt) %>%
  use_series(snapper_r_fhier_cnt) %>%
  identical(to_compare_sa_fhier$snapper_r_fhier_cnt)

# GOM, FHIER counts

to_compare_gom_fhier <-
  fhier_cnts_sa_gom_snapper_r %>%
  filter(sa_gom == "gom")

fhier_mrip_catch_by_species_state_region_waves_list_for_plot_gom10 %>%
  filter(species_itis == "168853") %>%
  group_by(species_itis) %>%
  summarise(snapper_r_fhier_cnt = sum(fhier_catch_by_4, na.rm = TRUE)) %>%
  select(snapper_r_fhier_cnt) %>%
  use_series(snapper_r_fhier_cnt) %>%
  identical(to_compare_gom_fhier$snapper_r_fhier_cnt)

# SA, MRIP counts
to_compare_sa_fhier <-
  fhier_cnts_sa_gom_snapper_r %>%
  filter(sa_gom == "sa")

fhier_mrip_catch_by_species_state_region_waves_list_for_plot_sa10 %>%
  filter(species_itis == "168853") %>%
  group_by(species_itis) %>%
  summarise(snapper_r_mrip_cnt = sum(mrip_estimate_catch_by_4, na.rm = TRUE)) %>%
  select(snapper_r_mrip_cnt) %>%
  use_series(snapper_r_mrip_cnt) %>% 
  identical(to_compare_sa_mrip$snapper_r_mrip_cnt)

# GOM, MRIP counts
to_compare_gom_mrip <-
  mrip_cnts_sa_gom_snapper_r %>%
  filter(sa_gom == "gom")

fhier_mrip_catch_by_species_state_region_waves_list_for_plot_gom10 %>%
  filter(species_itis == "168853") %>%
  group_by(species_itis) %>%
  summarise(snapper_r_mrip_cnt = sum(mrip_estimate_catch_by_4, na.rm = TRUE)) %>%
  select(snapper_r_mrip_cnt) %>%
  use_series(snapper_r_mrip_cnt) %>% 
  identical(to_compare_gom_mrip$snapper_r_mrip_cnt)

# numbers OK

# GOM plots ----
fhier_mrip_gom_to_plot <-
  fhier_mrip_catch_by_species_state_region_waves_list_for_plot_gom10 %>%
  # change to shorter column names
  rename(c("MRIP" = "mrip_estimate_catch_by_4",
           "FHIER" = "fhier_catch_by_4")) %>%
  # reformat to a long format to have fhier and mrip data side by side
  pivot_longer(
    cols = c(MRIP,
             FHIER),
    names_to = "AGENCY",
    values_to = "CATCH_CNT"
  ) %>%
  # use only the new columns
  select(year_wave, species_itis, is_dolph, AGENCY, CATCH_CNT) %>%
  # remove lines where one or another agency doesn't have counts for this species
  drop_na()

glimpse(fhier_mrip_gom_to_plot)

# an overview plot
plot(fhier_mrip_gom_to_plot)

# plot_by_spp("SNAPPER, RED", fhier_mrip_gom_to_plot)

           # for each common name from the top 10
plots10 <- map(unique(fhier_mrip_gom_to_plot$is_dolph),
              # run the plot_by_spp with this common name as a parameter and the default value for no_legend (TRUE)
               function(x) {plot_by_spp(x, fhier_mrip_gom_to_plot)}
               )


# Title for all plots together
super_title = "GOM: species cnt by waves"

# separate a legend
plot_w_legend <- plot_by_spp("MACKEREL, SPANISH",
                             fhier_mrip_gom_to_plot,
                             # keep the legend
                             FALSE)
# use an aux function to pull out the legend
my_legend <- legend_for_grid_arrange(plot_w_legend)

# combine all plots
grid.arrange(grobs = plots10,
             top = super_title,
             left = my_legend,
             ncol = 3)

# SA plots ----

fhier_mrip_sa_to_plot <-
  fhier_mrip_catch_by_species_state_region_waves_list_for_plot_sa10 %>%
  # rename to shorter column names
  rename(c("MRIP" = "mrip_estimate_catch_by_4",
           "FHIER" = "fhier_catch_by_4")) %>%
  # reformat to a long format to have fhier and mrip data side by side
  pivot_longer(
    cols = c(MRIP,
             FHIER),
    names_to = "AGENCY",
    values_to = "CATCH_CNT"
  ) %>%
  # use only the new columns
  select(year_wave, species_itis, is_dolph, AGENCY, CATCH_CNT) %>%
  # remove lines where one or another agency doesn't have counts for this species
  drop_na()

glimpse(fhier_mrip_sa_to_plot)


# An overview plot
plot(fhier_mrip_sa_to_plot)

           # for each common name from the top 10
plots10 <- map(unique(fhier_mrip_sa_to_plot$is_dolph),
              # run the plot_by_spp with this common name as a parameter and the default value for no_legend (TRUE)
               function(x) {plot_by_spp(x, fhier_mrip_sa_to_plot)}
               )

# The following code is the same as before, with "SA" instead of "GOM"
super_title = "SA: species cnt by waves, dolphin spp. together"

# separate a legend
plot_w_legend <- plot_by_spp("SNAPPER, RED", fhier_mrip_sa_to_plot, FALSE)
my_legend <- legend_for_grid_arrange(plot_w_legend)

grid.arrange(grobs = plots10,
             top = super_title,
             left = my_legend,
             ncol = 3)

## no NAs ----
dim(fhier_mrip_catch_by_species_state_region_waves)
# [1] 5728    7

fhier_mrip_catch_by_species_state_region_waves_no_na <-
fhier_mrip_catch_by_species_state_region_waves %>%
  filter(complete.cases(.))
# 'data.frame':	786 obs. of  7 variables:

# fhier_mrip_catch_by_species_state_region_waves %>% 
#   tidyr::drop_na(mrip_estimate_catch_by_4,
#                  # 878 obs. of  7 variables
#                  fhier_catch_by_4) %>% str()
# 786 obs. of  7 variables

fhier_mrip_catch_by_species_state_region_waves_no_na_list <-
  fhier_mrip_catch_by_species_state_region_waves_no_na %>%
  # split by sa/gom
    split(as.factor(fhier_mrip_catch_by_species_state_region_waves_no_na$sa_gom))

# Look at the top most frequent FHIER spp. for each region
n_most_frequent_fhier_10_list_no_na <-
  fhier_mrip_catch_by_species_state_region_waves_no_na_list %>%
  # repeat for each region (SA and GOM)
  map(function(x) {x %>%
      # select ITIS and counts
      select(species_itis, fhier_catch_by_4) %>%
      group_by(species_itis) %>%
      # add a new column with a sum of counts for each spp.
      summarise(fhier_catch_by_spp = sum(fhier_catch_by_4, na.rm = TRUE)) %>%
      # sort in the discending order
      arrange(desc(fhier_catch_by_spp)) %>%
      # get the top 10
      head(10)
})

n_most_frequent_fhier_10_list_no_na$gom

n_most_frequent_fhier_10_list_no_na$sa

# Use fhier_mrip_catch_by_species_state_region_waves_no_na again
# make a new column "year_wave"
fhier_mrip_catch_by_species_state_region_waves_no_na_tmp1 <-
  mutate(fhier_mrip_catch_by_species_state_region_waves_no_na,
         year_wave = paste(year, wave, sep = "_"))

# Add the fhier_common_names we made earlier
fhier_mrip_catch_by_species_state_region_waves_no_na_tmp2 <-
  inner_join(fhier_mrip_catch_by_species_state_region_waves_no_na_tmp1,
           fhier_common_names,
           by = join_by(species_itis))

#| warning: false

# Make separate data frames
fhier_mrip_catch_by_species_state_region_waves_no_na_for_plot <-
  fhier_mrip_catch_by_species_state_region_waves_no_na_tmp2 %>%
  # split by sa_gom column
    split(as.factor(fhier_mrip_catch_by_species_state_region_waves_no_na$sa_gom)) %>%
  # remove extra columns in each df
    map(
      .f = list(. %>% dplyr::select(-one_of("year", "wave", "sa_gom")
                                    )
                )
  )

glimpse(fhier_mrip_catch_by_species_state_region_waves_no_na_for_plot)

fhier_mrip_catch_by_species_state_region_waves_no_na_for_plot$sa %>%
  select(species_itis) %>% unique()
# 58
n_most_frequent_fhier_10_list_no_na$sa
setdiff(n_most_frequent_fhier_10_list_no_na$sa$species_itis,
        fhier_mrip_catch_by_species_state_region_waves_no_na_for_plot$sa$species_itis)
# 0

setdiff(fhier_mrip_catch_by_species_state_region_waves_no_na_for_plot$sa$species_itis,
        n_most_frequent_fhier_10_list_no_na$sa$species_itis
        )
# 48

tmp1 <-
inner_join(fhier_mrip_catch_by_species_state_region_waves_no_na_for_plot$sa,
        n_most_frequent_fhier_10_list_no_na$sa)

tmp1 %>%
  select(species_itis) %>% unique()
# 10

#| classes: test

# For each region sum counts for one species,
# should be the same as before

# GOM
fhier_mrip_catch_by_species_state_region_waves_no_na_for_plot$gom %>%
  filter(species_itis == "168853") %>%
  group_by(species_itis) %>%
  summarise(snapper_r_gom_fhier_cnt = sum(fhier_catch_by_4, na.rm = TRUE),
            snapper_r_gom_mrip_cnt = sum(mrip_estimate_catch_by_4, na.rm = TRUE)
            )

# SA
fhier_mrip_catch_by_species_state_region_waves_no_na_for_plot$sa %>%
  filter(species_itis == "168853") %>%
  group_by(species_itis) %>%
  summarise(snapper_r_sa_fhier_cnt = sum(fhier_catch_by_4, na.rm = TRUE),
            snapper_r_sa_mrip_cnt = sum(mrip_estimate_catch_by_4, na.rm = TRUE)
            )

# keep only entries for spp. in the top ten list,
# separately for each region
fhier_mrip_catch_by_species_state_region_waves_no_na_for_plot_gom10 <-
  fhier_mrip_catch_by_species_state_region_waves_no_na_for_plot$gom %>%
  filter(species_itis %in% n_most_frequent_fhier_10_list_no_na$gom$species_itis)

glimpse(fhier_mrip_catch_by_species_state_region_waves_no_na_for_plot_gom10)
# 109 obs. of  6 variables

fhier_mrip_catch_by_species_state_region_waves_no_na_for_plot_sa10 <-
  fhier_mrip_catch_by_species_state_region_waves_no_na_for_plot$sa %>%
  filter(species_itis %in% n_most_frequent_fhier_10_list_no_na$sa$species_itis)

glimpse(fhier_mrip_catch_by_species_state_region_waves_no_na_for_plot_sa10)
# Rows: 140

fhier_mrip_gom__no_na_to_plot <-
  fhier_mrip_catch_by_species_state_region_waves_no_na_for_plot_gom10 %>%
  # change to shorter column names
  rename(c("MRIP" = "mrip_estimate_catch_by_4",
           "FHIER" = "fhier_catch_by_4")) %>%
  # reformat to a long format to have fhier and mrip data side by side
  pivot_longer(
    cols = c(MRIP,
             FHIER),
    names_to = "AGENCY",
    values_to = "CATCH_CNT"
  ) %>%
  # use only the new columns
  select(year_wave, species_itis, common_name, AGENCY, CATCH_CNT) %>%
  # remove lines where one or another agency doesn't have counts for this species
  drop_na()

glimpse(fhier_mrip_gom__no_na_to_plot)

# fhier_mrip_gom__no_na_to_plot %>% select(species_itis) %>% unique()
# 10

# an overview plot
plot(fhier_mrip_gom__no_na_to_plot)

# plot_by_spp("SNAPPER, RED", fhier_mrip_gom__no_na_to_plot)
# fhier_mrip_gom__no_na_to_plot %>%
#   select(common_name) %>% unique()
# 10

           # for each common name from the top 10
plots10 <- map(unique(fhier_mrip_gom__no_na_to_plot$common_name),
              # run the plot_by_spp with this common name as a parameter and the default value for no_legend (TRUE)
               function(x) {plot_by_spp(x, fhier_mrip_gom__no_na_to_plot)}
               )

# Title for all plots together
super_title = "GOM: The top 10 most abundant FHIER species by waves, no NAs"

# separate a legend
plot_w_legend <- plot_by_spp("SNAPPER, RED",
                             fhier_mrip_gom__no_na_to_plot,
                             # keep the legend
                             FALSE)
# use an aux function to pull out the legend
my_legend <- legend_for_grid_arrange(plot_w_legend)

# combine all plots
grid.arrange(grobs = plots10,
             top = super_title,
             left = my_legend,
             ncol = 3)

## SA plots no na ----

fhier_mrip_sa__no_na_to_plot <-
  fhier_mrip_catch_by_species_state_region_waves_no_na_for_plot_sa10 %>%
  # rename to shorter column names
  rename(c("MRIP" = "mrip_estimate_catch_by_4",
           "FHIER" = "fhier_catch_by_4")) %>%
  # reformat to a long format to have fhier and mrip data side by side
  pivot_longer(
    cols = c(MRIP,
             FHIER),
    names_to = "AGENCY",
    values_to = "CATCH_CNT"
  ) %>%
  # use only the new columns
  select(year_wave, species_itis, is_dolphin, AGENCY, CATCH_CNT) %>%
  # remove lines where one or another agency doesn't have counts for this species
  drop_na()

glimpse(fhier_mrip_sa__no_na_to_plot)
# fhier_mrip_sa__no_na_to_plot %>%
#   select(species_itis) %>% unique()
# 10

# An overview plot
plot(fhier_mrip_sa__no_na_to_plot)

           # for each common name from the top 10
plots10 <- map(unique(fhier_mrip_sa__no_na_to_plot$is_dolphin),
              # run the plot_by_spp with this common name as a parameter and the default value for no_legend (TRUE)
               function(x) {plot_by_spp(x, fhier_mrip_sa__no_na_to_plot)}
               )

# The following code is the same as before, with "SA" instead of "GOM"
super_title = "SA: The top 10 most abundant FHIER species by waves, no NAs"

# separate a legend
plot_w_legend <- plot_by_spp("SNAPPER, RED", fhier_mrip_sa__no_na_to_plot, FALSE)
my_legend <- legend_for_grid_arrange(plot_w_legend)

grid.arrange(grobs = plots10,
             top = super_title,
             left = my_legend,
             ncol = 3)

