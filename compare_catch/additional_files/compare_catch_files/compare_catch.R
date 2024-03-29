##| echo: false
library(zoo)
library(gridExtra)

# include auxilary functions ----
source("~/R_code_github/useful_functions_module.r")
my_paths <- set_work_dir()

##| echo: false

source("~/R_code_github/compare_catch/get_data.R")

## prepare FHIER data ----

## get column names vars ----
# There are different formats in different available files.
# Find a column name with "itis" in it
itis_field_name <- grep("itis", names(logbooks_content), value = T)
# catch_species_itis

# Same for "vessel.*official"
vessel_id_field_name <-
  grep("vessel.*official", names(logbooks_content), value = T)
# vessel_official_nbr

## Fix dates ----

fhier_logbooks_content <-
  logbooks_content  %>%
  # create a new column
  dplyr::mutate(trip_start_date_time =
    # trip start: combine a date without time, a space and a time
    paste(substr(trip_start_date, 1, 10),
    trip_start_time)) %>%
  # Same for the trip end
  dplyr::mutate(trip_end_date_time = paste(substr(trip_end_date, 1, 10), trip_end_time)) %>%
  # change the new column types to a date
  change_to_dates("trip_start_date_time", "%Y-%m-%d %H%M") %>%
  change_to_dates("trip_end_date_time", "%Y-%m-%d %H%M") %>%
  # change the column type to a number
  dplyr::mutate(reported_quantity = as.integer(reported_quantity))

# view
fhier_logbooks_content %>% dplyr::select(starts_with("trip")) %>% str()

fhier_logbooks_content_date_fixed_tmp <-
  fhier_logbooks_content %>%
  # if a "trip_end_date" is before 2020 - use "notif_trip_end_date" column instead
  dplyr::mutate(trip_end_date1 = ifelse(
    trip_end_date < "2020-01-01",
    notif_trip_end_date,
    trip_end_date
  ))

fhier_logbooks_content_date_fixed <-
  fhier_logbooks_content_date_fixed_tmp %>%
  # manually change the wrong value
  dplyr::mutate(trip_end_date2 = ifelse(
    # find it
    grepl("1992", fhier_logbooks_content_date_fixed_tmp$trip_end_date1),
    # change it
    "2022-10-16 01:00:00",
    # don't change anything else
    trip_end_date1
  ))

fhier_logbooks_content_date_fixed %<>%
  dplyr::filter(year(trip_end_date) == "2022")

fhier_logbooks_content_waves <-
  fhier_logbooks_content_date_fixed %>%
  # add a new column with a trip end Month
  dplyr::mutate(end_month = as.yearmon(trip_end_date2)) %>%
  # add a new column with a trip end Year
  dplyr::mutate(end_year =
           year(trip_end_date2)) %>%
  # add a new column with a number for each trip end Month
  dplyr::mutate(end_month_num = month(trip_end_date2)) %>%
  # add a new column with a Wave
  dplyr::mutate(end_wave  = floor((end_month_num + 1) / 2))

#| classes: test

# test: show the new columns ----
fhier_logbooks_content_waves %>%
  dplyr::select(end_month, end_year, end_month_num, end_wave) %>%
  unique() %>%
  # sort by end_month_num
  dplyr::arrange(end_month_num)

# Florida counties by region (from the Internet) ----
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
  dplyr::mutate(
    end_port_fl_reg = dplyr::case_when(
      # check in the list
      fix_names(end_port_county) %in% fix_names(fl_counties$SA) ~ "sa",
      fix_names(end_port_county) %in% fix_names(fl_counties$GOM) ~ "gom",
      # if not on the list - keep it
      .default = end_port_county
    )
  )

## test: check regions ----
fhier_logbooks_content_waves_fl_county %>%
  # get FL only
  dplyr::filter(end_port_state == "FL") %>%
  # sort by county
  dplyr::arrange(end_port_county) %>%
  dplyr::distinct() %>%
  # 37 counties
  dplyr::select(end_port_fl_reg) %>%
  # what else is in the new column beside sa and gom
  dplyr::filter(!(end_port_fl_reg %in% c("sa", "gom"))) %>% unique()

# NOT-SPECIFIED

## states to regions ----
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
  dplyr::filter(state_name %in% tolower(states_sa$state_name)) %>%
  # get abbreviations
  dplyr::select(state_abb)

fhier_logbooks_content_waves__sa_gom <-
  fhier_logbooks_content_waves_fl_county %>%
  # add a new column "end_port_sa_gom" with sa or gom for each state
  # use fix_name aux function to unify state names (lower case, no spaces etc.)
  dplyr::mutate(end_port_sa_gom = dplyr::case_when(
    # if a name is in our SA list - "sa", otherwise - "gom"
    fix_names(end_port_state) %in% fix_names(sa_state_abb$state_abb) ~ "sa",
    .default = "gom"
  )) %>%
  # go through the new column again
  # if an end port state is Florida - use the region from the previous step (column "end_port_fl_reg")
  # otherwise don't change
  dplyr::mutate(end_port_sa_gom = ifelse(
    tolower(end_port_state) == "fl",
    end_port_fl_reg,
    end_port_sa_gom
  )) %>%
  # remove this column, we don't need it anymore
  dplyr::select(-end_port_fl_reg)

#| classes: test
## test: states and regions ----
fhier_logbooks_content_waves__sa_gom %>%
  # look at states and regions
  dplyr::select(end_port_state, end_port_sa_gom) %>%
  unique() %>%
  dplyr::glimpse()

glimpse(fhier_logbooks_content_waves__sa_gom)

## combine dolphin and dolphinfish for FHIER data ----
fhier_logbooks_content_waves__sa_gom_dolph <-
  fhier_logbooks_content_waves__sa_gom %>%
  rename(common_name_orig = common_name) %>%
  dplyr::mutate(common_name = if_else(
    tolower(common_name_orig) %in% c("dolphin", "dolphinfish"),
    "DOLPHIN",
    common_name_orig
    )
  )

glimpse(fhier_logbooks_content_waves__sa_gom_dolph)

### test: dolphins ----
fhier_logbooks_content_waves__sa_gom_dolph %>%
  dplyr::filter(tolower(common_name_orig) %in% c("dolphin", "dolphinfish")) %>%
  dplyr::select(common_name_orig, common_name) %>% unique()
# ---

## calculate catch ----

fhier_catch_by_species_state_region_waves <-  
  fhier_logbooks_content_waves__sa_gom %>%
  # dplyr::select only relevant columns
  dplyr::select(
    catch_species_itis,
    end_port_state,
    end_port_sa_gom,
    end_year,
    end_wave,
    reported_quantity
  ) %>%
  # group by all of them but "reported_quantity"
  dplyr::group_by(
    catch_species_itis,
    end_port_state,
    end_port_sa_gom,
    end_year,
    end_wave
  ) %>%
  # save a sum of reported_quantity in each group in fhier_quantity_by_4
  # remove NAs
  summarise(fhier_quantity_by_4 = sum(as.integer(reported_quantity), na.rm = TRUE)) %>%
  as.data.frame()

#| classes: test
### test: cnts for 1 sp. ----
test_species_itis <-
  fhier_logbooks_content %>%
  dplyr::filter(tolower(common_name) == "mackerel, spanish") %>%
  dplyr::select(catch_species_itis) %>%
  unique() %>%
  # get a string, not a df
  use_series(catch_species_itis)

fhier_test_cnts <-
  fhier_catch_by_species_state_region_waves %>%
  # get the same species
  dplyr::filter(catch_species_itis == test_species_itis) %>%
  # group by region
  dplyr::group_by(catch_species_itis, end_port_sa_gom) %>%
  # sum the FHIER catch
  summarise(mackerel_fhier_cnt = sum(fhier_quantity_by_4, na.rm = TRUE)) %>%
  as.data.frame()

## MRIP ----

mrip_estimate %<>%
  dplyr::mutate(ab1 = as.integer(ab1))

mrip_estimate_catch_by_species_state_region_waves <-
  mrip_estimate %>%
  # dplyr::select the relevan columns only
  dplyr::select(itis_code, new_sta, sub_reg, year, wave, ab1) %>%
  # group by all except the counts
  dplyr::group_by(itis_code, new_sta, sub_reg, year, wave) %>%
  # save the sum of "ab1" for each group in "mrip_estimate_catch_by_4"
  # remove NAs
  summarise(mrip_estimate_catch_by_4 = sum(as.integer(ab1), na.rm = TRUE)) %>%
  as.data.frame()

glimpse(mrip_estimate_catch_by_species_state_region_waves)
# 'data.frame':	878 obs. of  6 variables

# "year" and "wave" to numbers
mrip_estimate_catch_by_species_state_region_waves1 <-
  mrip_estimate_catch_by_species_state_region_waves %>%
  dplyr::mutate(year = as.double(year)) %>%
  dplyr::mutate(wave = as.double(wave))

mrip_estimate_catch_by_species_state_region_waves <-
  mrip_estimate_catch_by_species_state_region_waves1 %>%
  # change a 6 to "sa" and a 7 "gom", leave everything else in place
  dplyr::mutate(sa_gom = dplyr::case_when(sub_reg == "6" ~ "sa",
                            sub_reg == "7" ~ "gom",
                            .default = sub_reg),
                            # put the new column after sub_reg (by default at the end)
                            .after = sub_reg) %>%
  # drop sub_reg
  dplyr::select(-sub_reg)

### make a test mrip one sp. var ----
# names(mrip_estimate_catch_by_species_state_region_waves)
mrip_test_cnts <-
  mrip_estimate_catch_by_species_state_region_waves %>%
  # get one species
  dplyr::filter(itis_code == test_species_itis) %>%
  # group by region
  dplyr::group_by(itis_code, sa_gom) %>%
  # sum the MRIP catch
  summarise(mackerel_mrip_cnt = sum(mrip_estimate_catch_by_4, na.rm = TRUE)) %>%
  as.data.frame()

## rename fields ----

# common field names
wave_data_names_common <- c("species_itis",
                     "state",
                     "sa_gom",
                     "year",
                     "wave"
                    )

# to be sure columns are in the same order
names(mrip_estimate_catch_by_species_state_region_waves)

mrip_names <- c("itis_code",
                "new_sta",
                "sa_gom",
                "year",
                "wave",
                "mrip_estimate_catch_by_4"
)


mrip_estimate_catch_by_species_state_region_waves %<>%
  rename_at(vars(mrip_names[1:2]), function(x) wave_data_names_common[1:2])

# names(fhier_catch_by_species_state_region_waves) %>% cat()
fhier_names <- c(
  "catch_species_itis",
  "end_port_state",
  "end_port_sa_gom",
  "end_year",
  "end_wave",
  "fhier_quantity_by_4")
  
fhier_catch_by_species_state_region_waves %<>%
  rename_at(vars(fhier_names[1:5]),
            function(x) wave_data_names_common[1:5])
# %>% head(100) %>% tail(2)

### rename fields in the test variables ----
names(fhier_test_cnts) <- c("species_itis", "sa_gom", "mackerel_fhier_cnt")

#was: "catch_species_itis" "end_port_sa_gom"    "mackerel_fhier_cnt"

# names(mrip_test_cnts)

### test: rename fields ----
identical(names(fhier_catch_by_species_state_region_waves)[1:5],
          names(mrip_estimate_catch_by_species_state_region_waves)[1:5])

## Join Fhier and MRIP ---- 
fhier_mrip_catch_by_species_state_region_waves <-
  full_join(fhier_catch_by_species_state_region_waves,
             mrip_estimate_catch_by_species_state_region_waves,
              by = join_by(species_itis, state, sa_gom, year, wave)
             )
# default Joining with `by = join_by(species_itis, state, sa_gom, year, wave)`
# final$S1 <- dplyr::coalesce(final[[" S1 .x"]],  final[[" S1 .y"]])

### test join ---- 
# look at the first 20 entries for mackerel spanish
fhier_mrip_catch_by_species_state_region_waves %>%
  dplyr::filter(species_itis == test_species_itis) %>% head(20)

#| classes: test
### test one sp in MRIP ----

#| classes: test
#### compare the saved numbers with those in the join, they should be the same ----
# names(fhier_mrip_catch_by_species_state_region_waves)
fhier_mrip_catch_by_species_state_region_waves %>%
  dplyr::filter(species_itis == test_species_itis) %>%
  dplyr::group_by(species_itis, sa_gom) %>%
  summarise(mackerel_fhier_cnt = sum(fhier_quantity_by_4, na.rm = TRUE)) %>%
  use_series(mackerel_fhier_cnt) %>% 
  identical(fhier_test_cnts$mackerel_fhier_cnt)

# mrip_test_cnts
# fhier_test_cnts

fhier_mrip_catch_by_species_state_region_waves %>%
  dplyr::filter(species_itis == test_species_itis) %>%
  dplyr::group_by(species_itis, sa_gom) %>%
  summarise(mackerel_mrip_cnt = sum(mrip_estimate_catch_by_4, na.rm = TRUE)) %>%
  use_series(mackerel_mrip_cnt) %>%
  identical(mrip_test_cnts$mackerel_mrip_cnt)

## save common names and itis in a separate data frame ----
fhier_common_names <-
  fhier_logbooks_content %>%
  # names()
  dplyr::select(catch_species_itis, common_name) %>%
  unique()

# add column names
names(fhier_common_names) <- c("species_itis", "common_name")

# grep("grouper, black", fhier_common_names$common_name, value = T, ignore.case = T)

# "DOLPHINFISH",
sa_top <- c(
"BASS, BLACK SEA",
"DOLPHIN",
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
  dplyr::filter(common_name %in% sa_top)

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
  dplyr::filter(common_name %in% gom_top)

glimpse(gom_top_spp)
  
## A function to make a plot by spp. ----

my_theme45 <- theme(
    # turn x text
      axis.text.x = element_text(angle = 45),
    # change text size
      plot.title = element_text(size = 9),
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 8)
   )

plot_by_spp <- function(com_name, my_df, no_legend = TRUE) {
  # browser()

 one_plot <-
  my_df %>%
    # only the com name from the parameters
    dplyr::filter(common_name == !!com_name) %>%
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
   my_theme45
   # theme(
   #  # turn x text
   #    axis.text.x = element_text(angle = 45),
   #  # change text size
   #    plot.title = element_text(size = 9),
   #    legend.title = element_text(size = 8),
   #    legend.text = element_text(size = 8)
   # )

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
# Rows: 5,728

## make a new column "year_wave" ----
fhier_mrip_catch_by_species_state_region_waves_tmp1 <-
  dplyr::mutate(fhier_mrip_catch_by_species_state_region_waves,
         year_wave = paste(year, wave, sep = "_"))

## Add the fhier_common_names we made earlier ----
fhier_mrip_catch_by_species_state_region_waves_tmp2 <-
  inner_join(fhier_mrip_catch_by_species_state_region_waves_tmp1,
           fhier_common_names,
           by = join_by(species_itis))

#| warning: false

## Make separate data frames ----
fhier_mrip_catch_by_species_state_region_waves_list_for_plot <-
  fhier_mrip_catch_by_species_state_region_waves_tmp2 %>%
  # split by sa_gom column
    split(as.factor(fhier_mrip_catch_by_species_state_region_waves$sa_gom)) %>%
  # remove extra columns in each df
    purrr::map(
      .f = list(. %>% dplyr::select(-one_of("year", "wave", "sa_gom")
                                    )
                )
  )

glimpse(fhier_mrip_catch_by_species_state_region_waves_list_for_plot)

#| classes: test

### test: For each region sum counts for one species, ----
# should be the same as before

#### GOM fhier test ----
# names(fhier_mrip_catch_by_species_state_region_waves_list_for_plot$gom)
fhier_mrip_catch_by_species_state_region_waves_list_for_plot$gom %>%
  dplyr::filter(species_itis == test_species_itis) %>%
  dplyr::group_by(species_itis) %>%
  summarise(mackerel_fhier_cnt = sum(fhier_quantity_by_4, na.rm = TRUE)) %>%
  use_series(mackerel_fhier_cnt) %>%
  identical(
    fhier_test_cnts %>%
              dplyr::filter(sa_gom == "gom") %>%
              dplyr::select(mackerel_fhier_cnt) %>%
              use_series(mackerel_fhier_cnt)
            )

#### SA sa_mrip test

  fhier_mrip_catch_by_species_state_region_waves_list_for_plot$sa %>%
  dplyr::filter(species_itis == test_species_itis) %>%
  dplyr::group_by(species_itis) %>%
  summarise(mackerel_mrip_cnt = sum(mrip_estimate_catch_by_4, na.rm = TRUE)) %>%
      use_series(mackerel_mrip_cnt) %>%
  identical(
    mrip_test_cnts %>%
              dplyr::filter(sa_gom == "sa") %>%
              dplyr::select(mackerel_mrip_cnt) %>%
              use_series(mackerel_mrip_cnt)
            )

## keep only entries for spp. in the top ten list, separately for each region ----
fhier_mrip_catch_by_species_state_region_waves_list_for_plot_gom10 <-
  fhier_mrip_catch_by_species_state_region_waves_list_for_plot$gom %>%
  dplyr::filter(species_itis %in% gom_top_spp$species_itis)
# 231  
# dplyr::filter(species_itis %in% n_most_frequent_fhier_10_list$gom$species_itis)
# Rows: 217
# str(fhier_mrip_catch_by_species_state_region_waves_list_for_plot_gom10)
# ?'data.frame':	196 obs. of  6 variables
# 'data.frame':	238 obs. of  6 variables (new list)

fhier_mrip_catch_by_species_state_region_waves_list_for_plot_sa10 <-
  fhier_mrip_catch_by_species_state_region_waves_list_for_plot$sa %>%
  dplyr::filter(species_itis %in% sa_top_spp$species_itis) %>%
  dplyr::mutate()
  
glimpse(fhier_mrip_catch_by_species_state_region_waves_list_for_plot_sa10)
# Rows: 300
# Rows: 274
# 173
# 228 (new list)
# 206 with combined dolphins

# fhier_mrip_catch_by_species_state_region_waves_list_for_plot_sa10 %>% dplyr::select(common_name) %>% unique()

#| classes: test

#### test: For the top 10, for each region sum separately MRIP and FHIER counts for one species, ----
# should be the same as before

#### test SA, FHIER counts
fhier_mrip_catch_by_species_state_region_waves_list_for_plot_sa10 %>%
  dplyr::filter(species_itis == test_species_itis) %>%
  dplyr::group_by(species_itis) %>%
  summarise(mackerel_fhier_cnt = sum(fhier_quantity_by_4, na.rm = TRUE)) %>%
  dplyr::select(mackerel_fhier_cnt) %>%
  use_series(mackerel_fhier_cnt) %>%
  identical(
    fhier_test_cnts %>%
              dplyr::filter(sa_gom == "sa") %>%
              dplyr::select(mackerel_fhier_cnt) %>%
              use_series(mackerel_fhier_cnt)
            )

# GOM, FHIER counts

fhier_mrip_catch_by_species_state_region_waves_list_for_plot_gom10 %>%
  dplyr::filter(species_itis == test_species_itis) %>%
  dplyr::group_by(species_itis) %>%
  summarise(mackerel_fhier_cnt = sum(fhier_quantity_by_4, na.rm = TRUE)) %>%
  dplyr::select(mackerel_fhier_cnt) %>%
  use_series(mackerel_fhier_cnt) %>%
  identical(
    fhier_test_cnts %>%
              dplyr::filter(sa_gom == "gom") %>%
              dplyr::select(mackerel_fhier_cnt) %>%
              use_series(mackerel_fhier_cnt)
            )


# SA, MRIP counts
fhier_mrip_catch_by_species_state_region_waves_list_for_plot_sa10 %>%
  dplyr::filter(species_itis == test_species_itis) %>%
  dplyr::group_by(species_itis) %>%
  summarise(mackerel_mrip_cnt = sum(mrip_estimate_catch_by_4, na.rm = TRUE)) %>%
  dplyr::select(mackerel_mrip_cnt) %>%
  use_series(mackerel_mrip_cnt) %>% 
  identical(
    mrip_test_cnts %>%
              dplyr::filter(sa_gom == "sa") %>%
              dplyr::select(mackerel_mrip_cnt) %>%
              use_series(mackerel_mrip_cnt)
            )

# GOM, MRIP counts
fhier_mrip_catch_by_species_state_region_waves_list_for_plot_gom10 %>%
  dplyr::filter(species_itis == test_species_itis) %>%
  dplyr::group_by(species_itis) %>%
  summarise(mackerel_mrip_cnt = sum(mrip_estimate_catch_by_4, na.rm = TRUE)) %>%
    dplyr::select(mackerel_mrip_cnt) %>%
  use_series(mackerel_mrip_cnt) %>%
  identical(
    mrip_test_cnts %>%
              dplyr::filter(sa_gom == "gom") %>%
              dplyr::select(mackerel_mrip_cnt) %>%
              use_series(mackerel_mrip_cnt)
            )

# numbers are OK

### convert to a long format for plotting
fhier_mrip_to_plot_format <- function(my_df) {
  my_df %>%
  # change to shorter column names
  rename(c("MRIP" = "mrip_estimate_catch_by_4",
           "FHIER" = "fhier_quantity_by_4")) %>%
  # reformat to a long format to have fhier and mrip data side by side
  tidyr::pivot_longer(
    cols = c(MRIP,
             FHIER),
    names_to = "AGENCY",
    values_to = "CATCH_CNT"
  ) %>%
  # use only the new columns
  dplyr::select(year_wave, species_itis, common_name, AGENCY, CATCH_CNT) %>%
    return()
}


## GOM plots ----

### using drop_na ----

fhier_mrip_gom_to_plot <- fhier_mrip_to_plot_format(fhier_mrip_catch_by_species_state_region_waves_list_for_plot_gom10) %>%
  # remove lines where one or another agency doesn't have counts for this species
  drop_na()

# all.equal(fhier_mrip_gom_to_plot, fhier_mrip_gom_to_plota)

glimpse(fhier_mrip_gom_to_plot)

###  NA to 0 ---- 
fhier_mrip_gom_to_plot_0 <-
  fhier_mrip_to_plot_format(fhier_mrip_catch_by_species_state_region_waves_list_for_plot_gom10) %>%
  # change NAs to 0 where one or another agency doesn't have counts for this species
  dplyr::mutate_all(~replace_na(., 0))

# all.equal(fhier_mrip_gom_to_plot_0, fhier_mrip_gom_to_plot_0a)

glimpse(fhier_mrip_gom_to_plot_0)

## test: compare drop_na and 0 ----
all.equal(
  fhier_mrip_gom_to_plot_0 %>%
    dplyr::arrange(species_itis, year_wave, AGENCY) %>%
    slice(1:300)
  ,
  fhier_mrip_gom_to_plot %>%
    dplyr::arrange(species_itis, year_wave, AGENCY) %>%
    slice(1:300)
  )

# [1] "Attributes: < Component “row.names”: Numeric: lengths (476, 358) differ >" # [5] "Component “CATCH_CNT”: Mean relative difference: 2.276243"

mean(fhier_mrip_gom_to_plot$CATCH_CNT)
# [1] 5605.33
mean(fhier_mrip_gom_to_plot_0$CATCH_CNT)
# [1] 4215.773

# an overview plot
plot(fhier_mrip_gom_to_plot)

# plot_by_spp("MACKEREL, SPANISH", fhier_mrip_gom_to_plot)

           # for each common name from the top 10
plots10 <- purrr::map(unique(fhier_mrip_gom_to_plot$common_name),
              # run the plot_by_spp with this common name as a parameter and the default value for no_legend (TRUE)
               function(x) {plot_by_spp(x, fhier_mrip_gom_to_plot)}
               )


# Title for all plots together
super_title = "GOM: species counts by waves"

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

## SA plots ----

fhier_mrip_sa_to_plot <-
  fhier_mrip_to_plot_format(fhier_mrip_catch_by_species_state_region_waves_list_for_plot_sa10) %>%
  # remove lines where one or another agency doesn't have counts for this species
  drop_na()

# all.equal(fhier_mrip_sa_to_plot, fhier_mrip_sa_to_plot_a)

glimpse(fhier_mrip_sa_to_plot)

# An overview plot
plot(fhier_mrip_sa_to_plot)

           # for each common name from the top 10
plots10 <- purrr::map(unique(fhier_mrip_sa_to_plot$common_name),
              # run the plot_by_spp with this common name as a parameter and the default value for no_legend (TRUE)
               function(x) {plot_by_spp(x, fhier_mrip_sa_to_plot)}
               )

# The following code is the same as before, with "SA" instead of "GOM"
super_title = "SA: species counts by waves"

# separate a legend
plot_w_legend <- plot_by_spp("MACKEREL, SPANISH", fhier_mrip_sa_to_plot, FALSE)
my_legend <- legend_for_grid_arrange(plot_w_legend)

grid.arrange(grobs = plots10,
             top = super_title,
             left = my_legend,
             ncol = 3)


# source("~/R_code_github/compare_catch_no_na.R")

## use a MRIP/FHIER count ratio ----
# keep ymax the same across plots

fhier_mrip_gom_ind <-
  fhier_mrip_catch_by_species_state_region_waves_list_for_plot_gom10 %>%
  dplyr::select(-c(state, species_itis)) %>%
  dplyr::mutate_all(~ replace_na(., 0)) %>%
  dplyr::group_by(year_wave, common_name) %>%
  # aggregate counts by states
  summarise(fhier_cnts = sum(fhier_quantity_by_4),
            mrip_cnts = sum(mrip_estimate_catch_by_4 )) %>%
  dplyr::mutate(
    cnt_index = (mrip_cnts - fhier_cnts) /
      (mrip_cnts + fhier_cnts)
  ) %>%
  dplyr::mutate(cnt_index = round(cnt_index, 2)) %>%
  # head()
  # dplyr::mutate(m_f_ratio = round(mrip_estimate_catch_by_4 / fhier_quantity_by_4, 2))
  # x = fct_rev(fct_reorder(common_name,
  #                                  !!sym(count_field_name),
  #                                  .fun = max)),
  dplyr::mutate(common_name_order = fct_reorder(
    common_name,
    (mrip_cnts + fhier_cnts),
  )) %>%
  # dplyr::mutate(date_order = order(year_wave)
  # )
  dplyr::mutate(dates_index = as.factor(year_wave),
    year_wave = factor(year_wave, levels = unique(dates_index))
  )

# fhier_mrip_gom_ind %>% dplyr::select(year_wave, date_order, dates_index) %>% head()

## temp: 1 sp.
# length(fhier_mrip_gom_ind1$year_wave)
fhier_mrip_gom_ind1 %>%
  ggplot(aes(x = year_wave,
           y = cnt_index) +
         geom_point())

str(fhier_mrip_gom_ind1)
fhier_mrip_gom_ind1 %>%
ggplot(
         aes(x = year_wave,
             y = cnt_index,
             color = cnt_index
            ),
  ) +
    # geom_col(position = "dodge")
  geom_point()

plot(fhier_mrip_gom_ind)

# purrr::map(unique(fhier_mrip_gom_ind$common_name)
plot_ind <- function(my_df, com_n) {
  my_df %>%
    dplyr::filter(common_name == com_n) %>%
    ggplot(aes(x = year_wave,
               y = cnt_index,
               color = cnt_index),) +
    geom_point() +
    ylim(-1, 1)
}

gom_ind_plots <- purrr::map(unique(fhier_mrip_gom_ind$common_name),
    function(com_n) {
      # head(x)
      # plot_by_spp(x, fhier_mrip_gom_to_plot)
      fhier_mrip_gom_ind %>%
        dplyr::filter(common_name == com_n) %>%
        ggplot(
         aes(x = year_wave,
             y = cnt_index,
             color = cnt_index
            ),
  ) +
    # geom_col(position = "dodge")
  geom_point() +
        my_theme45 +
        ylim(-1, 1)
            })

# str(gom_ind_plots[[1]])
# library('grid')

# grid.newpage()
# grid.draw(cbind(
#   ggplotGrob(gom_ind_plots[[1]]),
#   ggplotGrob(gom_ind_plots[[2]]),
#   size = "last"
# ))

grid.arrange(grobs = gom_ind_plots,
             # top = super_title,
             # left = my_legend,
             ncol = 3)

fhier_mrip_gom_ind1 <-
  fhier_mrip_gom_ind %>%
  as.data.frame() %>%
  dplyr::filter(common_name == "MACKEREL, SPANISH")
# %>%
  # dplyr::select(year_wave, cnt_index)
# %>%
#   summary()
  # plot()
  
  # as.data.frame() %>%
  # dplyr::arrange(year_wave)

plot(fhier_mrip_gom_ind1$year_wave,
     fhier_mrip_gom_ind1$cnt_index)

