## No NAs ----

dim(fhier_mrip_catch_by_species_state_region_waves)
# [1] 5728    7

fhier_mrip_catch_by_species_state_region_waves_no_na <-
fhier_mrip_catch_by_species_state_region_waves %>%
  filter(complete.cases(.))
# 'data.frame':	786 obs. of  7 variables:

# fhier_mrip_catch_by_species_state_region_waves %>% 
#   tidyr::drop_na(mrip_estimate_catch_by_4,
#                  # 878 obs. of  7 variables
#                  fhier_quantity_by_4) %>% str()
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
      select(species_itis, fhier_quantity_by_4) %>%
      group_by(species_itis) %>%
      # add a new column with a sum of counts for each spp.
      summarise(fhier_catch_by_spp = sum(fhier_quantity_by_4, na.rm = TRUE)) %>%
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
  filter(species_itis == test_species_itis) %>%
  group_by(species_itis) %>%
  summarise(mackerel_gom_fhier_cnt = sum(fhier_quantity_by_4, na.rm = TRUE),
            mackerel_gom_mrip_cnt = sum(mrip_estimate_catch_by_4, na.rm = TRUE)
            )

# SA
fhier_mrip_catch_by_species_state_region_waves_no_na_for_plot$sa %>%
  filter(species_itis == test_species_itis) %>%
  group_by(species_itis) %>%
  summarise(mackerel_sa_fhier_cnt = sum(fhier_quantity_by_4, na.rm = TRUE),
            mackerel_sa_mrip_cnt = sum(mrip_estimate_catch_by_4, na.rm = TRUE)
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
           "FHIER" = "fhier_quantity_by_4")) %>%
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

# plot_by_spp("MACKEREL, SPANISH", fhier_mrip_gom__no_na_to_plot)
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
plot_w_legend <- plot_by_spp("MACKEREL, SPANISH",
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

fhier_mrip_sa__no_na_to_plot <-
  fhier_mrip_catch_by_species_state_region_waves_no_na_for_plot_sa10 %>%
  # rename to shorter column names
  rename(c("MRIP" = "mrip_estimate_catch_by_4",
           "FHIER" = "fhier_quantity_by_4")) %>%
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

glimpse(fhier_mrip_sa__no_na_to_plot)
# fhier_mrip_sa__no_na_to_plot %>%
#   select(species_itis) %>% unique()
# 10

# An overview plot
plot(fhier_mrip_sa__no_na_to_plot)

           # for each common name from the top 10
plots10 <- map(unique(fhier_mrip_sa__no_na_to_plot$common_name),
              # run the plot_by_spp with this common name as a parameter and the default value for no_legend (TRUE)
               function(x) {plot_by_spp(x, fhier_mrip_sa__no_na_to_plot)}
               )

# The following code is the same as before, with "SA" instead of "GOM"
super_title = "SA: The top 10 most abundant FHIER species by waves, no NAs"

# separate a legend
plot_w_legend <- plot_by_spp("MACKEREL, SPANISH", fhier_mrip_sa__no_na_to_plot, FALSE)
my_legend <- legend_for_grid_arrange(plot_w_legend)

grid.arrange(grobs = plots10,
             top = super_title,
             left = my_legend,
             ncol = 3)

