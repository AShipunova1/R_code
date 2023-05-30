# separate by sa, gom top acl ---
fhier_acl_catch_by_species_state_region_waves_states_list_acl_top_spp_sa_gom <-
  # drop "NOT-SPECIFIED"
  map(c("sa", "gom"),
      function(current_sa_gom) {
        # browser()
        fhier_acl_catch_by_species_state_region_waves_states_list[[current_sa_gom]] %>%
          map(function(current_df) {
            current_df %>%
              
              filter(if (current_sa_gom == "gom") {
                scientific_name %in% gom_acl_top_spp$scientific_name
              }
              else if (current_sa_gom == "sa") {
                scientific_name %in% sa_acl_top_spp$scientific_name
              }) %>%
              return()
          }) %>%
          return()
      })

names(fhier_acl_catch_by_species_state_region_waves_states_list_acl_top_spp_sa_gom) <- c("sa", "gom")
View(fhier_acl_catch_by_species_state_region_waves_states_list_acl_top_spp_sa_gom)


# all.equal(state_wave_list_state_sedar,
          # fhier_acl_catch_by_species_state_region_waves_states_list_acl_top_spp_sa_gom
          # )

fhier_acl_catch_by_species_state_region_waves_states_list_acl_top_spp_sa_gom$sa$NC %>%
  select(scientific_name) %>% unique() %>% dim()
# sa$NC 12
# $gom$AL 11

# make a legend
make_a_legend <- function(my_df) {
  # browser()
  my_state = "FL"
  
  my_df_state <- my_df[["sa"]][[my_state]]
  my_df_state_in_long_format <-
    fhier_acl_to_plot_format(my_df_state)
  
  # one plot with a legend
  plot_w_legend_st_sedar <-
    plot_by_spp(my_df_state_in_long_format,
                # fhier_acl_to_plot_format(state_wave_has_rec_acl_data_list_state_sedar$sa[[my_state]]),
                "MACKEREL, SPANISH",
                no_legend = FALSE)
  
  # use an aux function to pull out the legend
  my_legend_st_sedar <-
    legend_for_grid_arrange(plot_w_legend_st_sedar)
  
  return(my_legend_st_sedar)
}

one_legend <- make_a_legend(fhier_acl_catch_by_species_state_region_waves_states_list_acl_top_spp_sa_gom)
my_out_dir <- r"(compare_catch\12 categories New\2) By wave and state\2b) Recreational ACL tops)"

get_current_acl_list <- function(current_sa_gom) {
  current_top_spp   = gom_acl_top_spp
  if (current_sa_gom == "sa") {
    current_top_spp = sa_acl_top_spp
  }
  return(current_top_spp)
}

state_wave_plots_rec_acl_top <-
  # for each region
  map(c("sa", "gom"),
      function(current_sa_gom) {
        # get spp list
        current_top_spp <- get_current_acl_list(current_sa_gom)
        # get data for this region
        current_st_df_list <-
          state_wave_has_rec_acl_data_list_state_rec_acl_top[[current_sa_gom]]
        # make_one_state_plot for each state in that region (for all spp from the list)
        map(
          names(current_st_df_list),
          ~ make_one_state_plot(.x,
                                current_st_df_list,
                                current_top_spp,
                                current_sa_gom,
                                "2022 Counts by State and rec_acl_top spp. lists")
        )
      })
