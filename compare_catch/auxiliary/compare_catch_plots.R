# 3 sets of spp: 
# 1a) SEDAR spp; 
# 2b) Recreational ACL tops; 
# 3c) All FHIER spp

# Plots:
# 1) By wave and region
# 2) By wave and state
# 3) By year and region
# 4) By year and state

### convert to a long format for plotting
to_long_format <- function(my_df) {
  
  my_df %>%
  # change to shorter column names
  rename(c("Rec_ACL" = starts_with("rec_acl"),
           "FHIER" = starts_with("fhier")
           )
         ) %>%
  # reformat to a long format to have fhier and acl data side by side
  pivot_longer(
    cols = c(Rec_ACL,
             FHIER),
    names_to = "ORIGIN",
    values_to = "CATCH_CNT"
  ) %>%
    return()
}

plot_by_time <-
  function(my_df,
           my_title,
           sort_field = "rec_acl_cnts_by_year_reg",
           show_counts = TRUE,
           show_com_names = TRUE,
           show_legend = TRUE) {
    # browser()
    one_plot_by_year <-
      my_df %>%
      # make "common_name_fhier" a factor to keep an order by desc(rec_acl_cnts_by_year_reg)
      mutate(common_name_fhier = reorder(common_name_fhier, desc(!!sym(sort_field)))) %>%
      select(-starts_with("species_itis")) %>%
      to_long_format() %>%
      ggplot(aes(CATCH_CNT, common_name_fhier, fill = ORIGIN)) +
      scale_fill_manual(values = c("Rec_ACL" = "deepskyblue", "FHIER" = "red")) +
      # columns are side by side (not stacked)
      geom_col(position = "dodge") +
      labs(title = my_title,
           y = "") +
      theme(axis.text.y = element_text(size = 6)) +
      theme_bw() +
      my_theme
    
    if (show_counts) {
      one_plot_by_year <-
        one_plot_by_year +
        geom_text(aes(label = CATCH_CNT),
                  size = 3,
                  position = position_dodge(width = 0.9))
    }
    
    if (!show_com_names) {
      one_plot_by_year <-
        one_plot_by_year +
        theme(axis.text.y = element_blank())
    }
    
    if (!show_legend) {
      one_plot_by_year <-
        one_plot_by_year +
        theme(legend.position = "none")
    }
    
    return(one_plot_by_year)
  }

# 1) By wave and region ----
# rename for clarity
fhier_acl_catch_by_species_state_region_waves_list_for_plot <-
  fhier_acl_catch_by_species_state_region_waves_list

## A function to make a plot by spp. ----

my_theme <- theme(
  # turn x text
  axis.text.x = element_text(# angle = 45
    # low it down
    # ,
    vjust = 0.5),
  # change text size
  plot.title = element_text(size = 10),
  legend.title = element_text(size = 8),
  legend.text = element_text(size = 8),
  axis.title = element_text(size = 9)

)

plot_by_spp <- function(my_df, com_name, no_legend = TRUE) {
  # browser()

 one_plot <-
  my_df %>%
    # only get the com name from the parameters
    filter(common_name_fhier == !!com_name) %>%
  ggplot(
         aes(x = wave,
             y = CATCH_CNT,
            # color by the origin
             fill = ORIGIN)
  ) +
    # manually change default colors
    scale_fill_manual(values = c("ACL" = "deepskyblue", "FHIER" = "red")) +
    # columns are side by side (not stacked)
    geom_col(position = "dodge") +
    labs(title = com_name,
        # remove x and y axes titles
         x = "",
         y = ""
    ) +
   scale_x_continuous(n.breaks = 6) +
   # catch_cnt for each bar
   geom_text(aes(label = CATCH_CNT),
             position = position_dodge(width = 0.9),
             vjust = -0.25,
             # size is in mm for geom_bar
             size = 2) +
   # blank theme from ggplot
   theme_bw() +
   my_theme

  # By default the "no_legend" parameter is TRUE
            # make a legend if no_legend is FALSE
  if (no_legend) {
    one_plot <- one_plot +
      theme(legend.position = "none")
  }
  return(one_plot)
}

#| classes: test

### test: For each region sum counts for one species, ----
# should be the same as before

#### GOM fhier test ----
# names(fhier_acl_catch_by_species_state_region_waves_list_for_plot$gom)
fhier_acl_catch_by_species_state_region_waves_list_for_plot$gom %>%
  filter(scientific_name == test_species_name) %>%
  group_by(scientific_name) %>%
  summarise(mackerel_fhier_cnt = sum(fhier_quantity_by_4, na.rm = TRUE)) %>%
  use_series(mackerel_fhier_cnt) %>%
  identical(
    fhier_test_cnts %>%
              filter(sa_gom == "gom") %>%
              select(mackerel_fhier_cnt) %>%
              use_series(mackerel_fhier_cnt)
            )

#### SA sa_acl test ----

  fhier_acl_catch_by_species_state_region_waves_list_for_plot$sa %>%
  filter(scientific_name == test_species_name) %>%
  group_by(scientific_name) %>%
  summarise(mackerel_acl_cnt = sum(rec_acl_estimate_catch_by_4, na.rm = TRUE)) %>%
      use_series(mackerel_acl_cnt) %>%
  identical(
    acl_test_cnts %>%
              filter(sa_gom == "sa") %>%
              select(mackerel_acl_cnt) %>%
              use_series(mackerel_acl_cnt)
            )

# 1) By wave and region 1a) SEDAR ----
## keep only entries for spp. in the top ten list, separately for each region ----
fhier_acl_catch_by_species_state_region_waves_list_for_plot_gom_top_sedar <-
  # fhier_acl_catch_by_species_state_region_waves_list
  fhier_acl_catch_by_species_state_region_waves_list_for_plot$gom %>%
  filter(scientific_name %in% gom_top_spp$scientific_name)
# 231  
# 250
# 258

# str(fhier_acl_catch_by_species_state_region_waves_list_for_plot_gom_top_sedar)
# 'data.frame':	196 obs. of  6 variables
# 'data.frame':	238 obs. of  6 variables (new list)
# 'data.frame':	258 obs. of  9 variables new file and sero only

fhier_acl_catch_by_species_state_region_waves_list_for_plot_sa_sedar <-
  fhier_acl_catch_by_species_state_region_waves_list_for_plot$sa %>%
  filter(scientific_name %in% sa_top_spp$scientific_name)

dim(fhier_acl_catch_by_species_state_region_waves_list_for_plot_sa_sedar)
# Rows: 300
# Rows: 274
# 173
# 228 (new list)
# 206 with combined dolphins
# 179 new file and sero only
# 163 - combined dolphins
fhier_acl_catch_by_species_state_region_waves_list_for_plot_sa_sedar %>%
  select(common_name_fhier) %>% unique() %>% dim()
# 11
# 9

#| classes: test
#### test: For the top 10, for each region sum separately ACL and FHIER counts for one species, ----
# should be the same as before

#### test SA, FHIER counts ----
fhier_acl_catch_by_species_state_region_waves_list_for_plot_sa_sedar %>%
  filter(scientific_name == test_species_name) %>%
  group_by(scientific_name) %>%
  summarise(mackerel_fhier_cnt = sum(fhier_quantity_by_4, na.rm = TRUE)) %>%
  select(mackerel_fhier_cnt) %>%
  use_series(mackerel_fhier_cnt) %>%
  identical(
    fhier_test_cnts %>%
              filter(sa_gom == "sa") %>%
              select(mackerel_fhier_cnt) %>%
              use_series(mackerel_fhier_cnt)
            )

# GOM, FHIER counts
fhier_acl_catch_by_species_state_region_waves_list_for_plot_gom_top_sedar %>%
  filter(scientific_name == test_species_name) %>%
  group_by(scientific_name) %>%
  summarise(mackerel_fhier_cnt = sum(fhier_quantity_by_4, na.rm = TRUE)) %>%
  select(mackerel_fhier_cnt) %>%
  use_series(mackerel_fhier_cnt) %>%
  identical(
    fhier_test_cnts %>%
              filter(sa_gom == "gom") %>%
              select(mackerel_fhier_cnt) %>%
              use_series(mackerel_fhier_cnt)
            )

# SA, ACL counts
fhier_acl_catch_by_species_state_region_waves_list_for_plot_sa_sedar %>%
  filter(scientific_name == test_species_name) %>%
  group_by(scientific_name) %>%
  summarise(mackerel_acl_cnt = sum(rec_acl_estimate_catch_by_4, na.rm = TRUE)) %>%
  select(mackerel_acl_cnt) %>%
  use_series(mackerel_acl_cnt) %>% 
  identical(
    acl_test_cnts %>%
              filter(sa_gom == "sa") %>%
              select(mackerel_acl_cnt) %>%
              use_series(mackerel_acl_cnt)
            )

# GOM, ACL counts
fhier_acl_catch_by_species_state_region_waves_list_for_plot_gom_top_sedar %>%
  filter(scientific_name == test_species_name) %>%
  group_by(scientific_name) %>%
  summarise(mackerel_acl_cnt = sum(rec_acl_estimate_catch_by_4, na.rm = TRUE)) %>%
    select(mackerel_acl_cnt) %>%
  use_series(mackerel_acl_cnt) %>% 
  identical(
    acl_test_cnts %>%
              filter(sa_gom == "gom") %>%
              select(mackerel_acl_cnt) %>%
              use_series(mackerel_acl_cnt)
            )
# TRUE
# all four above should be TRUE

### convert to a long format for plotting
fhier_acl_to_plot_format <- function(my_df) {
  my_df %>%
  # change to shorter column names
  rename(c("ACL" = "rec_acl_estimate_catch_by_4",
           "FHIER" = "fhier_quantity_by_4")) %>%
  # reformat to a long format to have fhier and acl data side by side
  pivot_longer(
    cols = c(ACL,
             FHIER),
    names_to = "ORIGIN",
    values_to = "CATCH_CNT"
  ) %>%
  # use only the new columns
  select(wave, species_itis_fhier, common_name_fhier, ORIGIN, CATCH_CNT) %>%
    group_by(wave, species_itis_fhier, common_name_fhier, ORIGIN) %>%
    summarise(CATCH_CNT = sum(CATCH_CNT)) %>%
    return()
}

## GOM plots ----

fhier_acl_gom_to_plot <-
  fhier_acl_to_plot_format(fhier_acl_catch_by_species_state_region_waves_list_for_plot_gom_top_sedar)

# View(fhier_acl_catch_by_species_state_region_waves_list_for_plot$gom)
# 
# an overview plot
# plot(fhier_acl_gom_to_plot)

# plot_by_spp(fhier_acl_gom_to_plot, "MACKEREL, SPANISH")

### GOM plots for each common name from the top 10 ----
spp_to_plot_gom <- fhier_acl_gom_to_plot$common_name_fhier %>%
  unique() %>%
  # na.omit()
  na.exclude()

plots10_gom <- map(spp_to_plot_gom,
              # run the plot_by_spp with this common name as a parameter and the default value for no_legend (TRUE)
               function(com_name) {plot_by_spp(fhier_acl_gom_to_plot, com_name)}
               )

# Title for all plots together
super_title = "GOM: species counts by waves 2022 (SEDAR spp. list)"

# separate a legend
plot_w_legend_gom <- plot_by_spp(                            fhier_acl_gom_to_plot, "MACKEREL, SPANISH",
                             # keep the legend
                             FALSE)
# use an aux function to pull out the legend
my_legend_gom <- legend_for_grid_arrange(plot_w_legend_gom)

# clean the plate
grid.newpage()
# combine all plots
grid.arrange(grobs = plots10_gom,
             top = super_title,
             left = my_legend_gom,
             ncol = 3)

## SA plots ----

fhier_acl_sa_to_plot <-
  fhier_acl_to_plot_format(fhier_acl_catch_by_species_state_region_waves_list_for_plot_sa_sedar) %>% 
  ungroup()

# glimpse(fhier_acl_sa_to_plot)

# An overview plot
# plot(fhier_acl_sa_to_plot)

           # for each common name from the top 10
sa_plots10 <- map(unique(fhier_acl_sa_to_plot$common_name_fhier),
              # run the plot_by_spp with this common name as a parameter and the default value for no_legend (TRUE)
               function(com_name) {plot_by_spp(fhier_acl_sa_to_plot, com_name)}
               )

# The following code is the same as before, with "SA" instead of "GOM"
sa_super_title = "SA: species counts by waves (SEDAR spp. list)"

#### separate a legend ----
sa_plot_w_legend <- plot_by_spp(fhier_acl_sa_to_plot, "MACKEREL, SPANISH", FALSE)
sa_my_legend <- legend_for_grid_arrange(sa_plot_w_legend)

#### draw all sa plots10 together ----
grid.arrange(grobs = sa_plots10,
             top = sa_super_title,
             left = sa_my_legend,
             ncol = 3)

# source("~/R_code_github/compare_catch_no_na.R")

## Count Index: use a ACL/FHIER count ratio ----
# keep ymax the same across plots

# https://stackoverflow.com/questions/17074189/incorrect-left-align-using-tablegrob
textii <- function(d,
                   gp = gpar(),
                   name = "row-label-",
                   just = "left",
                   parse = TRUE) {
  justification <- switch(just,
              "center" = 0.5,
              "right" = 1,
              "left" = 0)
  parseglobal <- parse
  function(ii, parse = parseglobal) {
    lab <- if (parse)
      parse(text = d[ii])
    else
      d[ii]
    textGrob(
      x = justification,
      label = lab,
      just = just,
      gp = gp,
      name = paste(name, ii, sep = "")
    )
  }
}

# assignInNamespace("textii", textii, "gridExtra")
# grid.table(z, core.just = "left")

## footnote explanation ----
footnote = textGrob(
  "The Ratio is calculated as (acl_cnts - fhier_cnts) / (acl_cnts + fhier_cnts). Hence if the bars are > 0 then ACL estmates > FHIER counts and vice versa.
The smaller the bar is the closer the ACL estmates are to the FHIER counts.",
  gp = gpar(fontface = 3, fontsize = 10),
  # justify left
  hjust = 0,
  x = 0.01, y = 0.99,
  vjust = 1
)

## plot_ind function ----
# map(unique(fhier_acl_gom_ind$common_name)

plot_ind <- function(my_df, com_n, mypalette, no_legend = TRUE) {
  # browser()
  one_ind_plot <-
    my_df %>%
    filter(common_name_fhier == com_n) %>%
    ggplot(aes(x = wave,
               y = cnt_index,
               fill = as.factor(cnt_index)
               )
           ) +
    # geom_col(fill = "deepskyblue") +
      # geom_bar(fill = cnt_index, stat = 'identity') + 
    geom_col() +
    scale_fill_manual(values = mypalette) +
    # scale_fill_viridis_c() +
    theme_bw() +
    my_theme +
        labs(title = com_n,
        # remove x and y axes titles
         x = "",
         y = ""
    ) +
    ylim(-1, 1)
  
  if (no_legend) {
    one_ind_plot <- one_ind_plot +
      theme(legend.position = "none")
  }
  return(one_ind_plot)
}

fhier_acl_catch_by_species_state_region_waves_list_for_plot_gom_top_sedar %>%
  filter(is.na(common_name_fhier)) %>% unique()
# 8

## calculate_cnt_index function ----
calculate_cnt_index <- function(my_df) {
  # browser()
  my_df %>%
    select(wave, common_name_fhier, fhier_quantity_by_4, rec_acl_estimate_catch_by_4) %>%
    # select(-c(state, species_itis_fhier, species_itis_mrip)) %>%
    mutate_all( ~ replace_na(., 0)) %>%
    group_by(wave, common_name_fhier) %>%
    # aggregate counts by states
    summarise(
      fhier_cnts = sum(fhier_quantity_by_4),
      acl_cnts = sum(rec_acl_estimate_catch_by_4)
    ) %>%
    mutate(cnt_index = (acl_cnts - fhier_cnts) /
             (acl_cnts + fhier_cnts)) %>%
    mutate(cnt_index = round(cnt_index, 2)) %>%
    return()
}

### GOM index ----
fhier_acl_gom_ind <- calculate_cnt_index( fhier_acl_catch_by_species_state_region_waves_list_for_plot_gom_top_sedar)
# View(fhier_acl_catch_by_species_state_region_waves_list_for_plot_gom_top_sedar)
# names(fhier_acl_catch_by_species_state_region_waves_list_for_plot_gom_top_sedar)

# glimpse(fhier_acl_gom_ind)
# fhier_acl_gom_ind %>%
#   mutate(wave = strsplit(year_wave, "_")[[1]][[2]]) %>%
#   select(wave) %>% unique()
# 
# fhier_acl_gom_ind <-
#   fhier_acl_gom_ind %>%
#   separate_wider_delim(
#     year_wave,
#     "_",
#     # cols_remove = F,
#     names = c("year", "wave")
#   ) %>%
#   select(-year)
# # %>%
#   # select(year_wave, wave) %>% unique()
# 
# strsplit("2022_1", "_")[[1]][[2]]

### GOM index plots ----
# plot(fhier_acl_gom_ind)

#### palete ----
gom_all_cnt_indexes <- sort(unique(fhier_acl_gom_ind$cnt_index))
q_colors_gom = length(gom_all_cnt_indexes)
mypalette = viridis(q_colors_gom, option = "D")
# mypalette <- rainbow(length(gom_all_cnt_indexes))
names(mypalette) <- gom_all_cnt_indexes
mypalette

# use_wave(fhier_acl_gom_ind) %>% glimpse()

one_plot <- plot_ind(fhier_acl_gom_ind, "MACKEREL, SPANISH", mypalette)

gom_ind_plots <- map(unique(fhier_acl_gom_ind$common_name_fhier),
              # run the plot_ind with this common name as a parameter and the default value for no_legend (TRUE)
               function(x) {plot_ind(fhier_acl_gom_ind, x, mypalette)}
               )

super_title = "GOM Counts Ratio by Wave 2022"

#### draw gom plots ----
grid.newpage()
grid.arrange(grobs = gom_ind_plots,
             top = super_title,
             bottom = footnote,
             # left = my_legend,
             ncol = 3)

### SA index ----
fhier_acl_sa_ind <- calculate_cnt_index(fhier_acl_catch_by_species_state_region_waves_list_for_plot_sa_sedar)

### SA index plots ----
# plot(fhier_acl_sa_ind)

sa_all_cnt_indexes <- sort(unique(fhier_acl_sa_ind$cnt_index))
q_colors_sa = length(sa_all_cnt_indexes)
mypalette = viridis(q_colors_sa, option = "D")
# mypalette <- rainbow(length(gom_all_cnt_indexes))
# names(mypalette) <- gom_all_cnt_indexes
# mypalette <- rainbow(length(fhier_acl_sa_ind$cnt_index))
names(mypalette) <- sa_all_cnt_indexes
mypalette
# mypalette %>% unique()

sa_ind_plots <- map(unique(fhier_acl_sa_ind$common_name_fhier),
              # run the plot_ind with this common name as a parameter and the default value for no_legend (TRUE)
               function(x) {plot_ind(fhier_acl_sa_ind, x, mypalette)}
               )

super_title = "SA Counts Ratio by Wave 2022"

#### draw SA plots ----
grid.newpage()
grid.arrange(grobs = sa_ind_plots,
             top = super_title,
             # left = my_legend,
             bottom = footnote,
             # , padding = unit(1, "line")
             ncol = 3)

## show both types of plot together

ind_grouper_red_eq <- gom_ind_plots[[5]]
ind_mackerel_king_fhier <- gom_ind_plots[[7]]
ind_snapper_red_acl <- gom_ind_plots[[10]]

my_legend_gom_flat <-
  legend_for_grid_arrange(
    plot_w_legend_gom
    # + theme(
    #   legend.box.spacing = margin(0),
    #   legend.spacing = margin(0, 0, 0, 0),
    # )
    + guides(
      fill =
        guide_legend(
          nrow = 1,
          title = NULL,
          hjust = 1,
          direction = "horizontal",
          keyheight = 0.5
        )
    )
  )

grid.newpage()
p1 <- 
  grid.arrange(ind_grouper_red_eq,
             plots10_gom[[1]],
             ind_mackerel_king_fhier,
             plots10_gom[[9]],
             ind_snapper_red_acl,
             plots10_gom[[8]],
             bottom = my_legend_gom_flat
             )

my_legend_gom_vert <-
  legend_for_grid_arrange(plot_w_legend_gom
                          + guides(
                            fill =
                              guide_legend(
                                ncol = 1,
                                title = NULL,
                                # hjust = 1,
                                direction = "vertical"
                              )
                          ))

theme_low <- theme(
  legend.box.spacing = margin(0),
  legend.spacing = margin(0, 0, 0, 0),
  legend.title = element_blank()
)

grid.arrange(
  arrangeGrob(
    ind_grouper_red_eq,
    ind_mackerel_king_fhier,
    ind_snapper_red_acl,
    top = "Ratio",
    bottom = textGrob(
  "The Ratio is calculated as (acl_cnts - fhier_cnts) / (acl_cnts + fhier_cnts)",
  # gp = gpar(fontface = 3, fontsize = 10)
  # ,
  # justify left
  hjust = 0,
  x = 0.01, y = 1.1,
  vjust = 1
)
  ),
  arrangeGrob(plots10_gom[[1]],
              plots10_gom[[9]],
              plots10_gom[[8]],
              top = "Catch Counts",
              bottom = my_legend_gom_flat),
  ncol = 2
)

# 1) By wave and region 2b) Recreational ACL tops ----
## GOM Top ACL species plots ----
# View(fhier_acl_catch_by_species_state_region_waves_list_for_plot$gom)

gom_acl_top_to_plot <-
  fhier_acl_catch_by_species_state_region_waves_list_for_plot$gom %>%
  inner_join(gom_acl_top_spp,
             by = join_by(scientific_name))
# default Joining with `by = join_by(species_itis, common_name)`
# 'data.frame':	225 obs. of  7 variables:
# 305

gom_acl_top_to_plot_longer <- fhier_acl_to_plot_format(gom_acl_top_to_plot)
# View(gom_acl_top_to_plot_longer)

### GOM plots for each common name from the top 10 ----
spp_to_plot_gom_acl_top <- gom_acl_top_to_plot_longer$common_name_fhier %>%
  unique() %>%
  na.exclude()

plots_acl_top_gom <- map(spp_to_plot_gom_acl_top,
              # run the plot_by_spp with this common name as a parameter and the default value for no_legend (TRUE)
               function(com_name) {plot_by_spp(gom_acl_top_to_plot_longer, com_name)}
               )

# plots_acl_top_gom[[2]]
# both_by_to_acl %>%
#   filter(common_name == 'RUNNER, BLUE' &
#            wave == 1)

# Title for all plots together
super_title = "GOM: Top ACL species counts by waves 2022"

# separate a legend
plot_w_legend_gom <-
  plot_by_spp(gom_acl_top_to_plot_longer,
              "MACKEREL, SPANISH",
              # keep the legend
              FALSE)
# use an aux function to pull out the legend
my_legend_gom <- legend_for_grid_arrange(plot_w_legend_gom)

# clean the plate
grid.newpage()
# combine all plots
grid.arrange(grobs = plots_acl_top_gom,
             top = super_title,
             left = my_legend_gom,
             ncol = 4)

# SA Top ACL species plots ----

sa_acl_top_to_plot <-
  fhier_acl_catch_by_species_state_region_waves_list_for_plot$sa %>%
  inner_join(sa_acl_top_spp, by = join_by(scientific_name))

# dim(sa_acl_top_to_plot)
# 'data.frame':	331 obs. of  6 variables:
# 259

sa_acl_top_to_plot_longer <- fhier_acl_to_plot_format(sa_acl_top_to_plot)

# test the longer format transformation ----
sa_acl_top_to_plot %>%
  filter(scientific_name == test_species_name) %>%
  count(acl_count = sum(rec_acl_estimate_catch_by_4)) %>%
  use_series(acl_count) %>%
  identical(
    sa_acl_top_spp %>%
      filter(scientific_name == test_species_name) %>%
      select(acl_count) %>%
      use_series(acl_count)
  )
# T
# 103230

### sa plots for each common name from the top ACL spp ----

plots_acl_top_sa <-
  unique(sa_acl_top_to_plot_longer$common_name_fhier) %>%
  # all names except NA
  na.exclude() %>%
  map(# run the plot_by_spp with this common name as a parameter and the default value for no_legend (TRUE)
    function(com_name) {
      plot_by_spp(sa_acl_top_to_plot_longer, com_name)
    })

# plots_acl_top_sa[[1]]

# Title for all plots together
super_title = "SA: Top ACL species counts by waves 2022"

# separate a legend from one random plot
plot_w_legend_sa <-
  plot_by_spp(sa_acl_top_to_plot_longer,
              "MACKEREL, SPANISH",
              # keep the legend
              FALSE)

# use an aux function to pull out the legend
my_legend_sa <- legend_for_grid_arrange(plot_w_legend_sa)

# clean the plate
grid.newpage()
# combine all plots
grid.arrange(grobs = plots_acl_top_sa,
             top = super_title,
             left = my_legend_sa,
             ncol = 4)

# 1) By wave and region 3c) All FHIER spp ----
#### GOM ----

region_waves_gom_long_wave_list <-
  fhier_acl_catch_by_species_state_region_waves_list_for_plot$gom %>%
  # split by waves column
  split(
    as.factor(
      fhier_acl_catch_by_species_state_region_waves_list_for_plot$gom$wave
    )
  ) %>%
  # remove extra columns in each df
  map(.f = list(. %>% dplyr::select(-one_of("state", "wave"))))

# View(region_waves_gom_long_wave_list)
my_reg <- "GOM"

plots_region_waves_gom_long_wave_list <-
  names(region_waves_gom_long_wave_list) %>%
  map(function(wave_num) {
    # browser()
    region_waves_gom_long_wave_list[[wave_num]] %>%
      unique() %>%
      plot_by_time(
        my_title = paste0(my_reg, " wave ", wave_num, " 2022"),
        sort_field = "rec_acl_estimate_catch_by_4",
        show_counts = FALSE,
        show_com_names = TRUE,
        show_legend = TRUE
      )
  })

# plots_region_waves_gom_long_wave_list[[4]]

names(region_waves_gom_long_wave_list) %>%
  map(function(wave_num) {
    ggsave(
      paste0(my_reg, wave_num, "w.pdf"),
      plots_region_waves_gom_long_wave_list[[as.numeric(wave_num)]],
      width = 20,
      height = 20,
      units = "cm"
    )
  })

#### SA ----

region_waves_sa_long_wave_list <-
  fhier_acl_catch_by_species_state_region_waves_list_for_plot$sa %>%
  # split by waves column
  split(
    as.factor(
      fhier_acl_catch_by_species_state_region_waves_list_for_plot$sa$wave
    )
  ) %>%
  # remove extra columns in each df
  map(.f = list(. %>% dplyr::select(-one_of("state", "wave"))))

# View(region_waves_sa_long_wave_list)
my_reg <- "SA"

plots_region_waves_sa_long_wave_list <-
  names(region_waves_sa_long_wave_list) %>%
  map(function(wave_num) {
    # browser()
    region_waves_sa_long_wave_list[[wave_num]] %>%
      unique() %>%
      plot_by_time(
        my_title = paste0(my_reg, " wave ", wave_num, " 2022"),
        sort_field = "rec_acl_estimate_catch_by_4",
        show_counts = FALSE,
        show_com_names = TRUE,
        show_legend = TRUE
      )
  })

plots_region_waves_sa_long_wave_list[[1]]
# ggsave("1a.png", plots_region_waves_sa_long_wave_list[[1]])
names(region_waves_sa_long_wave_list) %>%
  map(function(wave_num) {
    # browser()
    ggsave(
      paste0(my_reg, wave_num, "w.pdf"),
      plots_region_waves_sa_long_wave_list[[as.numeric(wave_num)]],
      width = 20,
      height = 20,
      units = "cm"
    )
  })

# 2) By wave and state ----
# 2) By wave and state 1a) SEDAR ----
# separate by sa and gom

state_wave_list_state_sedar <-
  # drop "NOT-SPECIFIED"
  map(c("sa", "gom"),
      function(current_sa_gom) {
        # browser()
        fhier_acl_catch_by_species_state_region_waves_states_list[[current_sa_gom]] %>%
          map(function(current_df) {
            current_df %>%
              
              filter(if (current_sa_gom == "gom") {
                scientific_name %in% gom_top_spp$scientific_name
              }
              else if (current_sa_gom == "sa") {
                scientific_name %in% sa_top_spp$scientific_name
              }) %>%
              return()
          }) %>%
          return()
      })

names(state_wave_list_state_sedar) <- c("sa", "gom")
View(state_wave_list_state_sedar)

state_wave_has_rec_acl_data_list_state_sedar <-
  state_wave_list_state_sedar %>%
  map(remove_no_mrip_cnts)

View(state_wave_has_rec_acl_data_list_state_sedar$gom$AL)
state_wave_has_rec_acl_data_list_state_sedar$gom$AL %>%
  select(scientific_name) %>% unique() %>% dim()
# sa$NC 9
# $gom$AL 11

each_state_to_plot <- function(my_df, spp_list) {
  # browser()
  one_st_to_plot <-
    fhier_acl_to_plot_format(my_df)
  
  plots_top <- map(spp_list$common_name,
                   # run the plot_by_spp with this common name as a parameter and the default value for no_legend (TRUE)
                   function(com_name) {
                     plot_by_spp(one_st_to_plot, com_name)
                   })
  return(plots_top)
}

# make a legend
make_a_legend <- function() {
  
  my_state = "FL"
  
  # one plot with a legend
  plot_w_legend_st_sedar <- plot_by_spp(
    fhier_acl_to_plot_format(state_wave_has_rec_acl_data_list_state_sedar$sa[[my_state]]),
    "MACKEREL, SPANISH",
    no_legend = FALSE
  )
  
  # use an aux function to pull out the legend
  my_legend_st_sedar <-
    legend_for_grid_arrange(plot_w_legend_st_sedar)
  
  return(my_legend_st_sedar)
}

one_legend <- make_a_legend()
my_out_dir <- r"(compare_catch\12 categories New\2) By wave and state\1a) SEDAR spp)"

save_plot_to_file <-
  function(current_sa_gom,
           state_abbr,
           combined_plot_for_1_state) {
    output_file_name <-
      paste0("2_1a_",
             current_sa_gom,
             "_",
             state_abbr,
             "_state_wave_sedar",
             ".png")
    
    ggsave(
      file = output_file_name,
      plot = combined_plot_for_1_state,
      device = "png",
      path = file.path(my_paths$outputs,
                       my_out_dir),
      width = 20,
      height = 20,
      units = "cm"
    )
  }

get_current_sedar_list <- function(current_sa_gom) {
  current_top_spp   = gom_top_spp
  if (current_sa_gom == "sa") {
    current_top_spp   = sa_top_spp
  }
  return(current_top_spp)
}

make_one_state_plot <-
  function(state_abbr,
           current_st_df_list,
           current_top_spp,
           current_sa_gom) {
    # browser()
    # get data for this state
    all_plots_for_st <-
      current_st_df_list[[state_abbr]] %>%
      each_state_to_plot(current_top_spp)
    
    super_title_sedar =
      paste(current_sa_gom,
            state_abbr,
            "2022 Counts by State and SEDAR spp. lists")
    
    combined_plot_for_1_state <-
      gridExtra::arrangeGrob(
        grobs = all_plots_for_st,
        top = super_title_sedar,
        left = one_legend,
        ncol = 2
      )
    
    #save each plot to file
    save_plot_to_pdf(current_sa_gom,
                     state_abbr,
                     combined_plot_for_1_state)
    
    return(combined_plot_for_1_state)
  }

state_wave_plots_sedar <-
  # for each region
  map(c("sa", "gom"),
      function(current_sa_gom) {
        # get spp list
        current_top_spp <- get_current_sedar_list(current_sa_gom)
        # get data for this region
        current_st_df_list <-
          state_wave_has_rec_acl_data_list_state_sedar[[current_sa_gom]]
        # make_one_state_plot for each state in that region (for all spp from the list)
        map(
          names(current_st_df_list),
          ~ make_one_state_plot(.x,
                                current_st_df_list,
                                current_top_spp,
                                current_sa_gom)
        )
      })

# to see plots
# grid.arrange(state_wave_plots_sedar[[1]][[1]])

# 2) By wave and state 2b) Recreational ACL tops ----
state_wave_rec_acl_top_list <-
  map(state_wave_has_rec_acl_data_list_new,
      function(by_state_df) {
        # browser()
        by_state_df %>%
          filter(
            scientific_name %in% gom_acl_top_spp$scientific_name |
              scientific_name %in% sa_acl_top_spp$scientific_name
          ) %>%
          return()
      })

View(state_wave_rec_acl_top_list)
state_wave_rec_acl_top_list[["FL"]] %>%
  select(scientific_name) %>% unique() %>% dim()
# 18

state_wave_plots_mrip_top <-
  # has rec_acl data
    names(state_wave_rec_acl_top_list) %>%
  # repeat for each state
  map(function(state_abbr) {
    super_title_state_mrip = paste0(state_abbr, ". Top ACL species counts by state. 2022")
    # browser()
    # get data for this state
    state_wave_rec_acl_top_list[[state_abbr]] %>%
      plot_by_time(
        my_title = super_title_state_mrip,
        sort_field = sort_field_state_wave_plots_sedar,
        show_counts = T,
        show_com_names = T,
        show_legend = T
      )
  })

state_wave_plots_mrip_top[[2]]

# one plot with a legend - use the same

# combine plots and the legend in a list
# gr_list <- c(state_wave_plots_sedar,
#              list(my_legend_st_sedar))

grid.newpage()
gridExtra::grid.arrange(
             grobs = state_wave_plots_sedar,
             top = super_title_sedar,
             left = my_legend_st_sedar,
             ncol = 2)

# 2) By wave and state 3c) All FHIER spp TODO ----

# 3) By year and region ----

## 3) By year and region 1a) SEDAR ----
my_title <- "By year and region SEDAR spp. SA"
fhier_acl_catch_by_species_region_year_list$sa %>%
  filter(scientific_name %in% sa_top_spp$scientific_name) %>% 
  # View()
  plot_by_time(my_title = my_title)

# same ordered by FHIER:
my_title <- "By year and region SEDAR spp. SA, ordered by FHIER cnts"
fhier_acl_catch_by_species_region_year_list$sa %>%
  filter(scientific_name %in% sa_top_spp$scientific_name) %>% 
  # View()
  plot_by_time(my_title = my_title, sort_field = "fhier_cnts_by_year_reg")

my_title <- "By year and region SEDAR spp. GOM"
fhier_acl_catch_by_species_region_year_list$gom %>%
  filter(scientific_name %in% gom_top_spp$scientific_name) %>% 
  # View()
  plot_by_time(my_title = my_title)

## 3) By year and region 2b) Recreational ACL tops ----
### gom ----
my_limit <- 6000
my_reg <- "GOM"
my_title <- paste0(my_reg, " 2022. rec_acl_cnts_by_year_reg > ", my_limit)

fhier_acl_catch_by_species_region_year_list$gom %>%
  # keep only top r_acl cnts
  filter(rec_acl_cnts_by_year_reg > my_limit) %>%
  plot_by_time(my_title = my_title)

### SA ---- 
my_limit <- 2000
my_reg <- "SA"
my_title <- paste0(my_reg, " 2022. rec_acl_cnts_by_year_reg > ", my_limit)

fhier_acl_catch_by_species_region_year_list$sa %>%
  # keep only top r_acl cnts
  filter(rec_acl_cnts_by_year_reg > my_limit) %>%
  plot_by_time(my_title = my_title)

### overview plots ----
fhier_acl_catch_by_species_region_year_list$gom %>%
  select(-common_name_fhier) %>%
  plot(main = "GOM by year")

fhier_acl_catch_by_species_region_year_list$sa %>%
  select(-common_name_fhier) %>%
  plot(main = "SA by year")

## 3) By year and region 3c) All FHIER spp ----
### gom ----
my_reg <- "GOM"
my_title <- paste0(my_reg, " 2022")

fhier_acl_catch_by_species_region_year_list$gom %>%
  plot_by_time(my_title = my_title, show_counts = FALSE)

### SA ---- 
my_reg <- "SA"
my_title <- paste0(my_reg, " 2022")

fhier_acl_catch_by_species_region_year_list$sa %>%
  plot_by_time(my_title = my_title)
# , show_counts = FALSE

### overview plots ----
fhier_acl_catch_by_species_region_year_list$gom %>%
  select(-common_name_fhier) %>%
  plot(main = "GOM by year")

fhier_acl_catch_by_species_region_year_list$sa %>%
  select(-common_name_fhier) %>%
  plot(main = "SA by year")

# 4) By year and state ----
# 4) By year and state 1a) SEDAR ----

# View(
#   fhier_acl_catch_by_species_state_year_list[[state_abbr]] %>%
#     filter(
#       (species_itis %in% gom_top_spp$species_itis)
#       |
#         (species_itis %in% sa_top_spp$species_itis)
#     ) %>%
#   select(common_name_fhier)
# )

View(fhier_acl_catch_by_species_state_year_list)

state_year_plots_sedar <-
  # has rec_acl data
    names(state_year_has_rec_acl_data_list_new) %>%
  # repeat for each state
  map(function(state_abbr) {
    # get data for this state
    fhier_acl_catch_by_species_state_year_list[[state_abbr]] %>%
      # keep only spp in the SEDAR spp lists
      filter(
        scientific_name %in% gom_top_spp$scientific_name |
          scientific_name %in% sa_top_spp$scientific_name
      ) %>%
      plot_by_time(
        my_title = state_abbr,
        sort_field = "rec_acl_sum_cnts",
        show_counts = F,
        show_com_names = T,
        show_legend = F
      )
  })

super_title_sedar = "2022 Counts by State and SEDAR spp. lists"

# one plot with a legend
my_state = "FL"
plot_w_legend_st_sedar <- 
  # data for one state
  fhier_acl_catch_by_species_state_year_list[[my_state]] %>%
  plot_by_time(
        my_title = my_state,
        sort_field = "rec_acl_sum_cnts",
        show_legend = TRUE
      )
  
# use an aux function to pull out the legend
my_legend_st_sedar <- legend_for_grid_arrange(plot_w_legend_st_sedar)

# combine plots and the legend in a list
gr_list <- c(state_year_plots_sedar,
             list(my_legend_st_sedar))

grid.newpage()
gridExtra::grid.arrange(
             # grobs = gr_list,
             grobs = state_year_plots_sedar,
             top = super_title_sedar,
             left = my_legend_st_sedar,
             ncol = 2)

# 4) By year and state 2b) Recreational ACL tops ----
# 
# fhier_acl_catch_by_species_region_year_list$gom %>%
#   # keep only top r_acl cnts
#   filter(rec_acl_cnts_by_year_reg > my_limit) %>%
#   plot_by_year(my_title = my_title)

# View(fhier_acl_catch_by_species_state_year_list[["FL"]])
my_limit <- 2000
small_st <- c("SC", "GA", "MS", "TX")
state_year_top_rec_acl_plots_2k <-
  # has rec_acl data
  small_st %>%
    # names(state_year_has_rec_acl_data_list_new) %>%
  # repeat for each state
  map(function(state_abbr) {
    # get data for this state
    fhier_acl_catch_by_species_state_year_list[[state_abbr]] %>%
#   # keep only top r_acl cnts
    filter(rec_acl_sum_cnts > my_limit) %>%
      plot_by_time(
        my_title = state_abbr,
        sort_field = "rec_acl_sum_cnts",
        show_counts = T,
        show_com_names = T,
        show_legend = F
      )
  })

my_limit <- 4000
big_st <- c("FL", "AL", "NC")
state_year_top_rec_acl_plots_4k <-
  # has rec_acl data
  big_st %>%
    # names(state_year_has_rec_acl_data_list_new) %>%
  # repeat for each state
  map(function(state_abbr) {
    # get data for this state
    fhier_acl_catch_by_species_state_year_list[[state_abbr]] %>%
#   # keep only top r_acl cnts
    filter(rec_acl_sum_cnts > my_limit) %>%
      plot_by_time(
        my_title = state_abbr,
        sort_field = "rec_acl_sum_cnts",
        show_counts = T,
        show_com_names = T,
        show_legend = F
      )
  })


super_title_top_rec_acl = "2022, Top rec ACL Counts by State"

# state_year_top_rec_acl_plots[[5]]

gr_list_top_rec_acl2 <- c(
  state_year_top_rec_acl_plots_2k,
  list(my_legend_st_sedar)
)

grid.newpage()
a <- gridExtra::grid.arrange(
  grobs = state_year_top_rec_acl_plots_4k,
  top = super_title_top_rec_acl,
  ncol = 3,
  nrow = 1,
  widths = c(unit(.4, "npc"), unit(.3, "npc"), unit(.3, "npc"))
)

b <-
  gridExtra::grid.arrange(
    grobs = gr_list_top_rec_acl2,
    ncol = 3,
    widths = c(unit(.45, "npc"), unit(.25, "npc"), unit(.3, "npc"))
  )

gridExtra::grid.arrange(a, b,
                        heights = c(unit(.65, "npc"),
                                    unit(.35, "npc")))

# 4) By year and state 3c) All FHIER spp ----
plot_by_time(fhier_acl_catch_by_species_state_year_list$AL, "AL", sort_field = "rec_acl_sum_cnts", show_counts = FALSE)

# state_year_has_rec_acl_data_list_new
state_year_plots <-
  names(state_year_has_rec_acl_data_list_new) %>%
  # repeat for each state
  map(function(state_abbr) {
    # get data for this state
    fhier_acl_catch_by_species_state_year_list[[state_abbr]] %>%
      # filter(fhier_sum_cnts > 2000) %>%
      # filter(rec_acl_sum_cnts > 2000) %>%
      plot_by_time(my_title = state_abbr, sort_field = "rec_acl_sum_cnts", show_counts = F, show_com_names = FALSE)
  })

super_title_y_st = "2022 Counts by State and spp."
grid.arrange(grobs = state_year_plots,
             top = super_title_y_st,
             # left = my_legend_gom,
             ncol = 2)

# state_year_plots[[7]]


#### 0 counts? ----
View(fhier_acl_catch_by_species_state_region_waves_list_for_plot$gom)

fhier_acl_catch_by_species_state_region_waves_list_for_plot$gom %>%
  filter(fhier_quantity_by_4 == 0 &
           rec_acl_estimate_catch_by_4 == 0) %>%
  count(scientific_name)
  # scientific_name  n
# 1       167793  1 sand perch
# 2       168559  2 bluefish
# 3       169056  3 invalid - other, see comments 	Pomadasyidae, grunts 
# 4       169539  2 Atlantic spadefish
# 5         <NA> 40
# common_name is NA

#   count(scientific_name)
#          scientific_name  n
# 1 ANISOTREMUS VIRGINICUS  1
# 2           CARANX RUBER  3
# 3   CHAETODIPTERUS FABER  2
# 4    DIPLECTRUM FORMOSUM  1
# 5    POMATOMUS SALTATRIX  2
# 6           SERIOLA SPP.  4
# 7                   <NA> 48
