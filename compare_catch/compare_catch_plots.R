# 3 sets of spp: 
# 1a) SEDAR; 
# 2b) Recreational ACL tops; 
# 3c) All FHIER spp

# Plots:
# 1) By wave and region
# 2) By wave and state
# 3) By year and region
# 4) By year and state

# 1) By wave and region ----
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
  plot.title = element_text(size = 9),
  legend.title = element_text(size = 8),
  legend.text = element_text(size = 8)
)

plot_by_spp <- function(com_name, my_df, no_legend = TRUE) {
  # browser()

 one_plot <-
  my_df %>%
    # only get the com name from the parameters
    filter(common_name == !!com_name) %>%
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
  filter(species_itis == test_species_itis) %>%
  group_by(species_itis) %>%
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
  filter(species_itis == test_species_itis) %>%
  group_by(species_itis) %>%
  summarise(mackerel_acl_cnt = sum(acl_estimate_catch_by_4, na.rm = TRUE)) %>%
      use_series(mackerel_acl_cnt) %>%
  identical(
    acl_test_cnts %>%
              filter(sa_gom == "sa") %>%
              select(mackerel_acl_cnt) %>%
              use_series(mackerel_acl_cnt)
            )

# 1) By wave and region 1a) SEDAR ----
## keep only entries for spp. in the top ten list, separately for each region ----
fhier_acl_catch_by_species_state_region_waves_list_for_plot_gom10 <-
  fhier_acl_catch_by_species_state_region_waves_list_for_plot$gom %>%
  filter(species_itis %in% gom_top_spp$species_itis)
# 231  
# filter(species_itis %in% n_most_frequent_fhier_10_list$gom$species_itis)
# Rows: 217
# str(fhier_acl_catch_by_species_state_region_waves_list_for_plot_gom10)
# ?'data.frame':	196 obs. of  6 variables
# 'data.frame':	238 obs. of  6 variables (new list)

fhier_acl_catch_by_species_state_region_waves_list_for_plot_sa10 <-
  fhier_acl_catch_by_species_state_region_waves_list_for_plot$sa %>%
  filter(species_itis %in% sa_top_spp$species_itis)
# %>%
#   mutate()
  
glimpse(fhier_acl_catch_by_species_state_region_waves_list_for_plot_sa10)
# Rows: 300
# Rows: 274
# 173
# 228 (new list)
# 206 with combined dolphins

# fhier_acl_catch_by_species_state_region_waves_list_for_plot_sa10 %>% select(common_name) %>% unique()

#| classes: test

#### test: For the top 10, for each region sum separately ACL and FHIER counts for one species, ----
# should be the same as before

#### test SA, FHIER counts ----
fhier_acl_catch_by_species_state_region_waves_list_for_plot_sa10 %>%
  filter(species_itis == test_species_itis) %>%
  group_by(species_itis) %>%
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

fhier_acl_catch_by_species_state_region_waves_list_for_plot_gom10 %>%
  filter(species_itis == test_species_itis) %>%
  group_by(species_itis) %>%
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
fhier_acl_catch_by_species_state_region_waves_list_for_plot_sa10 %>%
  filter(species_itis == test_species_itis) %>%
  group_by(species_itis) %>%
  summarise(mackerel_acl_cnt = sum(acl_estimate_catch_by_4, na.rm = TRUE)) %>%
  select(mackerel_acl_cnt) %>%
  use_series(mackerel_acl_cnt) %>% 
  identical(
    acl_test_cnts %>%
              filter(sa_gom == "sa") %>%
              select(mackerel_acl_cnt) %>%
              use_series(mackerel_acl_cnt)
            )

# GOM, ACL counts
fhier_acl_catch_by_species_state_region_waves_list_for_plot_gom10 %>%
  filter(species_itis == test_species_itis) %>%
  group_by(species_itis) %>%
  summarise(mackerel_acl_cnt = sum(acl_estimate_catch_by_4, na.rm = TRUE)) %>%
    select(mackerel_acl_cnt) %>%
  use_series(mackerel_acl_cnt) %>%
  identical(
    acl_test_cnts %>%
              filter(sa_gom == "gom") %>%
              select(mackerel_acl_cnt) %>%
              use_series(mackerel_acl_cnt)
            )

# numbers are OK

### convert to a long format for plotting
fhier_acl_to_plot_format <- function(my_df) {
  my_df %>%
  # change to shorter column names
  rename(c("ACL" = "acl_estimate_catch_by_4",
           "FHIER" = "fhier_quantity_by_4")) %>%
  # reformat to a long format to have fhier and acl data side by side
  pivot_longer(
    cols = c(ACL,
             FHIER),
    names_to = "ORIGIN",
    values_to = "CATCH_CNT"
  ) %>%
  # use only the new columns
  select(wave, species_itis, common_name, ORIGIN, CATCH_CNT) %>%
    group_by(wave, species_itis, common_name, ORIGIN) %>%
    summarise(CATCH_CNT = sum(CATCH_CNT)) %>%
    return()
}

## GOM plots ----

fhier_acl_gom_to_plot <-
  fhier_acl_to_plot_format(fhier_acl_catch_by_species_state_region_waves_list_for_plot_gom10)
# 
# an overview plot
plot(fhier_acl_gom_to_plot)

# plot_by_spp("MACKEREL, SPANISH", fhier_acl_gom_to_plot)

### GOM plots for each common name from the top 10 ----
plots10_gom <- map(unique(fhier_acl_gom_to_plot$common_name),
              # run the plot_by_spp with this common name as a parameter and the default value for no_legend (TRUE)
               function(x) {plot_by_spp(x, fhier_acl_gom_to_plot)}
               )

# Title for all plots together
super_title = "GOM: species counts by waves 2022 (SEDAR spp. list)"

# separate a legend
plot_w_legend_gom <- plot_by_spp("MACKEREL, SPANISH",
                             fhier_acl_gom_to_plot,
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
  fhier_acl_to_plot_format(fhier_acl_catch_by_species_state_region_waves_list_for_plot_sa10) %>% 
  ungroup()

# glimpse(fhier_acl_sa_to_plot)

# An overview plot
# plot(fhier_acl_sa_to_plot)

           # for each common name from the top 10
sa_plots10 <- map(unique(fhier_acl_sa_to_plot$common_name),
              # run the plot_by_spp with this common name as a parameter and the default value for no_legend (TRUE)
               function(x) {plot_by_spp(x, fhier_acl_sa_to_plot)}
               )

# The following code is the same as before, with "SA" instead of "GOM"
sa_super_title = "SA: species counts by waves (SEDAR spp. list)"

#### separate a legend ----
sa_plot_w_legend <- plot_by_spp("MACKEREL, SPANISH", fhier_acl_sa_to_plot, FALSE)
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
    filter(common_name == com_n) %>%
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

## calculate_cnt_index function ----
calculate_cnt_index <- function(my_df) {
  my_df %>%
    select(-c(state, species_itis)) %>%
    mutate_all( ~ replace_na(., 0)) %>%
    group_by(year_wave, common_name) %>%
    # aggregate counts by states
    summarise(
      fhier_cnts = sum(fhier_quantity_by_4),
      acl_cnts = sum(acl_estimate_catch_by_4)
    ) %>%
    mutate(cnt_index = (acl_cnts - fhier_cnts) /
             (acl_cnts + fhier_cnts)) %>%
    mutate(cnt_index = round(cnt_index, 2)) %>%
    return()
}

### GOM index ----
fhier_acl_gom_ind <- calculate_cnt_index(fhier_acl_catch_by_species_state_region_waves_list_for_plot_gom10)

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

one_plot <- plot_ind(use_wave(fhier_acl_gom_ind), "MACKEREL, SPANISH", mypalette)

gom_ind_plots <- map(unique(fhier_acl_gom_ind$common_name),
              # run the plot_ind with this common name as a parameter and the default value for no_legend (TRUE)
               function(x) {plot_ind(use_wave(fhier_acl_gom_ind), x, mypalette)}
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
fhier_acl_sa_ind <- calculate_cnt_index(fhier_acl_catch_by_species_state_region_waves_list_for_plot_sa10)

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

sa_ind_plots <- map(unique(fhier_acl_sa_ind$common_name),
              # run the plot_ind with this common name as a parameter and the default value for no_legend (TRUE)
               function(x) {plot_ind(use_wave(fhier_acl_sa_ind), x, mypalette)}
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

# 1) By wave and region 2b) Recreational ACL tops; ----
## GOM Top ACL species plots ----
# View(fhier_acl_catch_by_species_state_region_waves_list_for_plot$gom)

gom_acl_top_to_plot <-
  fhier_acl_catch_by_species_state_region_waves_list_for_plot$gom %>%
  inner_join(gom_acl_top_common_names)
# Joining with `by = join_by(species_itis, common_name)`
# 'data.frame':	225 obs. of  7 variables:

gom_acl_top_to_plot_longer <- fhier_acl_to_plot_format(gom_acl_top_to_plot)
View(gom_acl_top_to_plot_longer)

### GOM plots for each common name from the top 10 ----
plots_acl_top_gom <- map(unique(gom_acl_top_to_plot_longer$common_name),
              # run the plot_by_spp with this common name as a parameter and the default value for no_legend (TRUE)
               function(x) {plot_by_spp(x, gom_acl_top_to_plot_longer)}
               )

# plots_acl_top_gom[[2]]
# both_by_to_acl %>%
#   filter(common_name == 'RUNNER, BLUE' &
#            wave == 1)

# Title for all plots together
super_title = "GOM: Top ACL species counts by waves 2022"

# separate a legend
plot_w_legend_gom <- plot_by_spp("MACKEREL, SPANISH",
                             gom_acl_top_to_plot_longer,
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
  inner_join(sa_acl_top_common_names)
# Joining with `by = join_by(species_itis, common_name)`
# str(sa_acl_top_to_plot)
# 'data.frame':	331 obs. of  6 variables:
str(sa_acl_top_common_names)
sa_acl_top_to_plot_longer <- fhier_acl_to_plot_format(sa_acl_top_to_plot)

# test the longer format transformation ----
# View(sa_acl_top_to_plot)
# sa_acl_top_to_plot %>% 
#   filter(species_itis == '173138') %>%
# count(acl_count = sum(acl_estimate_catch_by_4)) %>% head()
# # 41469 
# filter(sa_acl_top_spp, species_itis == '173138')
# # 41469 


### sa plots for each common name from the top ACL spp ----
plots_acl_top_sa <- map(unique(sa_acl_top_to_plot_longer$common_name),
              # run the plot_by_spp with this common name as a parameter and the default value for no_legend (TRUE)
               function(x) {plot_by_spp(x, sa_acl_top_to_plot_longer)}
               )

# plots_acl_top_sa[[1]]

# Title for all plots together
super_title = "SA: Top ACL species counts by waves 2022"

# separate a legend from one random plot
plot_w_legend_sa <- plot_by_spp("MACKEREL, SPANISH",
                             sa_acl_top_to_plot_longer,
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
# GOM
# fhier_acl_catch_by_species_state_region_waves_list_for_plot$gom %>%
#   View()
# SA

## plots by waves / states ----
# 1a) SEDAR
# 2b) Recreational ACL tops
# 3c) All FHIER spp

# fhier_acl_catch_by_species_state_region_waves_states_list

# fhier_acl_catch_by_species_region_year_list$gom 
# fhier_acl_catch_by_species_region_year_list$sa

# my_df <- fhier_acl_catch_by_species_region_year_list$gom
# ### convert to a long format for plotting

# 3c) All FHIER spp by year / region ----
# TODO split by spp.? or by cnts?
# >600000
# 200000 to 500000
# 100000 to 200000
# <=100000

# my_df_long <-
#   my_df %>%
#   # change to shorter column names
#   rename(c("ACL" = "rec_acl_sum_cnts",
#            "FHIER" = "fhier_sum_cnts")) %>%
#   # reformat to a long format to have fhier and acl data side by side
#   pivot_longer(
#     cols = c(ACL,
#              FHIER),
#     names_to = "ORIGIN",
#     values_to = "CATCH_CNT"
#   ) %>%
#   # use only the new columns
#   select(common_name, ORIGIN, CATCH_CNT) %>%
#     group_by(common_name, ORIGIN) %>%
#     summarise(CATCH_CNT = sum(CATCH_CNT))

# my_df_long %>%
#   ggplot(
#          aes(x = common_name,
#              y = CATCH_CNT,
#             # color by the origin
#              fill = ORIGIN)
#   ) +
#     # manually change default colors
#     scale_fill_manual(values = c("ACL" = "deepskyblue", "FHIER" = "red")) +
#     # columns are side by side (not stacked)
#     geom_col(position = "dodge") +
#     labs(title = "com_name",
#         # remove x and y axes titles
#          x = "",
#          y = ""
#     ) +
#    # catch_cnt for each bar
#    geom_text(aes(label = CATCH_CNT),
#              position = position_dodge(width = 0.9),
#              vjust = -0.25,
#              # size is in mm for geom_bar
#              size = 2) +
#    # blank theme from ggplot
#    theme_bw() +
#    my_theme


# 1) By wave and region 1a) SEDAR; ----
# 1) By wave and region 2b) Recreational ACL tops; ----
# 1) By wave and region 3c) All FHIER spp; ----

# 2) By wave and state 1a) SEDAR
# 2) By wave and state 2b) Recreational ACL tops
# 2) By wave and state 3c) All FHIER spp


# 3) By year and region ----
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

plot_by_year <- function(my_df, my_title) {
  my_df %>%
    # make "common_name" a factor to keep an order by desc(rec_acl_cnts_by_year_reg)
    mutate(common_name = reorder(common_name, desc(rec_acl_cnts_by_year_reg))) %>%
    select(-species_itis) %>%
    to_long_format() %>%
    ggplot(aes(CATCH_CNT, common_name, fill = ORIGIN)) +
    scale_fill_manual(values = c("Rec_ACL" = "deepskyblue", "FHIER" = "red")) +
    # columns are side by side (not stacked)
    geom_col(position = "dodge") +
    labs(title = my_title,
         y = "") +
    geom_text(aes(label = CATCH_CNT),
              size = 3,
              position = position_dodge(width = 0.9)) %>%
    return()
}

## 3) By year and region 3c) All FHIER spp ----
### gom ----
my_reg <- "GOM"
my_title <- paste0(my_reg, " 2022")

fhier_acl_catch_by_species_region_year_list$gom %>%
  plot_by_year(my_title = my_title)

### SA ---- 
my_reg <- "SA"
my_title <- paste0(my_reg, " 2022")

fhier_acl_catch_by_species_region_year_list$sa %>%
  plot_by_year(my_title = my_title)

### overview plots ----
fhier_acl_catch_by_species_region_year_list$gom %>%
  select(-common_name) %>%
  plot(main = "GOM by year")

fhier_acl_catch_by_species_region_year_list$sa %>%
  select(-common_name) %>%
  plot(main = "SA by year")

## 3) By year and region 1a) SEDAR ----
my_title <- "By year and region SEDAR spp. SA"
fhier_acl_catch_by_species_region_year_list$sa %>%
  filter(species_itis %in% sa_top_spp$species_itis) %>% 
  # View()
  plot_by_year(my_title = my_title)

my_title <- "By year and region SEDAR spp. GOM"
fhier_acl_catch_by_species_region_year_list$gom %>%
  filter(species_itis %in% gom_top_spp$species_itis) %>% 
  # View()
  plot_by_year(my_title = my_title)


## 3) By year and region 2b) Recreational ACL tops ----
### gom ----
my_limit <- 6000
my_reg <- "GOM"
my_title <- paste0(my_reg, " 2022. rec_acl_cnts_by_year_reg > ", my_limit)

fhier_acl_catch_by_species_region_year_list$gom %>%
  # keep only top r_acl cnts
  filter(rec_acl_cnts_by_year_reg > my_limit) %>%
  plot_by_year(my_title = my_title)

### SA ---- 
my_limit <- 2000
my_reg <- "SA"
my_title <- paste0(my_reg, " 2022. rec_acl_cnts_by_year_reg > ", my_limit)

fhier_acl_catch_by_species_region_year_list$sa %>%
  # keep only top r_acl cnts
  filter(rec_acl_cnts_by_year_reg > my_limit) %>%
  plot_by_year(my_title = my_title)

fhier_acl_catch_by_species_region_year_list$gom %>%
  select(-common_name) %>%
  plot(main = "GOM by year")

fhier_acl_catch_by_species_region_year_list$sa %>%
  select(-common_name) %>%
  plot(main = "SA by year")

# 4) By year and state 1a) SEDAR ----
# 4) By year and state 2b) Recreational ACL tops ----
# 4) By year and state 3c) All FHIER spp ----
