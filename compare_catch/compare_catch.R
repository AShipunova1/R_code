##| echo: false
library(zoo)
library(gridExtra)
library(grid)
# install.packages("viridis")
library(viridis)

# include auxilary functions ----
source("~/R_code_github/useful_functions_module.r")
my_paths <- set_work_dir()

##| echo: false

source("~/R_code_github/compare_catch/compare_catch_data_preparation.R")

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
  filter(species_itis == test_species_itis) %>% head(20)

#| classes: test
### test one sp in MRIP ----

#| classes: test
#### compare the saved numbers with those in the join, they should be the same ----
# names(fhier_mrip_catch_by_species_state_region_waves)
fhier_mrip_catch_by_species_state_region_waves %>%
  filter(species_itis == test_species_itis) %>%
  group_by(species_itis, sa_gom) %>%
  summarise(mackerel_fhier_cnt = sum(fhier_quantity_by_4, na.rm = TRUE)) %>%
  use_series(mackerel_fhier_cnt) %>% 
  identical(fhier_test_cnts$mackerel_fhier_cnt)

# mrip_test_cnts
# fhier_test_cnts

fhier_mrip_catch_by_species_state_region_waves %>%
  filter(species_itis == test_species_itis) %>%
  group_by(species_itis, sa_gom) %>%
  summarise(mackerel_mrip_cnt = sum(mrip_estimate_catch_by_4, na.rm = TRUE)) %>%
  use_series(mackerel_mrip_cnt) %>%
  identical(mrip_test_cnts$mackerel_mrip_cnt)

## save common names and itis in a separate data frame ----
fhier_common_names <-
  fhier_logbooks_content %>%
  # names()
  select(catch_species_itis, common_name) %>%
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

glimpse(gom_top_spp)
  
## an aux function to use only a wave from year_wave
use_wave <- function(my_df) {
  my_df %>%
    separate_wider_delim(year_wave,
                         delim = "_",
                         names = c("year", "wave")) %>%
    select(-year) %>%
    return()
}

## A function to make a plot by spp. ----

my_theme <- theme(
  # turn x text
  axis.text.x = element_text(# angle = 45
    # low it down
    # ,
    vjust = 0.5),
  
  # + opts(axis.title.x = theme_text(vjust=-0.5))
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
    filter(common_name == !!com_name) %>%
  ggplot(
         aes(x = wave,
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
   scale_x_continuous(n.breaks = 6) +
   # blank theme from ggplot
   theme_bw() +
   my_theme

  # By default the "no_legend" parameter is TRUE
  if (no_legend) {
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
  mutate(fhier_mrip_catch_by_species_state_region_waves,
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
    map(
      .f = list(. %>% dplyr::select(-one_of("year", "sa_gom")
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

#### SA sa_mrip test

  fhier_mrip_catch_by_species_state_region_waves_list_for_plot$sa %>%
  filter(species_itis == test_species_itis) %>%
  group_by(species_itis) %>%
  summarise(mackerel_mrip_cnt = sum(mrip_estimate_catch_by_4, na.rm = TRUE)) %>%
      use_series(mackerel_mrip_cnt) %>%
  identical(
    mrip_test_cnts %>%
              filter(sa_gom == "sa") %>%
              select(mackerel_mrip_cnt) %>%
              use_series(mackerel_mrip_cnt)
            )

## keep only entries for spp. in the top ten list, separately for each region ----
fhier_mrip_catch_by_species_state_region_waves_list_for_plot_gom10 <-
  fhier_mrip_catch_by_species_state_region_waves_list_for_plot$gom %>%
  filter(species_itis %in% gom_top_spp$species_itis)
# 231  
# filter(species_itis %in% n_most_frequent_fhier_10_list$gom$species_itis)
# Rows: 217
# str(fhier_mrip_catch_by_species_state_region_waves_list_for_plot_gom10)
# ?'data.frame':	196 obs. of  6 variables
# 'data.frame':	238 obs. of  6 variables (new list)

fhier_mrip_catch_by_species_state_region_waves_list_for_plot_sa10 <-
  fhier_mrip_catch_by_species_state_region_waves_list_for_plot$sa %>%
  filter(species_itis %in% sa_top_spp$species_itis) %>%
  mutate()
  
glimpse(fhier_mrip_catch_by_species_state_region_waves_list_for_plot_sa10)
# Rows: 300
# Rows: 274
# 173
# 228 (new list)
# 206 with combined dolphins

# fhier_mrip_catch_by_species_state_region_waves_list_for_plot_sa10 %>% select(common_name) %>% unique()

#| classes: test

#### test: For the top 10, for each region sum separately MRIP and FHIER counts for one species, ----
# should be the same as before

#### test SA, FHIER counts
fhier_mrip_catch_by_species_state_region_waves_list_for_plot_sa10 %>%
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

fhier_mrip_catch_by_species_state_region_waves_list_for_plot_gom10 %>%
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


# SA, MRIP counts
fhier_mrip_catch_by_species_state_region_waves_list_for_plot_sa10 %>%
  filter(species_itis == test_species_itis) %>%
  group_by(species_itis) %>%
  summarise(mackerel_mrip_cnt = sum(mrip_estimate_catch_by_4, na.rm = TRUE)) %>%
  select(mackerel_mrip_cnt) %>%
  use_series(mackerel_mrip_cnt) %>% 
  identical(
    mrip_test_cnts %>%
              filter(sa_gom == "sa") %>%
              select(mackerel_mrip_cnt) %>%
              use_series(mackerel_mrip_cnt)
            )

# GOM, MRIP counts
fhier_mrip_catch_by_species_state_region_waves_list_for_plot_gom10 %>%
  filter(species_itis == test_species_itis) %>%
  group_by(species_itis) %>%
  summarise(mackerel_mrip_cnt = sum(mrip_estimate_catch_by_4, na.rm = TRUE)) %>%
    select(mackerel_mrip_cnt) %>%
  use_series(mackerel_mrip_cnt) %>%
  identical(
    mrip_test_cnts %>%
              filter(sa_gom == "gom") %>%
              select(mackerel_mrip_cnt) %>%
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
  pivot_longer(
    cols = c(MRIP,
             FHIER),
    names_to = "AGENCY",
    values_to = "CATCH_CNT"
  ) %>%
  # use only the new columns
  select(wave, species_itis, common_name, AGENCY, CATCH_CNT) %>%
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
  mutate_all(~replace_na(., 0))

# all.equal(fhier_mrip_gom_to_plot_0, fhier_mrip_gom_to_plot_0a)

glimpse(fhier_mrip_gom_to_plot_0)

## test: compare drop_na and 0 ----
all.equal(
  fhier_mrip_gom_to_plot_0 %>%
    arrange(species_itis, wave, AGENCY) %>%
    slice(1:300)
  ,
  fhier_mrip_gom_to_plot %>%
    arrange(species_itis, wave, AGENCY) %>%
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

### GOM plots for each common name from the top 10 ----
plots10_gom <- map(unique(fhier_mrip_gom_to_plot$common_name),
              # run the plot_by_spp with this common name as a parameter and the default value for no_legend (TRUE)
               function(x) {plot_by_spp(x, fhier_mrip_gom_to_plot)}
               )


# Title for all plots together
super_title = "GOM: species counts by waves 2022"

# separate a legend
plot_w_legend_gom <- plot_by_spp("MACKEREL, SPANISH",
                             fhier_mrip_gom_to_plot,
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

fhier_mrip_sa_to_plot <-
  fhier_mrip_to_plot_format(fhier_mrip_catch_by_species_state_region_waves_list_for_plot_sa10) %>%
  # remove lines where one or another agency doesn't have counts for this species
  drop_na()

# all.equal(fhier_mrip_sa_to_plot, fhier_mrip_sa_to_plot_a)

glimpse(fhier_mrip_sa_to_plot)

# An overview plot
plot(fhier_mrip_sa_to_plot)

           # for each common name from the top 10
plots10 <- map(unique(fhier_mrip_sa_to_plot$common_name),
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

## Count Index: use a MRIP/FHIER count ratio ----
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
  "The Ratio is calculated as (mrip_cnts - fhier_cnts) / (mrip_cnts + fhier_cnts). Hence if the bars are > 0 then FHIER counts < MRIP estmates and vice versa.
The smaller the bar is the closer the MRIP estmates are to the FHIER counts.",
  gp = gpar(fontface = 3, fontsize = 10),
  # justify left
  hjust = 0,
  x = 0.01, y = 0.99,
  vjust = 1
)

## plot_ind function ----
# map(unique(fhier_mrip_gom_ind$common_name)

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
      mrip_cnts = sum(mrip_estimate_catch_by_4)
    ) %>%
    mutate(cnt_index = (mrip_cnts - fhier_cnts) /
             (mrip_cnts + fhier_cnts)) %>%
    mutate(cnt_index = round(cnt_index, 2)) %>%
    return()
}

### GOM index ----
fhier_mrip_gom_ind <- calculate_cnt_index(fhier_mrip_catch_by_species_state_region_waves_list_for_plot_gom10)

# glimpse(fhier_mrip_gom_ind)
# fhier_mrip_gom_ind %>%
#   mutate(wave = strsplit(year_wave, "_")[[1]][[2]]) %>%
#   select(wave) %>% unique()
# 
# fhier_mrip_gom_ind <-
#   fhier_mrip_gom_ind %>%
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
# plot(fhier_mrip_gom_ind)

#### palete ----
gom_all_cnt_indexes <- sort(unique(fhier_mrip_gom_ind$cnt_index))
q_colors_gom = length(gom_all_cnt_indexes)
mypalette = viridis(q_colors_gom, option = "D")
# mypalette <- rainbow(length(gom_all_cnt_indexes))
names(mypalette) <- gom_all_cnt_indexes
mypalette

# use_wave(fhier_mrip_gom_ind) %>% glimpse()

one_plot <- plot_ind(use_wave(fhier_mrip_gom_ind), "MACKEREL, SPANISH", mypalette)

gom_ind_plots <- map(unique(fhier_mrip_gom_ind$common_name),
              # run the plot_ind with this common name as a parameter and the default value for no_legend (TRUE)
               function(x) {plot_ind(use_wave(fhier_mrip_gom_ind), x, mypalette)}
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
fhier_mrip_sa_ind <- calculate_cnt_index(fhier_mrip_catch_by_species_state_region_waves_list_for_plot_sa10)

### SA index plots ----
# plot(fhier_mrip_sa_ind)

sa_all_cnt_indexes <- sort(unique(fhier_mrip_sa_ind$cnt_index))
q_colors_sa = length(sa_all_cnt_indexes)
mypalette = viridis(q_colors_sa, option = "D")
# mypalette <- rainbow(length(gom_all_cnt_indexes))
# names(mypalette) <- gom_all_cnt_indexes
# mypalette <- rainbow(length(fhier_mrip_sa_ind$cnt_index))
names(mypalette) <- sa_all_cnt_indexes
mypalette
# mypalette %>% unique()

sa_ind_plots <- map(unique(fhier_mrip_sa_ind$common_name),
              # run the plot_ind with this common name as a parameter and the default value for no_legend (TRUE)
               function(x) {plot_ind(use_wave(fhier_mrip_sa_ind), x, mypalette)}
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
ind_snapper_red_mrip <- gom_ind_plots[[10]]

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
             ind_snapper_red_mrip,
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
    ind_snapper_red_mrip,
    top = "Ratio",
    bottom = textGrob(
  "The Ratio is calculated as (mrip_cnts - fhier_cnts) / (mrip_cnts + fhier_cnts)",
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

