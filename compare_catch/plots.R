library(gridExtra)

names(mrip_and_fhier)
mrip_and_fhier_itis_field_name <-
  grep("itis", tolower(names(mrip_and_fhier)), value = TRUE)

## ---- plot catch by species (1) ----
str(mrip_and_fhier[mrip_and_fhier_itis_field_name])

# catch_species_itis: chr
str(mrip_and_fhier$mrip_estimate_catch_by_species)
temp_df <- data.frame(mrip_and_fhier[mrip_and_fhier_itis_field_name], mrip_and_fhier$mrip_estimate_catch_by_species)
# Graph mrip
plot(temp_df,
     col="blue",
     pch = 17
     , xlim = c(1000, 948946)
                # max(mrip_and_fhier[mrip_and_fhier_itis_field_name]))
     , ylim = c(0, 100000) #20000
)
max(mrip_and_fhier[mrip_and_fhier_itis_field_name])
# NA?
# add fhier
length(unlist(mrip_and_fhier[mrip_and_fhier_itis_field_name]))
length(mrip_and_fhier$fhier_quantity_by_species)
points(unlist(mrip_and_fhier[mrip_and_fhier_itis_field_name]),
       mrip_and_fhier$fhier_quantity_by_species,
       pch = 19,
       cex = .6,
       col="red")

## ---- add common names ----
names(mrip_and_fhier)
mrip_and_fhier_w_names <- inner_join(fhier_species_count_by_disposition_com_names,
                                     mrip_and_fhier,
                                     by = c(mrip_and_fhier_itis_field_name, "fhier_quantity_by_species")
)

## ---- data overview ----
# MRIP numbers are much bigger
mrip_and_fhier_w_names %>%
  filter(mrip_estimate_catch_by_species > 1000) %>% dim()
# 100000: 125
# 10000 : 182
# 1000  : 221
# new 47

# Only few FHIER species have counts > 100000
mrip_and_fhier_w_names %>%
  filter(fhier_quantity_by_species > 1000) %>% dim()
# 100000: 10
# 10000 : 35
# 1000  : 97
# new: 114
glimpse(mrip_and_fhier_w_names)
mrip_and_fhier_w_common_names <-
  mrip_and_fhier_w_names %>%
  select(fhier_quantity_by_species,
         mrip_estimate_catch_by_species,
         common_name,
         species_itis)

data_overview(mrip_and_fhier)

# look at common names for the biggest catch
mrip_and_fhier_w_names %>%
  filter(fhier_quantity_by_species > 100000) %>%
  select(common_name,
         # COMMON_NAME.y,
         fhier_quantity_by_species,
         mrip_estimate_catch_by_species
  )

## ---- plot catch by species (2) ----
# str(mrip_and_fhier)
mrip_and_fhier %>%
  rename("fhier_cnt" = "fhier_quantity_by_species",
         "mrip_cnt" = "mrip_estimate_catch_by_species") %>%
  select(-species_itis ) %>%
  # filter(mrip_cnt < 10000000) %>%
  filter(mrip_cnt < 20000) %>%
  boxplot()

## ---- plot catch by species (3) ----
my_colors <- c("MRIP" = "blue", "FHIER" = "red")

sort(unlist(mrip_and_fhier[mrip_and_fhier_itis_field_name])) %>% head()
# MIN_species_itis <- 100
  # min(as.integer(mrip_and_fhier[mrip_and_fhier_itis_field_name]), na.rm = TRUE)
# use fhier_quantity_by_species
MAX_QUANTITY_BY_SPECIES <- 
  max(as.integer(mrip_and_fhier$fhier_quantity_by_species), na.rm = TRUE)
# 460094
# max(as.integer(mrip_and_fhier$mrip_estimate_catch_by_species), na.rm = TRUE)
# 893605
mrip_and_fhier_short_values <-
  # mrip_and_fhier_w_names %>%
  mrip_and_fhier %>%
  # mutate(species_itis  = ifelse(species_itis  >= MIN_species_itis , species_itis , NA)) %>%
  mutate(fhier_quantity_by_species_short = ifelse(fhier_quantity_by_species <= MAX_QUANTITY_BY_SPECIES, fhier_quantity_by_species, NA)) %>%
  mutate(mrip_estimate_catch_by_species_short = ifelse(mrip_estimate_catch_by_species <= MAX_QUANTITY_BY_SPECIES, mrip_estimate_catch_by_species, NA))
# mutate(order = fct_reorder(as.factor(species_itis ), common_name)) %>%
str(mrip_and_fhier_short_values)

counts_plot <-
  mrip_and_fhier_short_values %>%
  ggplot(aes(x = species_itis,
             # order,
             y = fhier_quantity_by_species_short,
             colour = "FHIER"
  ),
  size = 1
  # ,
  # alpha = 0.1
  ) +
  geom_point(aes(x = species_itis,
                 # order,
                 y = mrip_estimate_catch_by_species_short,
                 colour = "MRIP"
  ),
  size = 2,
  alpha = 0.5
  ) +
  labs(title = "catch by species",
       x = "species code",
       # x = "common_name",
       y = paste0("quantity by species if < ", MAX_QUANTITY_BY_SPECIES)
  ) +
  theme(
    # axis.text.x = element_text(angle = 45)
        # ,
        axis.text.x = element_blank()
  ) +
  scale_colour_manual(values = my_colors) +
  geom_point()

counts_plot
# Warning messages:
# 1: Removed 393 rows containing missing values (`geom_point()`). 
# 2: Removed 7 rows containing missing values (`geom_point()`). 

## ---- plot catch by species by an index (4) ----
mrip_and_fhier_uni <-
  mrip_and_fhier %>%
  arrange(mrip_estimate_catch_by_species + fhier_quantity_by_species) %>%
  mutate(cnt_index = (mrip_estimate_catch_by_species - fhier_quantity_by_species) /
           (mrip_estimate_catch_by_species + fhier_quantity_by_species)
         # * 2
  )
plot(mrip_and_fhier_uni$cnt_index,
     unlist(mrip_and_fhier_uni[species_itis])
     # , ylim = c(MIN_species_itis , max(mrip_and_fhier_uni[species_itis] ))
)

# mutate(order = fct_reorder(as.factor(week_num), year)) %>%
# ggplot(aes(x = order,
# y = reorder(vesselofficialnumber,
# as.integer(factor(total_count)), FUN = min),

## ---- index plot with ggplot ----
counts_plot_ind <-
  mrip_and_fhier_uni %>%
  mutate(order = fct_reorder(as.factor(mrip_estimate_catch_by_species + fhier_quantity_by_species), species_itis )) %>%
  # str()
  ggplot(aes(x = order,
             y = cnt_index
  ),
  # size = 2,
  # alpha = 0.1
  ) +
  labs(title = "catch by species",
       y = "count index",
       # x = "common_name",
       x = "species"
  ) +
  theme(
    # axis.text.x = element_text(angle = 45)
    # ,
    axis.text.x = element_blank()
  ) +
  geom_point(colour = "blue")

counts_plot_ind

# (10 - 1) / (10 + 1)


## ---- index plot / 10 with ggplot ----
mrip_and_fhier_uni_10 <-
  mrip_and_fhier %>%
  mutate(mrip_estimate_catch_by_species = mrip_estimate_catch_by_species / 10) %>%
  arrange(mrip_estimate_catch_by_species + fhier_quantity_by_species) %>%
  mutate(cnt_index = (mrip_estimate_catch_by_species - fhier_quantity_by_species) /
           (mrip_estimate_catch_by_species + fhier_quantity_by_species)
         # * 2
  )
counts_plot_ind_10 <-
  mrip_and_fhier_uni_10 %>%
  mutate(order = fct_reorder(as.factor(mrip_estimate_catch_by_species + fhier_quantity_by_species), species_itis )) %>%
  # str()
  ggplot(aes(x = order,
             y = cnt_index
  ),
  # size = 2,
  # alpha = 0.1
  ) +
  labs(title = "catch by species",
       y = "count index",
       # x = "common_name",
       x = "species"
  ) +
  theme(
    # axis.text.x = element_text(angle = 45)
    # ,
    axis.text.x = element_blank()
  ) +
  geom_point(colour = "blue")

# counts_plot_ind_10

## ---- Grouped barchart ----

get_long_mrip_and_fhier_short_values_n <- function(mrip_and_fhier_short_values, n = NA) {
  if(is.na(n)) n = 10
  long_mrip_and_fhier_short_values <-
    mrip_and_fhier_short_values %>%
    # MRIP count ~ 10 times bigger
    mutate(mrip_estimate_catch_by_species_by_n =
             mrip_estimate_catch_by_species / n) %>%
    rename(c("MRIP" = "mrip_estimate_catch_by_species_by_n",
             "FHIER" = "fhier_quantity_by_species")) %>%
    # reformat to a long format to have fhier and mrip data side by side
    pivot_longer(
      cols = c(MRIP,
               FHIER),
      names_to = "AGENCY",
      values_to = "CATCH_CNT"
    ) %>%
    # use only the new columns
    select(species_itis, AGENCY, CATCH_CNT) %>%
    # remove lines where one or another agency doesn't have counts for this species
    drop_na() %>%
    unique()
}

long_mrip_and_fhier_short_values <- get_long_mrip_and_fhier_short_values_n(mrip_and_fhier_short_values, 1)

head(long_mrip_and_fhier_short_values[1:4,])

long_mrip_and_fhier_short_values_m10 <- get_long_mrip_and_fhier_short_values_n(mrip_and_fhier_short_values)

head(long_mrip_and_fhier_short_values_m10[1:4,])

long_mrip_and_fhier_short_values[18:21,]
dim(long_mrip_and_fhier_short_values_m10)
max_cnt <- length(long_mrip_and_fhier_short_values$CATCH_CNT)
max_cnt

dim(long_mrip_and_fhier_short_values_m10[as.integer(max_cnt - max_cnt/8):max_cnt,])
81
(max_cnt - max_cnt/8)
560
start_from <- as.integer(max_cnt - max_cnt/5)
# Grouped
# ggplot(long_mrip_and_fhier_short_values[18:33,],
# ggplot(long_mrip_and_fhier_short_values_m10[start_from:max_cnt,],

long_mrip_and_fhier_short_values_1 <- get_long_mrip_and_fhier_short_values_n(mrip_and_fhier_short_values)
ggplot(long_mrip_and_fhier_short_values_1,
       aes(fill = AGENCY,
           y = CATCH_CNT,
           x = reorder(species_itis ,
                       as.integer(factor(CATCH_CNT)), FUN = min),
           # species_itis 
       )
) +
  labs(title = "catch by species",
       x = "species code sorted by catch count",
       y = paste0("quantity by species if < ", MAX_QUANTITY_BY_SPECIES)
  ) +
  theme(
    axis.text.x = element_blank()
  ) +
  geom_bar(position = "dodge", stat = "identity")

str(mrip_and_fhier_short_values)

## ---- plot max (5)  ----
n_most_frequent_fhier_10 <- get_n_most_frequent_fhier(10)
# xx <- merge(mrip_and_fhier,
#             n_most_frequent_fhier_15,
#             by = c("species_itis ", "fhier_quantity_by_species")) %>%
#   str()

str(n_most_frequent_fhier_10)
# mrip_and_fhier_w_names %<>%
#   mutate(species_itis = as.character(species_itis))

names(mrip_and_fhier_w_names)
to_plot_10 <- right_join(mrip_and_fhier_w_names,
                        n_most_frequent_fhier_10,
                      by = c(mrip_and_fhier_itis_field_name, "fhier_quantity_by_species")) %>%  
  select(common_name.x, fhier_quantity_by_species, mrip_estimate_catch_by_species)

to_plot_10 %>% head(2)

## ---- separately ----
theme_sep <- theme(
  axis.text.x = element_text(
    angle = 45
    , vjust = 0.5
    ),
  plot.title = element_text(hjust = 0.5),
  axis.text = element_text(size = 10)
)

fhier_to_ten <-
  factor(to_plot_10$fhier_quantity_by_species) %>%
  as.numeric()

plot_top_10_sep <- function() {

  fhier_top_only_plot <-
    to_plot_10 %>%
    select(common_name, fhier_quantity_by_species) %>%
    ggplot(aes(x = fhier_quantity_by_species ,
               y = reorder(common_name,
                           fhier_to_ten,
                           FUN = min
               )
    )
    ) +
    labs(title = "FHIER counts"
         , x = ""
         , y = ""
    ) +
    theme_sep +
    geom_point()
  
  # fhier_top_only_plot
  
  mrip_top_only_plot <- 
    to_plot_10 %>%
    select(common_name, mrip_estimate_catch_by_species) %>%
    ggplot(aes(x = mrip_estimate_catch_by_species,
               y = reorder(common_name,
                           fhier_to_ten,
                           FUN = min
               )
    )
    ) +
    labs(title = "MRIP estimate"
         , x = ""
         , y = ""
    ) +
    theme_sep +
    geom_point()
  
  # mrip_top_only_plot
  
  max_fhier = max(to_plot_10$fhier_quantity_by_species)
  # 460094
  min_fhier = min(to_plot_10$fhier_quantity_by_species)
  # 100786
  
  # Position of vertical line/area
  mrip_top_only_plot_l <-
    mrip_top_only_plot +
    annotate("rect", 
             xmin = min_fhier, xmax = max_fhier, 
             ymin = 0, ymax = 11,
             color = "red",
             fill='red',
             alpha = .2) +
    geom_vline(aes(xintercept = max_fhier), 
               color = "red") +
    geom_text(aes(x = (max_fhier - (1.8 * min_fhier)), 
                  y = 0.5, 
                  label = "FHIER counts"
    ), 
    color = "red",
    size = 3
    )
  
  # geom_text(aes(x = (max_fhier + 1), 
  #               y = 2, 
  #               label = "Max FHIER count"
  #               ), 
  #           color = "red", 
  #           angle=90)
  
  
  super_title = "The top 10 most abundant FHIER species"
  
  grid.arrange(fhier_top_only_plot, 
               mrip_top_only_plot_l, 
               top = super_title, 
               ncol = 2)
  
}

plot_top_10_sep

## ---- 50 species ----
plot_fhier_50_most_ab_species <- function() {
  n_most_frequent_fhier_50 <- get_n_most_frequent_fhier(50)
  to_plot_50 <- right_join(mrip_and_fhier_w_names,
                           n_most_frequent_fhier_50,
                           by = c("species_itis", "fhier_quantity_by_species")) %>%  
    select(common_name, fhier_quantity_by_species, mrip_estimate_catch_by_species)
  
  fhier_to_50 <-
    factor(to_plot_50$fhier_quantity_by_species) %>%
    as.numeric()
  
  fhier_top_50_only_plot <-
    to_plot_50 %>%
    select(common_name, fhier_quantity_by_species) %>%
    ggplot(aes(x = fhier_quantity_by_species,
               y = reorder(common_name,
                           fhier_to_50,
                           FUN = min
               )
    )
    ) +
    labs(title = "FHIER counts"
         , x = ""
         , y = ""
    ) +
    theme_sep +
    theme(axis.text = element_text(size = 3)) +
    geom_point()
  
  fhier_top_50_only_plot
  
  mrip_top_50_only_plot <- 
    to_plot_50 %>%
    select(common_name, mrip_estimate_catch_by_species) %>%
    ggplot(aes(x = mrip_estimate_catch_by_species,
               y = reorder(common_name,
                           fhier_to_50,
                           FUN = min
               )
    )
    ) +
    labs(title = "MRIP estimate"
         , x = ""
         , y = ""
    ) +
    theme_sep +
    theme(axis.text = element_text(size = 3)) +
    geom_point()
  
  mrip_top_50_only_plot
  
  max_fhier = max(to_plot_50$fhier_quantity_by_species)
  # 460094
  min_fhier = min(to_plot_50$fhier_quantity_by_species)
  # 5914
  
  # Position of vertical area
  
  mrip_top_50_only_plot_a <-
    mrip_top_50_only_plot +
    annotate("rect", 
             xmin = min_fhier, xmax = max_fhier, 
             ymin = 0, ymax = 51,
             color = "red",
             fill='red',
             alpha = .2) +
    geom_text(aes(x = 0.5 * mean(max_fhier),
                  # (max_fhier - (3 * min_fhier)), 
                  y = 1.5, 
                  label = "FHIER counts"
    ), 
    color = "red",
    size = 5
    )
  
  super_title = "The top 50 most abundant FHIER species"
  
  grid.arrange(fhier_top_50_only_plot, 
               mrip_top_50_only_plot_a, 
               top = super_title, 
               ncol = 2)
}

plot_fhier_50_most_ab_species

## ---- convert to long form ----
names(to_plot_10)
# TODO: combine with get_long_mrip_and_fhier_short_values_n
get_long_form <- function(to_plot_10) {
  long_to_plot <-
    to_plot_10 %>%
    rename(c("COMMON_NAME" = "common_name.x",
             "MRIP" = "mrip_estimate_catch_by_species",
             "FHIER" = "fhier_quantity_by_species")) %>%
    # reformat to a long format to have fhier and mrip data side by side
    pivot_longer(
      cols = c(MRIP,
               FHIER),
      names_to = "AGENCY",
      values_to = "CATCH_CNT"
    ) %>%
    # use only the new columns
    select(COMMON_NAME, AGENCY, CATCH_CNT) %>%
    unique()

  return(long_to_plot)
}

## ---- order by fhier counts ----
to_plot_long <- 
  to_plot_10 %>%
    arrange(fhier_quantity_by_species) %>%
    get_long_form()

# keep the common_name order by fhier counts in the plot
to_plot_long_s <-
  to_plot_long %>%
  mutate(c_n_index = as.factor(COMMON_NAME),
         COMMON_NAME = factor(COMMON_NAME, levels = unique(c_n_index)))

# > str(to_plot_long_s$ind2)
# Factor w/ 10 levels "BASS, BLACK SEA",..: 5 5 3 3 4 4 9 9 6 6 ...
# > str(to_plot_long_s$COMMON_NAME)
# Factor w/ 10 levels "SNAPPER, GRAY",..: 1 1 2 2 3 3 4 4 5 5 ...

## ---- both together ---- 
# str(to_plot_long)
# to_plot_long$CATCH_CNT

plot_most_frequent <-
  ggplot(to_plot_long_s,
         aes(fill = AGENCY,
             x = CATCH_CNT,
             y = COMMON_NAME
         )
  ) +
  labs(title = "catch by species",
       y = "species sorted by FHIER catch count",
       x = "quantity by species"
  ) +
  geom_bar(position = "dodge", stat = "identity")

plot_most_frequent

# ggsave(filename = "plot_10_most_frequent_both.png")

write.csv(to_plot_10, file = r"(C:\Users\anna.shipunova\Documents\R_files_local\my_outputs\compare_catch\top_10_both.csv)")

# ==== fhier and mrip top plot together ====

plot_10_most_frequent_both <-
  to_plot_10 %>%
  select(common_name.x,
         fhier_quantity_by_species,
         mrip_estimate_catch_by_species) %>%
  ggplot(aes(
    x = fhier_quantity_by_species,
    y = reorder(common_name.x,
                fhier_to_ten,
                FUN = min),
    color = "FHIER"
  )) +
  labs(title = "FHIER counts"
       , x = ""
       , y = "") +
  theme_sep +
  geom_point() +
  geom_point(aes(
    x = mrip_estimate_catch_by_species,
    y = reorder(common_name.x,
                fhier_to_ten,
                FUN = min),
    color = "MRIP"
  )) +
  labs(title = "Top 10 species by FHIER compare with MRIP"
       , x = "counts"
       , y = "") +
  scale_color_manual(values = my_colors)
# +
  # theme_sep +
  # geom_point()

plot_10_most_frequent_both

## ---- top 50 both ----
## ---- 50 species ----
# plot_fhier_50_most_ab_species <- function() {
  n_most_frequent_fhier_50 <- get_n_most_frequent_fhier(50)
  to_plot_50 <- right_join(mrip_and_fhier_w_names,
                           n_most_frequent_fhier_50,
                           by = c(mrip_and_fhier_itis_field_name, "fhier_quantity_by_species")) %>%  
                           # by = c("species_itis", "fhier_quantity_by_species")) %>%
    select(common_name.x, fhier_quantity_by_species, mrip_estimate_catch_by_species)
  
  fhier_to_50 <-
    factor(to_plot_50$fhier_quantity_by_species) %>%
    as.numeric()
  
  top_50_both_plot <-
    to_plot_50 %>%
    select(common_name.x, fhier_quantity_by_species, mrip_estimate_catch_by_species) %>%
    ggplot(aes(x = fhier_quantity_by_species,
               y = reorder(common_name.x,
                           fhier_to_50,
                           FUN = min
               ),
               color = "FHIER"
    )
    ) +
    labs(title = "FHIER counts"
         , x = ""
         , y = ""
    ) +
    theme_sep +
    theme(axis.text = element_text(size = 3)) +
    geom_point() +
  # top_50_both_plot
    geom_point(aes(x = mrip_estimate_catch_by_species,
               y = reorder(common_name.x,
                           fhier_to_50,
                           FUN = min
               ),
               color = "MRIP"
    )
    ) +
    labs(title = "The top 50 most abundant FHIER species"
         , x = "counts"
         , y = "") +
    scale_color_manual(values = my_colors) +
    theme_sep +
    theme(axis.text = element_text(size = 3)) +
    geom_point()
  
  
  top_50_both_plot
  # Removed 21 rows containing missing values (`geom_point()`). 
  
  max_fhier = max(to_plot_50$fhier_quantity_by_species)
  # 460094
  min_fhier = min(to_plot_50$fhier_quantity_by_species)
  # 5914
  
# ==== mrip_estimate_catch_by_species_state_region_waves ====
  # str(mrip_estimate_catch_by_species_state_region_waves )

mrip_estimate_catch_by_species_state_region_waves %>%
    mutate(x_col = paste(new_com, sub_reg, wave, sep = "_")) %>% plot()
    # mutate(order = fct_reorder(as.factor(mrip_estimate_catch_by_4), itis_cod)
    #        ) %>%
    # str()
    ggplot(aes(x = x_col,
               y = mrip_estimate_catch_by_4
    ),
    # size = 2,
    # alpha = 0.1
    ) +
    labs(title = "MRIP only by species, sub_reg, wave",
         y = "counts",
         x = "com name, sub_reg, wave"
    ) +
    theme(
      # axis.text.x = element_text(angle = 45)
      # ,
      axis.text.x = element_blank()
    ) +
    geom_point(colour = "blue")
  
  counts_plot_ind
  
  # (10 - 1) / (10 + 1)
  