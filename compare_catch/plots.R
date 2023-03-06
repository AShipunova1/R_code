## ---- plot catch by species (1) ----
# Graph mrip
plot(mrip_and_fhier$SP_CODE, mrip_and_fhier$mrip_estimate_catch_by_species,
     col="blue",
     pch = 17
     , xlim = c(8800000000, max(mrip_and_fhier$SP_CODE))
     , ylim = c(0, 100000) #20000
)

# add fhier
points(mrip_and_fhier$SP_CODE,
       mrip_and_fhier$fhier_quantity_by_species,
       pch = 19,
       cex = .6,
       col="red")

## ---- add common names ----
mrip_and_fhier_w_names <- inner_join(scientific_names_w_mrip,
                                     mrip_and_fhier,
                                     by = "SP_CODE"
)

## ---- data overview ----
# MRIP numbers are much bigger
mrip_and_fhier_w_names %>%
  filter(mrip_estimate_catch_by_species > 1000) %>% dim()
# 100000: 125
# 10000 : 182
# 1000  : 221

# Only few FHIER species have counts > 100000
mrip_and_fhier_w_names %>%
  filter(fhier_quantity_by_species > 1000) %>% dim()
# 100000: 10
# 10000 : 35
# 1000  : 97

mrip_and_fhier_w_common_names <-
  mrip_and_fhier_w_names %>%
  select(fhier_quantity_by_species,
         mrip_estimate_catch_by_species,
         COMMON_NAME.x,
         SP_CODE)

data_overview(mrip_and_fhier)

# look at common names for the biggest catch
mrip_and_fhier_w_names %>%
  filter(fhier_quantity_by_species > 100000) %>%
  select(COMMON_NAME.x,
         # COMMON_NAME.y,
         fhier_quantity_by_species,
         mrip_estimate_catch_by_species
  )

## ---- plot catch by species (2) ----
mrip_and_fhier %>%
  rename("fhier_cnt" = "fhier_quantity_by_species",
         "mrip_cnt" = "mrip_estimate_catch_by_species") %>%
  select(-SP_CODE) %>%
  # filter(mrip_cnt < 10000000) %>%
  filter(mrip_cnt < 20000) %>%
  boxplot()

## ---- plot catch by species (3) ----
my_colors <- c("MRIP" = "blue", "FHIER" = "red")

MIN_SP_CODE <- 8000000000
MAX_QUANTITY_BY_SPECIES <- 200000
mrip_and_fhier_short_values <-
  # mrip_and_fhier_w_names %>%
  mrip_and_fhier %>%
  mutate(SP_CODE = ifelse(SP_CODE >= MIN_SP_CODE, SP_CODE, NA)) %>%
  mutate(fhier_quantity_by_species_short = ifelse(fhier_quantity_by_species <= MAX_QUANTITY_BY_SPECIES, fhier_quantity_by_species, NA)) %>%
  mutate(mrip_estimate_catch_by_species_short = ifelse(mrip_estimate_catch_by_species <= MAX_QUANTITY_BY_SPECIES, mrip_estimate_catch_by_species, NA))
# mutate(order = fct_reorder(as.factor(SP_CODE), COMMON_NAME.x)) %>%
str(mrip_and_fhier_short_values)

counts_plot <-
  mrip_and_fhier_short_values %>%
  ggplot(aes(x = SP_CODE,
             # order,
             y = fhier_quantity_by_species_short,
             colour = "FHIER"
  ),
  size = 2,
  alpha = 0.1
  ) +
  geom_point(aes(x = SP_CODE,
                 # order,
                 y = mrip_estimate_catch_by_species_short,
                 colour = "MRIP"
  ),
  size = 1
  ) +
  labs(title = "catch by species",
       x = "species code",
       # x = "common_name",
       y = paste0("quantity by species if < ", MAX_QUANTITY_BY_SPECIES)
  ) +
  theme(axis.text.x = element_text(angle = 45)
        # ,
        # axis.text.y = element_blank()
  ) +
  scale_colour_manual(values = my_colors) +
  geom_point()

counts_plot
# Warning messages:
#   1: Removed 305 rows containing missing values (`geom_point()`).
# 2: Removed 154 rows containing missing values (`geom_point()`).

## ---- plot catch by species by an index (4) ----
mrip_and_fhier_uni <-
  mrip_and_fhier %>%
  arrange(mrip_estimate_catch_by_species + fhier_quantity_by_species) %>%
  mutate(cnt_index = (mrip_estimate_catch_by_species - fhier_quantity_by_species) /
           (mrip_estimate_catch_by_species + fhier_quantity_by_species)
         # * 2
  )
plot(mrip_and_fhier_uni$cnt_index,
     mrip_and_fhier_uni$SP_CODE
     , ylim = c(MIN_SP_CODE, max(mrip_and_fhier_uni$SP_CODE))
)

# mutate(order = fct_reorder(as.factor(week_num), year)) %>%
# ggplot(aes(x = order,
# y = reorder(vesselofficialnumber,
# as.integer(factor(total_count)), FUN = min),

## ---- index plot with ggplot ----
counts_plot_ind <-
  mrip_and_fhier_uni %>%
  mutate(order = fct_reorder(as.factor(mrip_estimate_catch_by_species + fhier_quantity_by_species), SP_CODE)) %>%
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
  mutate(order = fct_reorder(as.factor(mrip_estimate_catch_by_species + fhier_quantity_by_species), SP_CODE)) %>%
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

counts_plot_ind_10


## ---- Grouped barchart ----
long_mrip_and_fhier_short_values_m10 <- 
  mrip_and_fhier_short_values %>% 
  # MRIP count ~ 10 times bigger
  mutate(mrip_estimate_catch_by_species_by_10 = 
           mrip_estimate_catch_by_species / 10) %>%
  rename(c("MRIP" = "mrip_estimate_catch_by_species_by_10",
           "FHIER" = "fhier_quantity_by_species")) %>%
  # reformat to a long format to have fhier and mrip data side by side
  pivot_longer(
    cols = c(MRIP,
             FHIER), 
    names_to = "AGENCY",
    values_to = "CATCH_CNT"
  ) %>%
  # use only the new columns
  select(SP_CODE, AGENCY, CATCH_CNT) %>%
  # remove lines where one or another agency doesn't have counts for this species
  drop_na() %>%
  unique()

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
ggplot(long_mrip_and_fhier_short_values_m10[start_from:max_cnt,],
       aes(fill = AGENCY,
           y = CATCH_CNT,
           x = reorder(SP_CODE,
                       as.integer(factor(CATCH_CNT)), FUN = min),
           # SP_CODE
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
n_most_frequent_fhier_15 <- get_n_most_frequent_fhier(15)
xx <- merge(mrip_and_fhier,
            n_most_frequent_fhier_15,
            by = c("SP_CODE", "fhier_quantity_by_species")) %>%
  str()

to_plot <- inner_join(mrip_and_fhier,
                      n_most_frequent_fhier_15,
                      by = c("SP_CODE", "fhier_quantity_by_species")) %>%  
  select(COMMON_NAME.x, fhier_quantity_by_species, mrip_estimate_catch_by_species)

to_plot %>% head(2)

