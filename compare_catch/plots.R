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

## ---- plot catch by species (4) ----
mrip_and_fhier_uni <-
  mrip_and_fhier %>%
  arrange(mrip_estimate_catch_by_species + fhier_quantity_by_species) %>% 
  mutate(cnt_index = (mrip_estimate_catch_by_species - fhier_quantity_by_species) / 
           (mrip_estimate_catch_by_species + fhier_quantity_by_species)
         # * 2
  )
plot(mrip_and_fhier_uni$cnt_index,
     mrip_and_fhier_uni$SP_CODE
     , ylim = c(8800000000, max(mrip_and_fhier_uni$SP_CODE))
)
