# Compare catch in survey vs logbook
# see read.me

## ---- set up ----
library(zoo)

source("~/R_code_github/useful_functions_module.r")
my_paths <- set_work_dir()

source("~/R_code_github/compare_catch/get_data.R")

# ---- the breath of species caught in SEFIHIER (2022) ----
# ?? (where is the permit info) Do this by region (gulf vs s atl vessels). Or by landing?
# ---- Then the total caught (numbers) for each unique species. ----
# ?? species ids are different in MRIP in SEFHIER, need a scientific name to connect

## ---- ID the breath of species caught in all SEFHIER data. Do this by region (gulf vs s atl vessels) ----

# names(fhier_species_count_by_disposition)
# str(fhier_species_count_by_disposition)
# 'data.frame':	316171  obs. of  6 variables:
# fhier_species_count_by_disposition %>%
  # select(disposition) %>% unique()
## ---- logbooks_content ----
itis_field_name <- grep("itis", names(logbooks_content), value = T)

# catch_species_itis
vessel_id_field_name <- grep("vessel.*official", names(logbooks_content), value = T)
# vessel_official_nbr
# logbooks_content$end_port_state %>% unique()
# [1] "FL" "NC" "SC" "AL" "MS" "TX" "GA" "VA" "MD" "LA" "DE" "RI" "NJ" "MA" "NY"
# [16] "CT" "ME"

# get landing by coordinates
logbooks_content_short_2022 <-
  logbooks_content %>%
  select(area_code,
         catch_species_itis,
         common_name,
         disposition_code,
         disposition_name,
         end_port_state,
         end_port_name,
         latitude,
         longitude,
         notif_end_port,
         notif_end_port_state,
         reported_quantity,
         sub_area_code,
         trip_end_date,
         trip_end_time,
         trip_start_date,
         trip_start_time,
         trip_type,
         trip_type_name
  ) %>% 
  change_to_dates("trip_start_date", "%Y-%m-%d %H:%M:%S") %>% 
  filter(format(trip_start_date, format = "%Y") == "2022") %>%
  change_to_dates("trip_end_date", "%Y-%m-%d %H:%M:%S") %>%
  mutate(reported_quantity = as.integer(reported_quantity))

# rm(logbooks_content)
# gc()

# ?
logbooks_content_short_2022 %>%
  filter((disposition_name %in% c("RELEASED ALIVE", "DEAD DISCARD", "NO CATCH", "UNDER SIZE LIMIT"))) %>% dim()
# ! 194188     
# released 125888
# unique()


## ---- FHIER: count catch by species ----
# TODO: separate functions
from_count_by_disposition <- function() {
  fhier_quantity_by_species <-
    fhier_species_count_by_disposition %>%
    select(species_itis, reported_quantity) %>%
    group_by(species_itis) %>%
    summarise(fhier_quantity_by_species = sum(as.integer(reported_quantity)))


# head(fhier_quantity_by_species, 10)

## ---- add common names ----

# change both columns to numeric
fhier_quantity_by_species <-
  mutate(fhier_quantity_by_species, species_itis = as.numeric(species_itis))
scientific_names <-
  mutate(scientific_names, species_itis = as.numeric(species_itis))

names(scientific_names)
# common_name
names(fhier_quantity_by_species)
fhier_species_count_by_disposition_com_names <-
    inner_join(fhier_quantity_by_species,
          scientific_names, 
          by = c("catch_species_itis" = "species_itis")
          )

str(fhier_species_count_by_disposition_com_names)
  
# red snapper, greater amberjack, gag, and gray triggerfish
fhier_species_count_by_disposition_com_names %>%
  filter(grepl("snapper.*red", tolower(common_name)))

fhier_species_count_by_disposition_com_names %>%
  filter(grepl("amberjack.*greater", tolower(common_name)))

fhier_species_count_by_disposition_com_names %>%
  filter(grepl("gag", tolower(common_name)))

fhier_species_count_by_disposition_com_names %>%
  filter(grepl("triggerfish.*gray", tolower(common_name)))

names(fhier_species_count_by_disposition)

## ---- FHIER: count catch by species and permit ----
fhier_quantity_by_species_and_permit <-
  fhier_species_count_by_disposition %>%
  select(permit_region, species_itis, reported_quantity) %>% 
  group_by(species_itis, permit_region) %>% 
  summarise(fhier_quantity_by_species_and_permit = sum(as.integer(reported_quantity)))
# head(fhier_quantity_by_species_and_permit, 10)
} # end of work with count by disposition file

## ---- FHIER: count catch by species and permit ----
str(logbooks_content)

## - get coords ----
# end_port_name
# end_port
grep("sp", names(logbooks_content), value = T)
grep("com", names(logbooks_content), value = T)
grep("reg", names(logbooks_content), value = T)
# end_port_county?
fhier_logbooks_content <-
logbooks_content  %>%
  change_to_dates("trip_start_date", "%Y-%m-%d")

## ---- wave ----
# I have dfd$mon<-as.yearmon(dfd$Date) then
fhier_logbooks_content %>%
  mutate(start_month = as.yearmon(trip_start_date)) %>%
  mutate(start_month_num = month(trip_start_date)) %>% 
  # s = (df['month'] - 1) // 2 + 1
  mutate(start_wave  = (start_month_num - 1) / (2 + 1)
         ) %>%
  # select(trip_start_date, start_month, start_month_num, start_wave) %>%
  select(start_month, start_month_num, start_wave) %>%
  unique() %>%
  arrange(start_month_num) %>%
  tail()
# r<-as.data.frame(dfd %>%
                   # mutate(month = format(Date, "%m"), year = format(Date, "%Y")) %>%
                   # group_by(Group,mon) %>%
                   # summarise(total = mean(Score), total1 = mean(Score2))) 

# mutate(yearhalf = as.integer(6/7)+1) %>%
  
fhier_quantity_by_species_permit_state_region_waves <-
  logbooks_content %>%
  select(catch_species_itis, common_name, end_port_state, wave, ab1) %>% 
  
  select(permit_region, species_itis, reported_quantity) %>% 
  group_by(species_itis, permit_region) %>% 
  summarise(fhier_quantity_by_species_and_permit = sum(as.integer(reported_quantity)))

mrip_estimate_catch_by_species_state_region_waves <-
  mrip_estimate %>%
  select(itis_code, new_com, new_sta, sub_reg, wave, ab1) %>% 
  group_by(itis_code, new_com, new_sta, sub_reg, wave) %>% 
  summarise(mrip_estimate_catch_by_4 = sum(as.integer(ab1))) %>%
  as.data.frame()


## ---- MRIP data ----

## ---- convert ab1 to integers ----
# names(mrip_estimate)
mrip_estimate %<>%
  mutate(ab1 = as.integer(ab1))

## ---- MRIP: count catch by species and region ----
# str(mrip_estimate)
mrip_estimate_catch_by_species_and_region <-
  mrip_estimate %>%
    select(itis_code, sub_reg, ab1) %>%
    group_by(itis_code, sub_reg) %>% 
    summarise(mrip_estimate_catch_by_species_and_region = sum(ab1))
# head(mrip_estimate_catch_by_species_and_region, 20)

## ---- MRIP: count catch by species only ----
mrip_estimate_catch_by_species <-
  mrip_estimate %>%
  select(itis_code, ab1) %>% 
  group_by(itis_code) %>% 
  summarise(mrip_estimate_catch_by_species = sum(ab1))
# head(mrip_estimate_catch_by_species, 2)

grep("lon", names(mrip_estimate), value = T)
## ---- MRIP: count catch by species and state ----
mrip_estimate_catch_by_species_and_state <-
  mrip_estimate %>%
  select(itis_code, new_sta, new_com, ab1) %>% 
  group_by(itis_code, new_com, new_sta) %>% 
  summarise(mrip_estimate_catch_by_species = sum(as.integer(ab1)))

# str(mrip_estimate)
## ---- compare fhier with mrip ----
# mrip_estimate_catch
# head(fhier_species_count_by_disposition, 3)
# head(fhier_quantity_by_species, 3)
head(mrip_estimate_catch_by_species, 3)

## ---- column names ----
sp_itis_fhier <-
  grep("itis", tolower(names(fhier_species_count_by_disposition)), value = TRUE)
sp_itis_mrip <-
  grep("itis", tolower(names(mrip_estimate)), value = TRUE)

## ---- compare species in fhier with mrip ----
compare_species_in_fhier_with_mrip <- function(fhier_species_count) {
species_used_in_fhier <-
  fhier_species_count %>%
  select(all_of(sp_itis_fhier)) %>% 
  unique() %>%
  set_names(sp_itis_mrip <- "itis")

str(species_used_in_fhier)
# 458

species_in_mrip <-
  mrip_estimate %>%
  # select(sp_code) %>% unique()
  select(all_of(sp_itis_mrip)) %>% 
  unique() %>%
  set_names(sp_itis_mrip <- "itis")
str(species_in_mrip)
# 76 itis_code

# in FHIER with catch info only
setdiff(species_used_in_fhier, species_in_mrip) %>% str()
# 145
# 386
# in MRIP only
setdiff(species_in_mrip, species_used_in_fhier) %>% str()
# 119
# 4

# in both
intersect(species_used_in_fhier, species_in_mrip) %>% str()
# 229
# 72
}

## ---- if use by region/landing ----
# mrip_estimate_catch_1 <-
#   mrip_estimate_catch %>%
#     mutate(permit_region = 
#            case_when(sub_reg == "6" ~ "SA",
#                      sub_reg == "7" ~ "GOM"
#                     )
#            ) %>%
#   select(-sub_reg)

# str(mrip_estimate_catch_1)

## ---- most n frequent FHIER species ----

# str(fhier_quantity_by_species)

get_n_most_frequent_fhier <- function(n, df_name = NA) {
  if (not(is.data.frame(df_name))) {df_name <- fhier_quantity_by_species}
  df_name %>%
    arrange(desc(fhier_quantity_by_species)) %>%
    head(n) %>%
    return()
}

# source("~/R_code_github/compare_catch/plots.R")


## ---- add counts ----
# all
fhier_quantity_by_species <-
  logbooks_content_short_2022 %>%
  select(catch_species_itis, common_name, reported_quantity) %>% 
  mutate(reported_quantity = as.integer(reported_quantity)) %>%
  group_by(catch_species_itis, common_name) %>% 
  summarise(fhier_quantity_by_species = sum(reported_quantity)) %>%
  mutate(fhier_quantity_by_species = as.integer(fhier_quantity_by_species)) %>%
  arrange(desc(fhier_quantity_by_species))


head(fhier_quantity_by_species)

# by_species_and_state
fhier_quantity_by_species_and_state <-
  logbooks_content_short_2022 %>%
  select(catch_species_itis, common_name, end_port_state, reported_quantity) %>% 
  group_by(catch_species_itis, common_name, end_port_state) %>% 
  summarise(fhier_quantity_by_species_and_state = sum(as.integer(reported_quantity)))

head(fhier_quantity_by_species_and_state)

# by species and port
fhier_quantity_by_species_and_port <-
  logbooks_content_short_2022 %>%
  select(catch_species_itis, common_name, end_port_name, end_port_state,  reported_quantity) %>% 
  group_by(catch_species_itis, common_name, end_port_name, end_port_state) %>% 
  summarise(fhier_quantity_by_species_and_state = sum(as.integer(reported_quantity)))

tail(fhier_quantity_by_species_and_port)
# [1] 12057     5


## ---- combine mrip and fhier catch results by species
combine_mrip_and_fhier_catch_results_by_species <- function() {
  mrip_and_fhier <-
    full_join(
      fhier_quantity_by_species,
      mrip_estimate_catch_by_species,
      by = c("catch_species_itis" = "itis_code")
    )
  
  # head(mrip_and_fhier, 3)
  
  # fhier quantity is grater than mrip's
  mrip_and_fhier %>%
    filter(mrip_estimate_catch_by_species <= fhier_quantity_by_species) %>% str()
  # 15
  
  # combine mrip with fhier with common names
  # fhier_species_count_by_disposition_com_names
  
  return(mrip_and_fhier)
}
mrip_and_fhier <- combine_mrip_and_fhier_catch_results_by_species()

## --- shapefiles ----

# get 10 most ab
# get coordinates
# add ab
# add to map

most_frequent_fhier10 <- get_n_most_frequent_fhier(10, fhier_quantity_by_species)
glimpse(most_frequent_fhier10)

get_info_for_most_frq <- function() {
  logbooks_content_short_2022 %>%
    inner_join(most_frequent_fhier10, by = c("catch_species_itis", "common_name")) %>%
    select(catch_species_itis, common_name, end_port_name, end_port_state,  reported_quantity, latitude, longitude) %>%
    return()
}

most_frequent_fhier10_w_info <- get_info_for_most_frq()
str(most_frequent_fhier10_w_info)
grep("\\.y", names(most_frequent_fhier10_w_info), value = T)

# lat_lng <- function(la_lng_points) {
#   latitude <- 0
#   longitude <- 0
#   n <- points.length
#   
#   for (point in la_lng_points) {
#     latitude = latitude + point.latitude
#     longitude = longitude + point.longitude
#   }
#   
#   (latitude / n, longitude / n)
#   return 
# }

# ---- example from https://gis.stackexchange.com/questions/64392/finding-clusters-of-points-based-distance-rule-using-r ----
# ?require
require(sp)
require(rgdal)
library(leaflet)
library(gtools)


# Create example data and transform into projected coordinate system
# x <- c(-1.482156, -1.482318, -1.482129, -1.482880, -1.485735, -1.485770, -1.485913, -1.484275, -1.485866)
# y <- c(54.90083, 54.90078, 54.90077, 54.90011, 54.89936, 54.89935, 54.89935, 54.89879, 54.89902)

# ---- my_lat lon ----
## ---- make coord_table ----
# GIS_LATHBEG, GIS_LATHEND, GIS_LONHBEG, GIS_LONHEND

# str(lat_lon_cnts)
# str(most_frequent_fhier10_w_info)

lat_lon_cnts_w_info <-
  most_frequent_fhier10_w_info %>% 
  mutate(latitude = as.double(latitude) %>% 
           round(digits = 2)) %>% 
  mutate(longitude = as.double(longitude) %>% 
           round(digits = 2)) %>% 
  # filter(abs(latitude) >= 0 & abs(longitude) >= 0) %>%
  # to all positive
  mutate(latitude = abs(latitude)) %>%
  # to all negative
  mutate(longitude = (abs(longitude) * -1)) %>%
  # data_overview()
  group_by(common_name, latitude, longitude) %>%
  summarise(fhier_quantity_by_sp_geo = sum(as.integer(reported_quantity)))

# latitude     
# Min.   :-87.30  
# Max.   : 90.00    

#     longitude      
# Min.   :-105.14  
# Max.   : 137.59  

lat_lon_cnts_w_info %>% data_overview()
# latitude
# Min.   : 0.16
# Max.   :90.00 
#     longitude      
# Min.   :-137.59
# Max.   :  -0.32

lat_lon_cnts_w_info %>%
  filter(abs(longitude) < 80) %>%
  select(common_name, latitude, longitude) %>% unique %>% dim()
# 6693

lat_lon_cnts_w_info %>%
  filter(abs(longitude) > abs(latitude)) %>%
  select(common_name, latitude, longitude) %>% unique %>% dim()
# 58192     
dim(lat_lon_cnts_w_info)
# 58880     

lat_lon_cnts_w_info %>%
  filter(abs(latitude) < 10) %>% unique %>% dim()
# 623

dim(lat_lon_cnts_w_info)
# 58871

clean_geo_data <- function(lat_lon_cnts_w_info) {
  # cbind(stack(lat_lon_data_all[1:2]), stack(lat_lon_data_all[3:4])) -> res1
  # browser()
  res2 <- 
    lat_lon_cnts_w_info %>%
    ungroup() %>%
    select(latitude, longitude)

  # colnames(res2) <- c("lat", "lon")
  # remove NAs
  clean_lat_lon <- res2[complete.cases(res2), ]
  return(clean_lat_lon)
}

# lat_lon_data <- clean_geo_data(lat_lon_cnts_w_info)
# str(lat_lon_data)
lat_lon_short20 <-
  lat_lon_cnts_w_info %>%
  ungroup %>% 
  select(common_name, latitude, longitude) %>% unique() %>% tail(20)

str(lat_lon_cnts_w_info)
# gropd_df [58,871 × 4] (S3: grouped_df/tbl_df/tbl/data.frame)

# lat_lon_cnts_w_info %>%
#   ungroup() %>%
#   select(common_name) %>% unique() %>% head()

lat_lon_short_grey_snap <-
  lat_lon_cnts_w_info %>%
  ungroup %>% 
  filter(abs(longitude) > abs(latitude)) %>%
  filter(common_name == "SNAPPER, GRAY") %>%
  select(fhier_quantity_by_sp_geo, latitude, longitude) %>% unique() %>%
  # dim()
# 6525
tail(20)


# lat_lon_short_20000 <-
#   lat_lon_cnts_w_info %>%
#   ungroup %>% 
#   select(common_name, latitude, longitude) %>% unique() %>% tail(20000)

# str(lat_lon_short_20000)

clean_lat_lon_data20 <- clean_geo_data(lat_lon_short20)

names(most_frequent_fhier10_w_info)
lat_lon_cnts <-
  most_frequent_fhier10_w_info %>% 
  mutate(latitude = as.double(latitude) %>% round(digits = 2)) %>% 
  mutate(longitude = as.double(longitude) %>% round(digits = 2)) %>%
  filter(abs(latitude) >= 0 & abs(longitude) >= 0) %>%
  # select(latitude, longitude) %>% 
# [1] 30831     2
  # unique()
# %>%
  # dim()
# [1] 3609    2
  group_by(common_name, latitude, longitude) %>%
  summarise(fhier_quantity_by_sp_geo = sum(as.integer(reported_quantity)))
# dim(lat_lon_cnts)
# 59929     

plot_xy <- function() {
  x <- lat_lon_cnts$latitude
  # x <- as.double(drop_na(as.numeric(most_frequent_fhier10_w_info$latitude)))
  str(x)
  y <- lat_lon_cnts$longitude
  xy <- SpatialPointsDataFrame(
    matrix(c(x, y), ncol = 2),
    data.frame(ID = seq(1:length(x))),
    proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
  )
  
  xy <- spTransform(xy, CRS("+init=epsg:4326 +datum=WGS84"))
  
  # round(x, digits = 0)
  # here
  chc <- hclust(dist(
    data.frame(
      rownames = rownames(xy@data),
      x = coordinates(xy)[, 1],
      y = coordinates(xy)[, 2]
    )
  ),
  method = "complete")
  
  str(chc)
  dist_tr <- 40  # Distance threshold
  # Distance with a 40m threshold
  chc.d40 <- cutree(chc, h = d)
  # chc.d40 <- cutree(chc, k = 500)
  
  # Join results to meuse sp points
  xy@data <- data.frame(xy@data, Clust = chc.d40)
  
  # Plot results
  plot(xy, col = factor(xy@data$Clust), pch = 19)
  box(col = "black")
  title(main = "Clustering")
  legend(
    "topleft",
    legend = paste("Cluster", 1:4, sep = ""),
    col = palette()[1:4],
    pch = rep(19, 4),
    bg = "white"
  )
}

## --- round up coords ----
# install.packages("geosphere")
# install.packages(geosphere,distMeeus,distm)
# library(geosphere)
# # round(x, digits = 0)
# distance <- lat_lon_cnts %>%
#   select(latitude, longitude) %>%
#   distm()
# Error in .pointsToMatrix(x) : latitude < -90
# In addition: Warning message:
#   In .pointsToMatrix(x) : NAs introduced by coercion

## ---- convert coords ----
# Decimal Degrees = degrees + (minutes/60) + (seconds/3600)

## ---- counts by state for the top 10 ----
# fhier_quantity_by_species_and_state <-
#   logbooks_content_short_2022 %>%
# str(most_frequent_fhier10_w_info)  

most_frequent_fhier10_w_info_state_cnts <-
most_frequent_fhier10_w_info %>%
  select(catch_species_itis, common_name, end_port_state, reported_quantity) %>%
  group_by(catch_species_itis, common_name, end_port_state) %>%
  summarise(fhier_quantity_by_sp_n_state10 = sum(as.integer(reported_quantity)))
# %>% str()
# gropd_df [86 × 3] (S3: grouped_df/tbl_df/tbl/data.frame)

## ---- add state coords ----
most_frequent_fhier10_w_info_state_cnts_abbr <-
  states_coords_raw %>%
  mutate(state_name = tolower(state_name)) %>%
  inner_join(state_tbl, 
             by = "state_name") %>%
  inner_join(most_frequent_fhier10_w_info_state_cnts, 
             by = c("state_abb" = "end_port_state"),
             multiple = "all")

# head(most_frequent_fhier10_w_info_state_cnts_abbr)
# most_frequent_fhier10_w_info_state_cnts_abbr %>%
  # select(state_name) %>% unique()
# 15
# names(most_frequent_fhier10_w_info_state_cnts_abbr)
## ---- same for MRIP ----
names(mrip_estimate_catch_by_species_and_state)

mrip_fhier_by_state <-
  mrip_estimate_catch_by_species_and_state %>%
  inner_join(most_frequent_fhier10_w_info_state_cnts_abbr, 
             by = c("new_sta" = "state_abb",
                    "itis_code" = "catch_species_itis"),
             multiple = "all")

## ---- get real coordinates ----
names(most_frequent_fhier10_w_info)

most_frequent_fhier10_w_info_lat_lon <-
  most_frequent_fhier10_w_info %>% 
  mutate(latitude = as.numeric(latitude)) %>%
  # mutate(lat1 = trunc(latitude) + ((latitude - trunc(latitude)) / 60) ) %>% 
  mutate(longitude = as.numeric(longitude)) %>%
  filter(abs(latitude) >= 60 & abs(longitude) >= 20) %>%
  mutate(latitude = abs(latitude)) %>%
  # to all negative
  mutate(longitude = (abs(longitude) * -1))

# tibble [147 × 5] (S3: tbl_df/tbl/data.frame)
# data_overview(most_frequent_fhier10_w_info_lat_lon)

lat_lon_cnts <-
  most_frequent_fhier10_w_info_lat_lon %>%
  group_by(catch_species_itis, common_name, latitude, longitude) %>%
  summarise(fhier_quantity_by_sp_geo = sum(as.integer(reported_quantity))) %>%
  ungroup()

data_overview(lat_lon_cnts)
# before cleaning:
# reported_quantity    latitude        longitude      
# Min.   :   0.00   Min.   :-87.30   Min.   :-105.14  
# 1st Qu.:   3.00   1st Qu.: 26.96   1st Qu.: -85.98  
# Median :   8.00   Median : 29.00   Median : -82.47  
# Mean   :  12.89   Mean   : 28.84   Mean   : -50.69  
# 3rd Qu.:  15.00   3rd Qu.: 30.12   3rd Qu.: -78.38  
# Max.   :1000.00   Max.   : 90.00   Max.   : 137.59  

