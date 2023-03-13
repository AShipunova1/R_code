# put coordinates on a shapefile
# Using https://www.r-bloggers.com/2014/07/clipping-spatial-data-in-r/
#

library(broom)
library(mapview)
library(leafsync)

library(sf)
library(leaflet)
library(leafem)

# TODO:
# rm extra libraries
# 1) create work dir
# Prerequisite: have in the work dir
#   a) csv file with GIS_LATHBEG, GIS_LATHEND, GIS_LONHBEG, GIS_LONHEND
#   b) shapefile
# 2) read shapefile
# 3) read csv and convert to sprf (or get from db)
# 4) subtract
# 5) write csv
#   a) mapview
#

# read_shapefile <- function(filename) {
#   shapefile_file_name <- file.path(my_paths$inputs, "shapefiles", filename)
#   
#   x <- readOGR(shapefile_file_name)
#   return(x)
# }
# sa_shp <- read_shapefile("osa_n_gom/SA_EEZ_off_states.shp")
# gom_shp <- read_shapefile("osa_n_gom/ReefFish_EFH_GOM.shp")
# projargs: chr "+proj=longlat +datum=NAD83 +no_defs"

stack_lat_lon_mid <- function(lat_lon_mid_data) {
  cols_lat <- c(1:2, 5)
  cols_lon <- c(3:4, 6)

  res1 <- cbind(stack(lat_lon_mid_data[cols_lat]), stack(lat_lon_mid_data[cols_lon]))
  colnames(res1) <- c("lat", "i1", "lon", "i2")

  res2 <- dplyr::select(res1, !starts_with("i"))

  # remove NAs
  res2[complete.cases(res2), ]
}

try_catch_intersection <- function(lat_lon_data, shapefile_data) {
  shapefilename <- ""
  tryCatch(
    {
      # Just to highlight: if you want to use more than one
      # R expression in the "try" part then you'll have to
      # use curly brackets.
      # 'tryCatch()' will return the last evaluated expression
      # in case the "try" part was completed successfully
      message("This is the 'try' part")
      lat_lon_data[shapefile_data, ]
      # The return value is the actual value
      # that will be returned in case there is no condition
      # (e.g. warning or error).
      # You don't need to state the return value via `return()` as code
      # in the "try" part is not wrapped inside a function (unlike that
      # for the condition handlers for warnings and error below)
    },
    error = function(cond) {
      message(paste("No trips inside the shapefile area ", shapefilename))
      message("Here's the original error message:")
      message(cond)
      # Choose a return value in case of error
      return(NA)
    },
    warning = function(cond) {
      message(paste("Coordinates caused a warning:", shapefilename))
      message("Here's the original warning message:")
      message(cond)
      # Choose a return value in case of warning
      return(NULL)
    },
    finally = {
      # NOTE:
      # Here goes everything that should be executed at the end,
      # regardless of success or error.
      # If you want more than one expression to be executed, then you
      # need to wrap them in curly brackets ({...}); otherwise you could
      # just have written 'finally=<expression>'
      message(paste("Processed coords:", shapefilename))
      # message("Some other message at the end")
    }
  )
}

# works
lat_lon_data <- clean_lat_lon_data20
lat_lon_data_to_spf <- function(lat_lon_data, shapefile_data) {
  lat_lon_crs <- "+init=epsg:4326"
  coordinates(lat_lon_data) <- ~ lon + lat
  proj4string(lat_lon_data) <- lat_lon_crs
  #     lat     lon
  # 38.40167 -73.45000
  shapefile_data <- sa_shp
  shapefile_crs <- CRS(proj4string(shapefile_data))

  lat_lon_data <- spTransform(lat_lon_data, shapefile_crs)
  #        lon     lat
  # 1 -8176417 4636324

  proj4string(lat_lon_data) <- shapefile_crs
  proj4string(shapefile_data) <- shapefile_crs

  # lat_lon_data_list <- NULL
  # lat_lon_data_short <- try_catch_intersection(lat_lon_data, shapefile_data)
  # lat_lon_data_short <- lat_lon_data[shapefile_data, ]

  # if (nrow(lat_lon_data_short@coords) > 0) {
    # nrow(lat_lon_data_short)

    # transform back
    # lat_lon_data_short_origCRS <- spTransform(lat_lon_data_short, CRS(lat_lon_crs))

    # lat_lon_data_list <- list(lat_lon_data, lat_lon_data_short_origCRS)
  # }
  return(lat_lon_data)
}

# str(lat_lon_data_list)
write_result_to_csv <- function(lat_lon_data_short_origCRS, filenames = NULL) {
  if (is.null(filenames)) filenames <- list("lat_lon_data.csv")

  out_file_name_exists <- length(filenames) == 4 &&
    (nchar(filenames[[4]]) > 0)

  if (out_file_name_exists) {
    out_file_name <- tools::file_path_sans_ext(filenames[4])
  } else if (nchar(filenames[[1]]) > 0) {
    out_file_name <- tools::file_path_sans_ext(filenames[1])
  } else {
    stop("Please provide correct file names without qoutes")
  }
  out_file_name <- paste(out_file_name, "subset.csv", collapse = "", sep = "_")

  write.csv(coordinates(lat_lon_data_short_origCRS), file = out_file_name)
}

show_dots <- function(lat_lon) {
  lat_lon %>%
    leaflet() %>%
    addTiles() %>%
    addPolylines(data = lat_lon, lng = ~longitude, lat = ~latitude, group = ~common_name) %>%
    addMarkers(~longitude, ~latitude,
               label = "common_name",
               # label = paste(lat_lon$haulnum, " ", lat_lon$latitude, " ", lat_lon$longitude),
               labelOptions = labelOptions(noHide = T),
               clusterOptions = markerClusterOptions()
    )
}

view_maps <- function(shapefile_data, lat_lon_data_list) {
  lat_lon_data <- lat_lon_data_list[[1]]
  lat_lon_data_short_origCRS <- lat_lon_data_list[[2]]

  m1 <- mapview(shapefile_data)
  m2 <- mapview(lat_lon_data, color = "red")
  m3 <- mapview(lat_lon_data_short_origCRS, color = "yellow", cex = .1)

  m_all <- m1 + m2
  m_subset <- m1 + m3
  m_all
  m_subset
  latticeView(m_all, m_subset) # not synced

  filename <- "~/R_code/m_subset.png"
  # "c:\Users\anna.shipunova\Documents\R_code\get_subset_f.R"
  # file.exists("~/R_code/get_subset_f.R")
  mapshot(m_subset, file = filename, selfcontained = FALSE)
}

write_result_to_db <- function(lat_lon_data_short_origCRS, new_table_name = NULL) {
  if (is.null(new_table_name)) new_table_name <- "lat_lon_data_result"
  new_table_name <- toupper(new_table_name)

  new_table_name <- dbQuoteIdentifier(ANSI(), new_table_name)
  res_df <- as.data.frame(lat_lon_data_short_origCRS)
  colnames(res_df) <- toupper(colnames(res_df))
  field_types <- c(
    LON = "NUMBER(8,6)",
    LAT = "NUMBER(8,6)"
  )
  dbWriteTable(con_nova, new_table_name, res_df, field.types = field_types) # , overwrite = T
}

# ---------

my_test <- function() {
  # original_test_mode <- getOption('my_package.test_mode')
  # options('my_package.test_mode' = TRUE)

  # full_path_to_new_dir <- create_work_dir()
  # filenames <- read_filenames() #   c(coord_file_name, shapefile_path, shapefile_name, out_file_name)


  # filenames <- c("export_gsc.csv", "Great_South_Channel_Restricted_Trap_Pot_Area_(20150605)", "Great_South_Channel_Restricted_Trap_Pot_Area_(20150605)", "fancy_name.csv")
  
  # shapefile: r"("\R_files_local\my_inputs\shapefiles_sa_eez_off_states\SA_EEZ_off_states.shx")"
  
  # shapefile: r"("\R_files_local\my_inputs\shapefiles_sa_eez_off_states\SA_EEZ_off_states.shx")"
  # file.path(my_paths$inputs, r"(shapefiles\sa_eaz_off_states\shapefiles_sa_eez_off_states)") %>% file.exists()
  # adm1.spdf <- rbind(Pakistan.adm1.spdf, India.adm1.spdf, makeUniqueIDs = TRUE)
  # plot(adm1.spdf)
  shapefile_dir <- r"(shapefiles\sa_eaz_off_states\shapefiles_sa_eez_off_states)"
  
  filenames <- c("", 
                 file.path(my_paths$inputs, shapefile_dir),
                 file.path(my_paths$inputs, shapefile_dir, "SA_EEZ_off_states"), 
                 "")
  browser()
  shapefile_data <- read_shapefile(filenames)
  # lat_lon_data_all <- get_csv_data(filenames)
  table_name <- "ALL_INC15_20"
  lat_lon_data_all <- get_data_from_db(table_name, " WHERE month BETWEEN 04 AND 06")
  # no mid points:
  lat_lon_data <- clean_data(lat_lon_data_all)
  # with mid points:
  # lat_lon_mid_data <- add_middle_point(lat_lon_data_all)
  # lat_lon_data <- stack_lat_lon_mid(lat_lon_mid_data)
  #

  lat_lon_data_list <- lat_lon_data_to_spf(lat_lon_data, shapefile_data)

  if (!is.null(lat_lon_data_list)) {
    # view_maps(shapefile_data, lat_lon_data_list)
    write_result_to_csv(lat_lon_data_list[2], filenames)
  } else {
    print(paste("No trips inside the shapefile area", filenames[2]))
  }

  # options("my_package.test_mode" = NULL)

  # TODO: add to the filenames code:
  # if(getOption('my_package.test_mode', FALSE)) {
  # This happens in test mode
  # my_value <- 5
  # } else {
  # normal processing
  # my_value <- readline('please write value: ')
  # }

  # out <- list(shapefile_data, lat_lon_data)
  # return(out)

  # > shapefile_data <- rres2[[1]]
  # > class(shapefile_data)
  # [1] "SpatialPolygonsDataFrame"
  # > lat_lon_data <- rres2[[2]]
}
my_test()
# __main__

subset_coords <- function(coord_file_name = NULL, shapefile_path = NULL, shapefile_name = NULL, out_file_name = NULL, input_table_name = NULL, where_part = NULL, new_out_table_name = NULL, in_from_db = FALSE, out_to_db = FALSE) {
  full_path_to_new_dir <- create_work_dir()
  inp_filenames <- list(coord_file_name, shapefile_path, shapefile_name, out_file_name)
  filenames <- read_filenames(inp_filenames)
  shapefile_data <- read_shapefile(filenames)

  if (in_from_db == TRUE) {
    if (is.null(input_table_name)) input_table_name <- readline(prompt = "Input table name: ")
    # input_table_name = "request_inc_all"

    if (is.null(where_part)) where_part <- readline(prompt = "WHERE clause (can be empty): ") # " WHERE month BETWEEN 02 AND 04"

    lat_lon_data_all <- get_data_from_db(input_table_name, where_part)
  } else { # use csv file
    lat_lon_data_all <- get_csv_data(filenames)
  }

  lat_lon_data <- clean_data(lat_lon_data_all)

  lat_lon_data_list <- lat_lon_data_to_spf(lat_lon_data, shapefile_data)

  if (is.null(lat_lon_data_list)) {
    print(paste("No trips inside the shapefile area", filenames[2]))
  } else {
    # view_maps(shapefile_data, lat_lon_data_list)
    write_result_to_csv(lat_lon_data_list[2], filenames)

    if (out_to_db) {
      if (is.null(new_out_table_name)) new_out_table_name <- readline(prompt = "Output new table name: ")

      write_result_to_db(lat_lon_data_list[2], new_out_table_name)
      print(paste("new_out_table_name: ", new_out_table_name))
    }
  }
}

## ---- com_name and counts test ----

lat_lon_data_to_spf_only <- function(lat_lon_data, shapefile_data) {
  lat_lon_crs <- "+init=epsg:4326"
  coordinates(lat_lon_data) <- ~ longitude + latitude
  proj4string(lat_lon_data) <- lat_lon_crs
  #     lat     lon
  # 38.40167 -73.45000
  # change
  # shapefile_data <- sa_shp
  shapefile_crs <- CRS(proj4string(shapefile_data))
  
  lat_lon_data <- spTransform(lat_lon_data, shapefile_crs)
  #        lon     lat
  # 1 -8176417 4636324
  
  proj4string(lat_lon_data) <- shapefile_crs
  proj4string(shapefile_data) <- shapefile_crs
  
  return(lat_lon_data)
}

# points <- tribble(~name, ~lat, ~lon,
                  # 'Point A',     -38.119151, 145.401893,
                  # 'Point B',     -38.127870, 145.685598)


## ---- convert to map obj ----
# reversed lat and lon?
points <- ungroup(lat_lon_short_grey_snap)
str(points)
points_sf <- 
  st_as_sf(points, 
           coords = c("longitude", "latitude"),
           crs = 4326)

# leaflet(points_sf) %>%
#   addTiles() %>%
#   addLabelOnlyMarkers(label = ~common_name, 
#                       labelOptions = labelOptions(noHide = T,
#                                                   direction = 'top',
#                                                   textOnly = T))

lat_lon_short_grey_snap_sf <- lat_lon_data_to_spf_only(lat_lon_short_grey_snap, sa_shp)
  
map_labels_20 <- mapview(lat_lon_short_grey_snap_sf) + m_s_g %>%
  addStaticLabels(label = lat_lon_short_grey_snap$fhier_quantity_by_sp_geo,
                  noHide = TRUE,
                  direction = 'top',
                  textOnly = TRUE,
                  textsize = "20px")

# res11 <- show_dots(lat_lon_short20)
# str(res11)
m_20_w_names <- m_s_g %>%
  addStaticLabels(label = points$common_name,
                  noHide = TRUE,
                  direction = 'top',
                  textOnly = TRUE,
                  textsize = "20px")

mll1 <- mapview(lat_lon_short_grey_snap_sf)
m_all1 <- mll1 + m_s_g %>%
  addStaticLabels(label = points$common_name,
                  noHide = TRUE,
                  direction = 'top',
                  textOnly = TRUE,
                  textsize = "20px")

m_s <- mapview(sa_shp)
m_g <- mapview(gom_shp)
m_s_g <- m_s + m_g
m_s_g
m_ll <- mapview(lat_lon_data, color = "red")
m_ll
m_f <- m_s_g + m_ll
m_f
str(lat_lon_data)

# ===
## ---- states_coords_raw to map ----
states_coords_raw_sf <- 
  lat_lon_data_to_spf_only(states_coords_raw, sa_shp)
mapview(states_coords_raw, zcol = "state_name", xcol = "latitude", ycol = "longitude")

mll1 <- mapview(states_coords_raw_sf)

data_overview(states_coords_raw)
## ---- my_data with mapview ---- 

m_my_states <- mapview(most_frequent_fhier10_w_info_state_cnts_abbr, 
        zcol = "state_name", 
        xcol = "latitude", 
        ycol = "longitude")

## add to shapefiles
# works
most_frequent_fhier10_w_info_state_cnts_abbr_geo <- 
  st_as_sf(most_frequent_fhier10_w_info_state_cnts_abbr,
           coords = c("longitude", "latitude"),
           crs = 4326)

str(most_frequent_fhier10_w_info_state_cnts_abbr)
m_my_states_geo <- mapview(most_frequent_fhier10_w_info_state_cnts_abbr_geo, 
                       zcol = "state_name")
all3 <- m_my_states_geo + m_s + m_g

## ---- split by com name ----
most_frequent_fhier10_w_info_state_cnts_abbr_list <-
  most_frequent_fhier10_w_info_state_cnts_abbr %>%
  mutate(name_cnts = paste(common_name, fhier_quantity_by_sp_n_state10)) %>%
  split(most_frequent_fhier10_w_info_state_cnts_abbr, 
        f = most_frequent_fhier10_w_info_state_cnts_abbr$common_name)

str(most_frequent_fhier10_w_info_state_cnts_abbr_list)

# zcol = paste("state_name",                                                      "fhier_quantity_by_sp_n_state10")

all3 <- m_state_geo + m_s + m_g

#                    # %>%
#   mapview(most_frequent_fhier10_w_info_state_cnts_abbr_geo, 
#           zcol = "state_name")
# all3 <- m_my_states_geo + m_s + m_g
# str(map_list)
## ==== map each ====

map_list <- lapply(most_frequent_fhier10_w_info_state_cnts_abbr_list,
                   function(x) {x_sf = st_as_sf(x,
                                                coords = c("longitude", "latitude"),
                                                crs = 4326)
                   # browser()
                   mapview(x_sf,
                           zcol = "name_cnts"
                   )
                   }
)
map_list[[1]] + m_s + m_g
all_maps <- Reduce("+", map_list) + m_s + m_g

## ---- map mrip_fhier_by_state by common name ----
names(mrip_fhier_by_state)
mrip_fhier_by_state_long <-
  mrip_fhier_by_state %>%
  rename(c("MRIP" = "mrip_estimate_catch_by_species",
           "FHIER" = "fhier_quantity_by_sp_n_state10")) %>%
  # reformat to a long format to have fhier and mrip data side by side
  pivot_longer(
    cols = c(MRIP,
             FHIER),
    names_to = "AGENCY",
    values_to = "CATCH_CNT"
  ) %>% 
  mutate(name_cnts = paste(AGENCY, CATCH_CNT)) %>%
  ungroup()

mrip_fhier_by_state_long %>% head()

mrip_fhier_by_state_list <-
  split(mrip_fhier_by_state_long,
        f = mrip_fhier_by_state_long$itis_code)

str(mrip_fhier_by_state_list[[1]])

mrip_fhier_map_list <- lapply(mrip_fhier_by_state_list,
                   function(x) {x_sf = st_as_sf(x,
                                                coords = c("longitude",
                                                           "latitude"),
                                                crs = 4326)
                   # browser()
                   mapview(x_sf,
                           zcol = "name_cnts",
                           cex = 
                   )
                   }
)

mrip_fhier_map_list[[1]] + m_s + m_g
