# get coordinate subset by a shapefile
# Using https://www.r-bloggers.com/2014/07/clipping-spatial-data-in-r/
#

library(sp)  # vector data
library(dplyr)
# library(raster)  # raster data
library(rgdal)  # input/output, projections
# library(rgeos)  # geometry ops
# library(spdep)  # spatial dependence
library(broom)
# library(ggplot2)
library(mapview)
library(leafsync)
library(rlist)

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

create_work_dir <- function() {
  # main_dir_win <- "$HOME" # "%HOMEDRIVE%%HOMEPATH%"
  main_dir <- "~"
  sub_dir <- "coord_subset"
  full_path_to_new_dir <- file.path(main_dir, sub_dir)
  dir.create(full_path_to_new_dir)
  setwd(full_path_to_new_dir)
  full_path_to_new_dir
  
}

read_filenames <- function(filenames) {
  var_names <- list("coord_file_name", "shapefile_path", "shapefile_name_full", "out_file_name")
  # 
  # readline_list0 <-
  #   list(
  #     readline(prompt = "CSV file name (with GIS_LATHBEG, GIS_LATHEND, GIS_LONHBEG, GIS_LONHEND): "),
  #     readline(prompt = "Shapefile dir name: "),
  #     readline(prompt = "Shapefile name (no extension): "),
  #     readline(prompt = "Output file name: ")
  #   )
  
  # create.fun <- function(index, pos_name) { return (function() funs[[index]](pos_name))}
  # create.fun("coord_file_name", 1) -> coord_file_name
  # create.fun("shapefile_path", 1) -> shapefile_path
  # create.fun("shapefile_name_full", 1) -> shapefile_name_full
  # 
  # create.fun("out_file_name", 1) -> out_file_name
  
  # readline_list <- list(
  #   coord_file_name <-
  #     readline(prompt = "CSV file name (with GIS_LATHBEG, GIS_LATHEND, GIS_LONHBEG, GIS_LONHEND): "),
  #   shapefile_path <-
  #     readline(prompt = "Shapefile dir name: "),
  #   shapefile_name_full <-
  #     readline(prompt = "Shapefile name (no extension): "),
  #   out_file_name <-
  #     readline(prompt = "Output file name: ")
  # )
  
  for (i in 1:length(filenames)) {
    print(i)
    empty_idx <- 0
    if (is.null(filenames[[i]])) {
      empty_idx <- i
    }
    case_when(

      empty_idx == 1 ~ coord_file_name <-
        readline(prompt = "CSV file name (with GIS_LATHBEG, GIS_LATHEND, GIS_LONHBEG, GIS_LONHEND): "),
      empty_idx == 2 ~ shapefile_path <- readline(prompt = "Shapefile dir name: "),
      empty_idx == 3 ~ shapefile_name_full <-
        readline(prompt = "Shapefile name (no extension): "),
      empty_idx == 4 ~ out_file_name <- readline(prompt = "Output file name: ")
      
      # TRUE ~ as.character(x)
    )
    
    # if (is.null(filenames[[i]])) {
    #   readline_list[[i]]
    # }
    
  # }
  
  # if (list.any(is.null(filenames))) {
  #   coord_file_name <-
  #     readline(prompt = "CSV file name (with GIS_LATHBEG, GIS_LATHEND, GIS_LONHBEG, GIS_LONHEND): ")
  #   shapefile_path <- readline(prompt = "Shapefile dir name: ")
  #   shapefile_name_full <-
  #     readline(prompt = "Shapefile name (no extension): ")
  #   out_file_name <- readline(prompt = "Output file name: ")
  # }
  
  shapefile_name <- tools::file_path_sans_ext(shapefile_name_full)
  coord_file_name <- as.character(coord_file_name)
  shapefile_path <- as.character(shapefile_path)
  shapefile_name <- as.character(shapefile_name)
  
  c(coord_file_name,
    shapefile_path,
    shapefile_name,
    out_file_name)
}

read_shapefile <- function(filenames) {
  shapefile_file_path <- filenames[2]
  shapefile_file_name <- filenames[3]

  readOGR(
    dsn = shapefile_file_path,
    layer = shapefile_file_name # Do not need ".shp" file extension
  )
}

get_csv_data <- function(filenames) {
  read.csv(filenames[1]) %>%
    dplyr::select(starts_with("GIS"))
}

clean_data <- function(lat_lon_data_all) {
  cbind(stack(lat_lon_data_all[1:2]), stack(lat_lon_data_all[3:4])) -> res1

  colnames(res1) <- c("lat", "i1", "lon", "i2")

  res2 <- dplyr::select(res1, !starts_with("i"))

  # remove NAs
  res2[complete.cases(res2),]
}

get_data_from_db <- function(table_name, where_part = "") {
  # table_name <- "REQUEST_INC_ALL"
  
  q <-  paste("select distinct GIS_LATHBEG,
    GIS_LATHEND,
    GIS_LONHBEG,
    GIS_LONHEND 
             FROM ", table_name, where_part, sep = " ")
  print(q)
  #lat_lon_data_all <- 
  dbGetQuery(con_nova, q)
}

lat_lon_data_to_spf <- function(lat_lon_data, shapefile_data) {
  lat_lon_crs <- "+init=epsg:4326"
  coordinates(lat_lon_data) <- ~ lon + lat
  proj4string(lat_lon_data) <- lat_lon_crs
  #     lat     lon
  # 38.40167 -73.45000

  shapefile_crs <- CRS(proj4string(shapefile_data))

  lat_lon_data <- spTransform(lat_lon_data, shapefile_crs)
  #        lon     lat
  # 1 -8176417 4636324

  proj4string(lat_lon_data) <- shapefile_crs
  proj4string(shapefile_data) <- shapefile_crs

  lat_lon_data_short <- lat_lon_data[shapefile_data,]

  # transform back
  lat_lon_data_short_origCRS <- spTransform(lat_lon_data_short, CRS(lat_lon_crs))

  return(list(lat_lon_data, lat_lon_data_short_origCRS))
}

write_result_to_csv <- function(lat_lon_data_short_origCRS, filenames = NULL)  {
  if(is.null(filenames)) filenames <- list("lat_lon_data.csv")
  
  out_file_name_exists <- length(filenames) == 4 &&
    (nchar(filenames[[4]]) > 0)
  
  if (out_file_name_exists) {
    out_file_name <- tools::file_path_sans_ext(filenames[4])
  }
  else if(nchar(filenames[[1]]) > 0) {
    out_file_name <- tools::file_path_sans_ext(filenames[1])
  }
  else {
    stop("Please provide correct file names without qoutes")
  }
  out_file_name = paste(out_file_name, "subset.csv", collapse = "", sep = "_")

  write.csv(coordinates(lat_lon_data_short_origCRS), file = out_file_name)
}

view_maps <- function(shapefile_data, lat_lon_data_list) {

  lat_lon_data <- lat_lon_data_list[[1]]
  lat_lon_data_short_origCRS <- lat_lon_data_list[[2]]

  m1 <- mapview(shapefile_data)
  m2 <- mapview(lat_lon_data, color = "red")
  m3 <- mapview(lat_lon_data_short_origCRS, color = "red")

  m_all <- m1 + m2
  m_subset <- m1 + m3
  m_all
  m_subset
  latticeView(m_all, m_subset) # not synced
  
  filename="~/R_code/m_subset.png"
  # "c:\Users\anna.shipunova\Documents\R_code\get_subset_f.R"
  # file.exists("~/R_code/get_subset_f.R")
  mapshot(m_subset, file = filename, selfcontained = FALSE)
  
}

write_result_to_db <- function(lat_lon_data_short_origCRS, new_table_name = NULL)  {
  if(is.null(new_table_name)) new_table_name <- "lat_lon_data_result"
  new_table_name <- toupper(new_table_name)
  

  new_table_name <- dbQuoteIdentifier(ANSI(), new_table_name)
  res_df <- as.data.frame(lat_lon_data_short_origCRS)
  colnames(res_df) <- toupper(colnames(res_df))
  field_types <- c(
    LON = "NUMBER(8,6)",
    LAT = "NUMBER(8,6)"
  )
  dbWriteTable(con_nova, new_table_name, res_df, field.types = field_types) #, overwrite = T
  
}

# ---------

my_test <- function() {
  # original_test_mode <- getOption('my_package.test_mode')
  # options('my_package.test_mode' = TRUE)

  full_path_to_new_dir <- create_work_dir()
  # filenames <- read_filenames() #   c(coord_file_name, shapefile_path, shapefile_name, out_file_name)
  
  # filenames <- c("export_mass_restr.csv", "Massachusetts_Restricted_Area_(20150605)", "Massachusetts_Restricted_Area_(20150605)")
  # filenames <- c("export_gsc.csv", "Great_South_Channel_Restricted_Trap_Pot_Area_(20150605)", "Great_South_Channel_Restricted_Trap_Pot_Area_(20150605)", "fancy_name.csv")
  filenames <- c("export_mass_restr.csv", "Massachusetts_Restricted_Area_(20150605)", "Massachusetts_Restricted_Area_(20150605)", "")
  
  shapefile_data <- read_shapefile(filenames)
  #lat_lon_data_all <- get_csv_data(filenames)
  table_name = "request_inc_all"
  lat_lon_data_all <- get_data_from_db(table_name, " WHERE month BETWEEN 04 AND 06")
  lat_lon_data <- clean_data(lat_lon_data_all)

  lat_lon_data_list <- lat_lon_data_to_spf(lat_lon_data, shapefile_data)
  
  view_maps(shapefile_data, lat_lon_data_list)
  write_result_to_csv(lat_lon_data_list[2], filenames)
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

# __main__
# filenames = c(coord_file_name, shapefile_path, shapefile_name)

subset_coords <- function(coord_file_name = NULL, shapefile_path = NULL, shapefile_name = NULL, out_file_name = NULL, input_table_name = NULL, where_part = NULL, new_out_table_name = NULL, in_from_db = FALSE, out_to_db = FALSE) {
  full_path_to_new_dir <- create_work_dir()
  inp_filenames = list(coord_file_name, shapefile_path, shapefile_name, out_file_name)
  filenames <- read_filenames(inp_filenames)               
  shapefile_data <- read_shapefile(filenames)
  
  if (in_from_db == TRUE) {
    
      if(is.null(input_table_name)) input_table_name <- readline(prompt = "Input table name: " )
      # input_table_name = "request_inc_all"
      
      if(is.null(where_part)) where_part <- readline(prompt = "WHERE clause (can be empty): " ) # " WHERE month BETWEEN 02 AND 04"
      
      lat_lon_data_all <- get_data_from_db(input_table_name, where_part)  
  }
  
  else { # use csv file
    lat_lon_data_all <- get_csv_data(filenames)
  }
  
  lat_lon_data <- clean_data(lat_lon_data_all)
  
  lat_lon_data_list <- lat_lon_data_to_spf(lat_lon_data, shapefile_data)
  
  view_maps(shapefile_data, lat_lon_data_list)
  write_result_to_csv(lat_lon_data_list[2], filenames)
  
  if (out_to_db) {
    if(is.null(new_out_table_name)) new_out_table_name <- readline(prompt = "Output new table name: " )
  
    write_result_to_db(lat_lon_data_list[2], new_out_table_name)
    print(paste("new_out_table_name: ", new_out_table_name))
  }
}

