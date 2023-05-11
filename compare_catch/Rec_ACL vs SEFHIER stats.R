#----General Notes on how code works ----
  # (1) SEFHIER species list excel file (from Alicia Breton) 
  # (2) MRIP : "O:\Fishery Data\ACL Data\FES_Rec_data(mail_survey)\MRIP_FES_rec81_22wv6_01Mar23\MRIP_FES_rec81_22wv6_01Mar23w2014to2021LACreel.xlsx"
        # 2022 only: local mripaclspec_rec81_22wv6_01mar23w2014to2021LACreel_2022.xlsx

      ##MRIP data: field background info 
      # landing	Total Harvest (A+B1)	The total number of fish removed from the fishery resource.  May be obtained by summing catch types A (CLAIM) and B1 (HARVEST).
      # tot_cat	Total Catch (A+B1+B2)	The number of fish caught but not necessarily brought ashore.  May be obtained by summing catch types A (CLAIM), B1 (HARVEST), and B2 (RELEASE).
      
      # sp_code	Species = ITIS_CODE (SA or GOM_LABEL has common name)
      
      # sub_reg	Region	" Subregion code for region of trip
      # 4   = North Atlantic (ME; NH; MA; RI; CT) 
      # 5   = Mid-Atlantic (NY; NJ; DE; MD; VA) 
      # 6   = South Atlantic (NC; SC; GA; EFL) 
      # 7   = Gulf of Mexico (WFL; AL; MS; LA; TX) 
      #SRHS: 6=Atlantic (NC-FL Keys areas 1-17), 7=Gulf of Mexico (Dry Tortugas-TX areas 18-29)
      #TPWD and LA CREEL: 7=Gulf of Mexico
      # 8   = West Pacific (HI) 
      # 11 = U. S. Caribbean (Puerto Rico and Virgin Islands"
      
      # Use all
      # area_x	Fishing Area	" Collapsed area of fishing 
      # 1 = Ocean <= 3 mi (all but WFL) 
      # 2 = Ocean > 3 mi (all but WFL) 
      # 3 = Ocean <= 10 mi (WFL only) 
      # 4 = Ocean > 10 mi (WFL only) 
      # 5 = Inland"	CHAR
      
      # Use DS column to filter out SRHS (headboat)
      
      # Use "new mode" column to filter out private and shore modes (private = rec; 
      #            shore mode = private rec fishing from shore)
      # # new_mode = recorded mode of fishing used by SFD (1=shore, 2=headboat, 3=charterboat, 4=private boat, 5=charter/headboat, 6=priv/shore)
      
      # ab1			type A + type B1 catch estimate (number of fish killed or kept)

  # (3) SEFHIER all logbooks file (from FHIER report) 
        # need the ports from the logbook file to interjoin to the catch file 
          #- using the trip ID or something. 
          #That way you'd know where they landed the catch, for filtering by area to compare to MRIP
  
#general set up:
#load required packages; or install first if necessary 
library(tidyverse) #Collection of packages (visualization, manipulation): ggplot2, dplyr, purrr, etc.
library(readxl) # to read in XL files
library(zoo) #converting dates
library(magrittr) #for data piping (%<>% allows piping in both direction) 
library(readxl)  # reading in .xlsx
library(ROracle)

#create path to your working and output directory - where do you keep the data on your computer?
Path <- "C:/Users/michelle.masi/Documents/SEFHIER/R code/Rec ACL vs SEFHIER stats/"
Inputs <- "Inputs/"
Outputs <- "Outputs/"

#---- Read in all data -----
#data file (1) 
SEFHIER_species <- read_excel(paste(Path,Inputs,"SEFHIER_species.xlsx",sep=""), sheet = "Species Only")
  # taking a quick look
  glimpse(SEFHIER_species) #headers                                                                       
  #View(SEFHIER_species) #opens R data view tab

#data MRIP file (2)  
  mrip_raw <- read_excel(paste(Path, Inputs, "mripaclspec_rec81_22wv6_01mar23w2014to2021LACreel.xlsx", sep = ""))
  # taking a quick look
  glimpse(mrip_raw) #headers                                                                       
  #View(mrip_raw) #opens R data view tab
  
#data file (3)
  SEFHIER_logbooks <- read.csv(paste(Path, Inputs, "FHIER_all_logbook_data.csv", sep = ""))
  # taking a quick look
  glimpse(SEFHIER_logbooks) #headers                                                                       
  #View(SEFHIER_logbooks) #opens R data view tab
  
#---- Data processing ----     

#FHIER all logbook data processing ----
  
  #create function to change to lowercase
  # Use my function in case we want to change the case in all functions
  my_headers_case_function <- tolower
  
  #function to remove dots, replacing everything non-alphabetical with underscores,
     # everything that starts w/ underscore 1 or more times (^ means the beginning of the string)
     # (* = 0 or more; + = 1 or more times) - this changes the order of brackets, 
     # then changes all to lower case
  fix_names <- function(x) {
    x %>%
      # remove dots from variable x
      str_replace_all("\\.", "") %>% 
      # all not letters and numbers to underscores
      str_replace_all("[^A-z0-9]", "_") %>%   
      # letters only in the beginning
      str_replace_all("^(_*)(.+)", "\\2\\1") %>%
      # tolower
      my_headers_case_function()
  }
  
  #takes colnames() from DF provided and runs the fixed names function on them
  clean_headers <- function(my_df) {
  colnames(my_df) %<>%
    fix_names()
  return(my_df)
  }
  
  
  #some of the vessel IDs have spaces after them, and clean headers so that they are uniform
  clean_all_csvs <- function(csvs, vessel_id_field_name = NA) {
    # unify headers
    csvs_clean0 <- lapply(csvs, clean_headers)
    # trim vesselofficialnumber, just in case
    # browser()
    csvs_clean1 <- trim_all_vessel_ids_simple(csvs_clean0, vessel_id_field_name)
    return(csvs_clean1)
  }
 
  # trim "vesselofficialnumber", there are 273 w spaces in Feb 2023
  trim_all_vessel_ids_simple <-
    function(csvs_clean_ws, col_name_to_trim = NA) {
      csvs_clean <- lapply(csvs_clean_ws, function(x) {
        if (is.na(col_name_to_trim)) {
          col_name_to_trim <- grep("vessel.*official.*number",
                                   tolower(names(x)),
                                   value = T)
        }
        col_name_to_trim_s <- sym(col_name_to_trim)
        # Hard code vessel_official_number as vessel id
        x %>%
          mutate(vessel_official_number = trimws(!!col_name_to_trim_s)) %>%
          # mutate({{col_name_to_trim_s}} := trimws(!!col_name_to_trim_s)) %>%
          return()
      })
      return(csvs_clean)
    }
  
  #using the clean_all_csv function to do lines 78-85 above, and change vessel_id_field
  logbooks_content <- clean_all_csvs(SEFHIER_logbooks,
                   vessel_id_field_name = "vessel_official_nbr")
  
   
  #first create a function to modify data/time fields 
  # Change a column class to POSIXct in the "my_df" for the field "field_name" using the "date_format"
  change_to_dates <- function(my_df, field_name, date_format) {
    my_df %>%
      mutate({{field_name}} := as.POSIXct(pull(my_df[field_name]),
                                          format = date_format)) %>%
      return()
  }
  
  #Now, need to modify dates and times to appropriate R format
  SEFHIER_logbooks  %>%
    # create a new column
    mutate(trip_start_date_time =
             # trip start: combine a date without time, a space and a time
             paste(substr(trip_start_date, 1, 10),
                   trip_start_time)) %>%
    # Same for the trip end
    mutate(trip_end_date_time = paste(substr(trip_end_date, 1, 10), trip_end_time)) %>%
    # change the new column types to a date
    change_to_dates("trip_start_date_time", "%Y-%m-%d %H%M") %>%
    change_to_dates("trip_end_date_time", "%Y-%m-%d %H%M") %>%
    # change the column type to a number
    mutate(reported_quantity = as.integer(reported_quantity))
  
  #Did not modify date fields in all logbook data
  fhier_common_names <-
  fhier_logbooks_content %>%
  # names()
  select(catch_species_itis, common_name) %>%
  unique()

  
  
  
    
#MRIP data processing ----  
#filter MRIP data for just 2022 
  mrip_raw_2022 <- mrip_raw %>% filter(year == "2022")  
    #check
    dim(mrip_raw)
    # [1] 347379 67
    dim(mrip_raw_2022)
    # [1] 8332   67
    
#filter MRIP 2022 raw data for just regions 6 & 7 (Gulf and SA, respectively) ----
    mrip_raw_CBT_Gulf_and_SA <- mrip_raw_2022 %>% filter(SUB_REG %in% c(6, 7))
  #filter out SRHS data
    mrip_raw_CBT_Gulf_and_SA_noSRHS <- mrip_raw_CBT_Gulf_and_SA %>% filter(DS != "SRHS")
  #filter out private and shore modes
    mrip_raw_CBT_Gulf_and_SA_noSRHSorPrivateRec <- mrip_raw_CBT_Gulf_and_SA_noSRHS %>% filter(NEW_MODE %in% c(2, 3, 5)) 
    
    #----MRIP scientific species list ----
    MRIP_scientific_species_list <- mrip_raw_CBT_Gulf_and_SA_noSRHSorPrivateRec %>% select(NEW_SCI) %>% unique()
    
    
#----Grab just scientific names from SEFHIER_species.xlsx file ----
    SEFHIER_scientific_names <- SEFHIER_species %>% select(SCIENTIFIC_NAME)


## prepare FHIER logbook data ----
   # "Species ITIS" = column with ITIS codes in FHIER data
   # "Vessel Official" Number = column with vessel ID in FHIER data
  
  
  
#---- STOPPED HERE ----

    
    
    
    
    
    
       
    # Florida counties by region (from the Internet) ----
    fl_counties <- list(
      "SA" = c(
        "Brevard",
        "Broward",
        "Duval",
        "Flagler",
        "Indian River",
        "Martin",
        "Miami-Dade",
        "Nassau",
        "Palm Beach",
        "St. Johns",
        "St. Lucie",
        "Volusia"
      ),
      "GOM" = c(
        "Bay",
        "Charlotte",
        "Citrus",
        "Collier",
        "Dixie",
        "Escambia",
        "Franklin",
        "Gulf",
        "Hernando",
        "Hillsborough",
        "Lee",
        "Levy",
        "Manatee",
        "Monroe",
        "Okaloosa",
        "Pasco",
        "Pinellas",
        "Santa Rosa",
        "Sarasota",
        "Taylor",
        "Wakulla",
        "Walton"
      )
    )
    
    fhier_logbooks_content_waves_fl_county <-
      fhier_logbooks_content_waves %>%
      # create a new column "end_port_fl_reg" with SA, GOM or whatever else left
      mutate(
        end_port_fl_reg = case_when(
          # check in the list
          # if there is no end county, use the start
          fix_names(start_port_county) %in% fix_names(fl_counties$SA) ~ "sa",
          fix_names(start_port_county) %in% fix_names(fl_counties$GOM) ~ "gom",
          fix_names(end_port_county) %in% fix_names(fl_counties$SA) ~ "sa",
          fix_names(end_port_county) %in% fix_names(fl_counties$GOM) ~ "gom",
          # if not on the list - keep it
          .default = end_port_county
        )
      )
    
    ## test: check regions ----
    fhier_logbooks_content_waves_fl_county %>%
      # get FL only
      filter(end_port_state == "FL") %>%
      # sort by county
      arrange(end_port_county) %>%
      distinct() %>%
      # data_overview()
      # 37 counties
      # vessel_official_number          1096
      select(end_port_fl_reg) %>%
      table()
    # using only end_port_counties
    #    gom NOT-SPECIFIED            sa 
    # 201559           188         30220 
    # using a start_port_county where there is no end_port_county
    #           gom NOT-SPECIFIED            sa 
    #        201703           112         30152 
    # what else is in the new column beside sa and gom
    # filter(!(end_port_fl_reg %in% c("sa", "gom"))) %>% unique()
    
    # NOT-SPECIFIED
    
    ## states to regions ----
    # list of states in the South Atlantic region (from the Internet)
    states_sa <- data.frame(
      state_name = c(
        "Delaware",
        "District of Columbia",
        # "Florida", # exclude, we have it separated by county
        "Georgia",
        "Maryland",
        "North Carolina",
        "South Carolina",
        "Virginia",
        "West Virginia"
      )
    )
    
    sa_state_abb <-
      # a default R table
      state_tbl %>%
      # get only these in our list
      filter(state_name %in% tolower(states_sa$state_name)) %>%
      # get abbreviations
      select(state_abb)
    
    fhier_logbooks_content_waves__sa_gom <-
      fhier_logbooks_content_waves_fl_county %>%
      # add a new column "end_port_sa_gom" with sa or gom for each state
      # use fix_name aux function to unify state names (lower case, no spaces etc.)
      mutate(end_port_sa_gom = case_when(
        # if a name is in our SA list - "sa", otherwise - "gom"
        fix_names(end_port_state) %in% fix_names(sa_state_abb$state_abb) ~ "sa",
        .default = "gom"
      )) %>%
      # go through the new column again
      # if an end port state is Florida - use the region from the previous step (column "end_port_fl_reg")
      # otherwise don't change
      mutate(end_port_sa_gom = ifelse(
        tolower(end_port_state) == "fl",
        end_port_fl_reg,
        end_port_sa_gom
      )) %>%
      # remove this column, we don't need it anymore
      select(-end_port_fl_reg)
    
    #| classes: test
    ## test: states and regions ----
    fhier_logbooks_content_waves__sa_gom %>%
      # look at states and regions
      select(end_port_state, end_port_sa_gom) %>%
      unique() %>%
      glimpse()
    
    glimpse(fhier_logbooks_content_waves__sa_gom)
    
    ## combine dolphin and dolphinfish for FHIER data ----
    fhier_logbooks_content_waves__sa_gom_dolph <-
      fhier_logbooks_content_waves__sa_gom %>%
      rename(common_name_orig = common_name) %>%
      mutate(common_name = if_else(
        tolower(common_name_orig) %in% c("dolphin", "dolphinfish"),
        "DOLPHIN",
        common_name_orig
      )
      )
    
    glimpse(fhier_logbooks_content_waves__sa_gom_dolph)
    
    ### test: dolphins ----
    fhier_logbooks_content_waves__sa_gom_dolph %>%
      filter(tolower(common_name_orig) %in% c("dolphin", "dolphinfish")) %>%
      select(common_name_orig, common_name) %>% unique()
    # ---
    
    ## calculate catch ----
    
    fhier_catch_by_species_state_region_waves <-  
      fhier_logbooks_content_waves__sa_gom %>%
      # select only relevant columns
      select(
        catch_species_itis,
        end_port_state,
        end_port_sa_gom,
        end_year,
        end_wave,
        reported_quantity
      ) %>%
      # group by all of them but "reported_quantity"
      group_by(
        catch_species_itis,
        end_port_state,
        end_port_sa_gom,
        end_year,
        end_wave
      ) %>%
      # save a sum of reported_quantity in each group in fhier_quantity_by_4
      # remove NAs
      summarise(fhier_quantity_by_4 = sum(as.integer(reported_quantity), na.rm = TRUE)) %>%
      as.data.frame()
    
    #| classes: test
    ### test: cnts for 1 sp. ----
    test_species_itis <-
      fhier_logbooks_content %>%
      filter(tolower(common_name) == "mackerel, spanish") %>%
      select(catch_species_itis) %>%
      unique() %>%
      # get a string, not a df
      use_series(catch_species_itis)
    
    fhier_test_cnts <-
      fhier_catch_by_species_state_region_waves %>%
      # get the same species
      filter(catch_species_itis == test_species_itis) %>%
      # group by region
      group_by(catch_species_itis, end_port_sa_gom) %>%
      # sum the FHIER catch
      summarise(mackerel_fhier_cnt = sum(fhier_quantity_by_4, na.rm = TRUE)) %>%
      as.data.frame()
    
    # source("~/R_code_github/compare_catch/compare_catch_fhier_q.R")
    
    ## MRIP ----
    
    mrip_estimate %<>%
      mutate(ab1 = as.integer(ab1))
    
    mrip_estimate_catch_by_species_state_region_waves <-
      mrip_estimate %>%
      # select the relevan columns only
      select(itis_code, new_sta, sub_reg, year, wave, ab1) %>%
      # group by all except the counts
      group_by(itis_code, new_sta, sub_reg, year, wave) %>%
      # save the sum of "ab1" for each group in "mrip_estimate_catch_by_4"
      # remove NAs
      summarise(mrip_estimate_catch_by_4 = sum(as.integer(ab1), na.rm = TRUE)) %>%
      as.data.frame()
    
    glimpse(mrip_estimate_catch_by_species_state_region_waves)
    # 'data.frame':	878 obs. of  6 variables
    
    # "year" and "wave" to numbers
    mrip_estimate_catch_by_species_state_region_waves1 <-
      mrip_estimate_catch_by_species_state_region_waves %>%
      mutate(year = as.double(year)) %>%
      mutate(wave = as.double(wave))
    
    mrip_estimate_catch_by_species_state_region_waves <-
      mrip_estimate_catch_by_species_state_region_waves1 %>%
      # change a 6 to "sa" and a 7 "gom", leave everything else in place
      mutate(sa_gom = case_when(sub_reg == "6" ~ "sa",
                                sub_reg == "7" ~ "gom",
                                .default = sub_reg),
             # put the new column after sub_reg (by default at the end)
             .after = sub_reg) %>%
      # drop sub_reg
      select(-sub_reg)
    
    ## rename fields ----
    
    # common field names
    wave_data_names_common <- c("species_itis",
                                "state",
                                "sa_gom",
                                "year",
                                "wave"
    )
    
    # to be sure columns are in the same order
    names(mrip_estimate_catch_by_species_state_region_waves)
    
    mrip_names <- c("itis_code",
                    "new_sta",
                    "sa_gom",
                    "year",
                    "wave",
                    "mrip_estimate_catch_by_4"
    )
    
    
    mrip_estimate_catch_by_species_state_region_waves %<>%
      rename_at(vars(mrip_names[1:2]), function(x) wave_data_names_common[1:2])
    
    fhier_names <- c(
      "catch_species_itis",
      "end_port_state",
      "end_port_sa_gom",
      "end_year",
      "end_wave",
      "fhier_quantity_by_4")
    
    fhier_catch_by_species_state_region_waves %<>%
      rename_at(vars(fhier_names[1:5]),
                function(x) wave_data_names_common[1:5])
    # %>% head(100) %>% tail(2)
    
    ### rename fields in the test variables ----
    names(fhier_test_cnts) <- c("species_itis", "sa_gom", "mackerel_fhier_cnt")
    
    #was: "catch_species_itis" "end_port_sa_gom"    "mackerel_fhier_cnt"
    
    # names(mrip_test_cnts)
    
    ### test: rename fields ----
    identical(names(fhier_catch_by_species_state_region_waves)[1:5],
              names(mrip_estimate_catch_by_species_state_region_waves)[1:5])    
    