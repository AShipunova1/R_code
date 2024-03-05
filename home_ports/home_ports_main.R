# home_ports
# today()
# [1] "2024-02-28"

# setup ----
source("~/R_code_github/useful_functions_module.r")
my_paths <- set_work_dir()

current_project_dir_path <- this.path::this.dir()

current_project_basename <- basename(current_project_dir_path)

current_output_dir <-
  file.path(my_paths$outputs,
            current_project_basename)

# dir.exists(current_output_dir)

# Variables for the current year(s)
my_year1 <- "2022"
my_beginning1 <- str_glue("{my_year1}-01-01")
my_end1 <- str_glue("{my_year1}-12-31")

my_year2 <- "2023"
my_beginning2 <- str_glue("{my_year2}-01-01")
my_end2 <- str_glue("{my_year2}-12-31")

my_year3 <- "2024"
my_beginning3 <- str_glue("{my_year3}-01-01")
my_end3 <- str_glue("{my_year3}-12-31")

# prepare data ----
get_data_file_path <-
  file.path(my_paths$git_r,
            current_project_basename,
            "home_ports_get_data.R")

# file.exists(get_data_file_path)

source(get_data_file_path)
# res:
# vessels_from_pims_ok

# run once to get lat lon and check names with no coords
# 1) add lat/lon
# 2) check names without coordinates
# 3) fix names

# separate hailing_port into city and state ----

# Explanations:
# The variable 'vessels_from_pims_split_addr' is created by:
# 1. Separating the 'hailing_port' column into two columns ('city' and 'state') using a comma as the delimiter with 'tidyr::separate_wider_delim'.
# 2. Dropping any additional columns created during the separation.
# 3. Trimming leading and trailing whitespaces from all character columns using 'mutate(across(where(is.character), str_trim))'.
vessels_from_pims_split_addr <-
  vessels_from_pims_ok |>
  tidyr::separate_wider_delim(hailing_port,
                              delim = ",",
                              names = c("city", "state"),
                              too_many = "merge") |> 
    mutate(across(where(is.character), str_squish))

vessels_from_pims_ok |>
  filter(grepl("\\d", hailing_port))
   # vessel_official_number hailing_port            
#    <chr>                  <chr>                   
#  1 574500                 HO0MASASSA, FL          
#  2 1040384                2, AL                   
#  3 NC6421AU               FIGURE 8 ISLAND, NC     
#  4 FL3407ML               0,                      
#  5 FL8939JR               00,                     
#  6 925240                 0,                      
#  7 FL5011MX               NAPLE4S, FL             
#  8 SC8023DE               LITTLE RIVERNHV1N4WH, SC
#  9 139403                 0,                      
# 10 DO552832               0,                      
# 11 1301930                22411 GENO LANE, AL     
# 12 GA1769JL               117 HAWK LANDING LN, GA 


# vessels_from_pims_ok |>
#   filter(grepl(",.+,", hailing_port))
# 1 945114                 REDINGTON SHORES, FL, FL
# 2 919225                 CHAUVIN, LA, LA         
# 3 AL6468LL               ALEXANDER CITY, AL, AL  
# 4 FL0702JJ               MATLACHA, BOKKELIA, FL
# 5 8811432134             PEMBROKE, PINES, FL     
   
# fix known home port typos ----

# this list is created manually
to_fix_list <- 
  list(c("117 HAWK LANDING LN#GA",
         "BRUNSWICK#GA"),
       c("22411 GENO LANE#AL",
         "GULF SHORES#AL"),
       c("ALEXANDER CITY, AL#AL",
         "ALEXANDER CITY#AL"),
       c("BAYOU LABATRE#AL",
         "BAYOU LA BATRE#AL"),
       c("CAROLINA BEACH#UN",
         "CAROLINA BEACH#NC"),
       c("CHALESTON#SC",
         "CHARLESTON#SC"),
       c("CHAUVIN, LA#LA",
         "CHAUVIN#LA"),
       c("CHAUVIN#LA, LA",
         "CHAUVIN#LA"),
       c("FERNADINA BCH#FL",
         "FERNANDINA BEACH#FL"),
       c("FIGURE 8 ISLAND#NC",
         "FIGURE EIGHT ISLAND#NC"),
       c("FORT MORGAN MARINA#AL",
         "FORT MORGAN#AL"),
       c("GALLINANO#LA",
         "GALLIANO#LA"),
       c("GEORGRTOWN#SC",
         "GEORGETOWN#SC"),
       c("GULFSHORES#AL",
         "GULF SHORES#AL"),
       c("HILISBORO INLET#FL",
         "HILLSBORO INLET#FL"),
       c("HO0MASASSA#FL",
         "HOMOSASSA#FL"),
       c("HOMOASSA#FL",
         "HOMOSASSA#FL"),
       c("HOUMA LA#LA",
         "HOUMA#LA"),
       c("INTERCOASTAL CITY#LA",
         "INTRACOASTAL CITY#LA"),
       c("ISLAMORADA#UN",
         "ISLAMORADA#FL"),
       c("KEYWEST#FL",
         "KEY WEST#FL"),
       c("LITTLE RIVERNHV1N4WH#SC",
         "LITTLE RIVER#SC"),
       c("LOXLEY AL#AL",
         "LOXLEY#AL"),
       c("MADIERA BEACH#FL",
         "MADEIRA BEACH#FL"),
       c("MATLACHA#BOKKELIA, FL",
         "MATLACHA#FL"),
       c("MAYPPORT#FL",
         "MAYPORT#FL"),
       c("MCLELLANVILLE#SC",
         "MCCLELLANVILLE#SC"),
       c("MURELLS INLET#SC",
         "MURRELLS INLET#SC"),
       c("MURRELS INLET#SC",
         "MURRELLS INLET#SC"),
       c("NAPLE4S#FL",
         "NAPLES#FL"),
       c("NEW SMYMA BEACH#FL",
         "NEW SMYRNA BEACH#FL"),
       c("NEW SYMRNA BEACH#FL",
         "NEW SMYRNA BEACH#FL"),
       c("OCEEAN CITY#MD",
         "OCEAN CITY#MD"),
       c("PEMBROKE#PINES, FL",
         "PEMBROKE PINES#FL"),
       c("POINT PLEASANT NJ#NJ",
         "POINT PLEASANT#NJ"),
       c("PORT AERANSAS#TX",
         "PORT ARANSAS#TX"),
       c("PORT CANVERAL#FL",
         "PORT CANAVERAL#FL"),
       c("PORT O CANNOR#TX",
         "PORT O CONNOR#TX"),
       c("PORT OCONNOR#TX",
         "PORT O'CONNOR#TX"),
       c("PORT ST.LUICE#FL",
         "PORT ST LUCIE#FL"),
       c("PUNTA GORGA#FL",
         "PUNTA GORDA#FL"),
       c("REDINGTON SHORES#FL, FL",
         "REDINGTON SHORES#FL"),
       c("RIVERIA BEACH#FL",
         "RIVIERA BEACH#FL"),
       c("S PADRE ISLE#TX",
         "S. PADRE ISLAND#TX"),
       c("SEBASTAIN#FL",
         "SEBASTIAN#FL"),
       c("ST AUGUSTIN#FL",
         "ST AUGUSTINE#FL"),
       c("ST PETERSBURG BEACH#FL",
         "ST PETERSBURG#FL"),
       c("STEINAHTCHEE#FL",
         "STEINHATCHEE#FL"),
       c("SUMMRLND KEY#FL",
         "SUMMERLAND KEY#FL"),
       c("SWANQUARTER#FL",
         "SWAN QUARTER#NC"),
       c("TAVENIER#FL",
         "TAVERNIER#FL"),
       c("WANCHEESE#NC",
         "WANCHESE#NC"))
# ---
# Explanations:
# Creating a new column 'city_state' by concatenating trimmed 'city' and 'state' columns, separated by '#'.
vessels_from_pims_split_addr__city_state <-
  vessels_from_pims_split_addr |>
  mutate(city_state =
           paste(
             trimws(city),
             trimws(state),
             sep = "#"
           ))

## check numbers in an address ----
# vessels_from_pims |> 
#   filter(official__ %in% c("1301930",
# "GA1769JL")) |> 
#   View()

vessels_from_pims_split_addr__city_state |>
  filter(grepl("\\d", city_state)) |> 
  select(city_state) |> 
  distinct()
# 1 HO0MASASSA#FL          
# 2 2#AL                   
# 3 FIGURE 8 ISLAND#NC     
# 4 0#                     
# 5 00#                    
# 6 NAPLE4S#FL             
  # 7 LITTLE RIVERNHV1N4WH#SC (fixed)
# 8 22411 GENO LANE#AL     
# 9 117 HAWK LANDING LN#GA 

vessels_from_pims_split_addr__city_state |>
  filter(grepl(",", city_state)) |> 
  select(city_state)
# 1 REDINGTON SHORES#FL, FL
# 2 CHAUVIN#LA, LA         
# 3 ALEXANDER CITY#AL, AL  
# 4 MATLACHA#BOKKELIA, FL  
# 5 PEMBROKE#PINES, FL     


# ---

# 1. **Column Extraction Using sapply:**
#    - The variable 'wrong_port_addr' is created by applying the 'sapply' function to 'to_fix_list'.
#    - The `sapply` function applies the '[' function to each element of 'to_fix_list' using the index 1.
# 
# 2. **Column Extraction Using '[':**
#    - The '[' function is used to extract the first element (index 1) from each element of 'to_fix_list'.
#    - This operation is used to extract a specific column or element from each list or data frame within 'to_fix_list'.
# 
# 3. **Final Result:**
#    - 'wrong_port_addr' holds the result of extracting the first element from each element within 'to_fix_list'.

wrong_port_addr <-
  sapply(to_fix_list, "[", 1)

# ---
# Explanations:
# The function 'get_correct_addr_by_wrong' takes a 'wrong_addr' as input and performs the following steps:
# 1. Finds the index of 'wrong_addr' in the 'to_fix_list'.
# 2. Uses 'tryCatch' to handle errors, printing information about the error and the index if one occurs.
# 3. Extracts the correct address from the pair.
# 4. Returns the correct address.
get_correct_addr_by_wrong <-
  function(wrong_addr) {
    idx <- grep(wrong_addr, to_fix_list)
    
    names_pair <-
      tryCatch(
        to_fix_list[[idx]],
        error = function(e) {
          print(e)
          print(str_glue("Index: {idx}"))
        }
      )
    good_addr <- names_pair[[2]]
    
    return(good_addr)
  }

# Explanations:
# The variable 'compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col_addr__fixed' is created by:
# 1. Creating a new column 'city_state_fixed' by replacing wrong addresses using 'get_correct_addr_by_wrong' for rows where 'city_state' is in 'wrong_port_addr'.
# 2. Separating the 'city_state_fixed' column into two columns ('city_fixed' and 'state_fixed') using '#' as the delimiter.
vessels_from_pims_split_addr__city_state__fix1 <-
  vessels_from_pims_split_addr__city_state |>
  rowwise() |>
  mutate(city_state_fixed =
           if (city_state %in% wrong_port_addr)
             get_correct_addr_by_wrong(city_state)
         else
           city_state) |>
  ungroup() |>
  tidyr::separate_wider_delim(city_state_fixed,
                              delim = "#",
                              names = c("city_fixed",
                                        "state_fixed")) |> 
  distinct()

n_distinct(vessels_from_pims_split_addr__city_state__fix1$vessel_official_number)
# [1] 23045

dim(vessels_from_pims_split_addr__city_state__fix1)
# [1] 23086     6

# add more fixes manually ----
## list of double ports ----
manual_fixes <-
  list(
    list("1112053", "NEW BERN", "NC"),
    list("1166732", "MIAMI", "FL"),
    list("1185107", "KEY WEST", "FL"),
    list("531549", "TOWNSEND", "GA"),
    list("581260", "PONCE INLET", "FL"),
    list("596153", "NEW BERN", "NC"),
    list("646818", "HOUSTON", "TX"),
    list("671353", "SWANSBORO", "NC"),
    list("FL0146BH", "MIAMI", "FL"),
    list("FL1431JU", "MARATHON", "FL"),
    list("FL1553TM", "BILOXI", "MS"),
    list("FL1862SU", "MIAMI", "FL"),
    list("FL2615MT", "STUART", "FL"),
    list("FL3119EE", "BOCA GRANDE", "FL"),
    list("FL3976FH", "PONCE INLET", "FL"),
    list("FL5011MX", "NAPLES", "FL"),
    list("FL5029RM", "KEY WEST", "FL"),
    list("FL5262LD", "LAUDERDALE BY THE SEA", "FL"),
    list("FL7549PJ", "KEY LARGO", "FL"),
    list("FL8000NR", "ST PETERSBURG BEACH", "FL"),
    list("FL8252JK", "MIAMI", "FL"),
    list("LA4017BH", "HACKBERRY", "LA"),
    list("LA6968EP", "LAROSE", "LA"),
    list("NC6164CW", "MOREHEAD CITY", "NC"),
    list("TX9606KA", "HOUSTON", "TX")
  )
    # list("139403", "MIAMI", "FL"), # no!


vessels_from_pims_split_addr__city_state__fix2 <-
  map_df(manual_fixes,
         \(x) {
           # browser()
           res <-
             vessels_from_pims_split_addr__city_state__fix1 |>
             mutate(
               city_fixed1 =
                 case_when(vessel_official_number == x[[1]] ~ x[[2]]),
               state_fixed1 =
                 case_when(vessel_official_number == x[[1]] ~ x[[3]])
             )
           return(res)
         }) |>
  distinct()

dim(vessels_from_pims_split_addr__city_state__fix2)
# [1] 23110     8
# [1] 23109     8

vessels_from_pims_split_addr__city_state__fix2 |>
  filter(vessel_official_number == "FL1431JU") |>
  glimpse()
# $ city_fixed             <chr> "KEY WEST", "MARATHON", "KEY WEST", "MARATHON"
# $ state_fixed            <chr> "FL", "FL", "FL", "FL"
# $ city_fixed1            <chr> NA, NA, "MARATHON", "MARATHON"
# $ state_fixed1           <chr> NA, NA, "FL", "FL"

# check
new_f_vsl <-
  sapply(manual_fixes, "[", 1) |> 
  unlist()

both <-
  intersect(
    vessels_from_pims_split_addr__city_state__fix1$vessel_official_number,
    new_f_vsl
  )
cat(both)
# 15
# 581260 531549 FL8252JK 646818 FL0146BH FL7549PJ 1185107 FL5011MX FL1431JU FL3976FH TX9606KA FL2615MT FL5029RM FL1553TM FL3119EE

vessels_from_pims_split_addr__city_state__fix2 |>
  filter(vessel_official_number %in% both) |>
  select(vessel_official_number,
         city_fixed1,
         state_fixed1) |>
  filter(!is.na(city_fixed1) & !is.na(city_fixed1)) |>
  distinct() |>
  glimpse()
# 16 ok

vessels_from_pims_split_addr__city_state__fix2 |>
  filter(vessel_official_number %in% both) |>
  select(vessel_official_number,
         city_fixed,
         state_fixed,
         city_fixed1,
         state_fixed1) |>
  filter(!is.na(city_fixed1) & !is.na(city_fixed1)) |>
  distinct() |>
  glimpse()

## replace duplicated values ----
# Explanations:
# 1. Updating 'city_fixed' and 'state_fixed' columns based on conditions using 'case_when':
#     - If 'city_fixed1' is not NA, update 'city_fixed' with 'city_fixed1'; otherwise, keep the existing value in 'city_fixed'.
#     - If 'state_fixed1' is not NA, update 'state_fixed' with 'state_fixed1'; otherwise, keep the existing value in 'state_fixed'.
# 2. Filtering rows where 'vessel_official_number' is not in 'both' or 'state_fixed1' is not missing.
# 3. Selecting all columns except "city_fixed1" and "state_fixed1".
# 4. Keeping only distinct rows in the final result to avoid duplications.
vessels_from_pims_split_addr__city_state__fix2_ok <-
  vessels_from_pims_split_addr__city_state__fix2 |>
  mutate(
    city_fixed =
      case_when(!is.na(city_fixed1) ~ city_fixed1,
                .default = city_fixed),
    state_fixed =
      case_when(!is.na(state_fixed1) ~ state_fixed1,
                .default = state_fixed)
  ) |> 
  filter((!vessel_official_number %in% both) |
           !is.na(state_fixed1)) |> 
  select(-c("city_fixed1", "state_fixed1")) |> 
  distinct()

dim(vessels_from_pims_split_addr__city_state__fix2_ok)
# [1] 23086     6

# check
vessels_from_pims_split_addr__city_state__fix2_ok |>
  filter(vessel_official_number %in% both) |>
  select(vessel_official_number,
         city_fixed,
         state_fixed) |> 
  distinct() |>
  glimpse()
# 15

## remove empty and bad vessel ids ----
# introduced by splitting doubles?
is_empty <- c(NA, "NA", "", "UN", "N/A")
wrong_vessel_ids <- c("FL", "FLORIDA", "MD", "NO", "NONE")

normal_length = 4

vessels_from_pims_split_addr__city_state__fix2_ok__good_ids <-
  vessels_from_pims_split_addr__city_state__fix2_ok |>
  filter(!vessel_official_number %in% is_empty) |>
  filter(!vessel_official_number %in% wrong_vessel_ids) |>
  filter(!str_length(vessel_official_number) < normal_length)

# TODO:
## check id_len != 6 ----
# check ids with spaces
vessels_from_pims_split_addr__city_state__fix2_ok__good_ids__len <-
  vessels_from_pims_split_addr__city_state__fix2_ok__good_ids |>
  group_by(vessel_official_number) |>
  mutate(id_len = str_length(vessel_official_number)) |>
  ungroup()

vessels_from_pims_split_addr__city_state__fix2_ok__good_ids__len |> 
  filter(!id_len == 6) |> 
  arrange(id_len) |> 
  glimpse()

dim(vessels_from_pims_split_addr__city_state__fix2_ok)
# [1] 23086     6

dim(vessels_from_pims_split_addr__city_state__fix2_ok__good_ids)
# [1] 23050     6

## check no address ----
vessels_from_pims_split_addr__city_state__fix2_ok__good_ids__no_addr <-
  vessels_from_pims_split_addr__city_state__fix2_ok__good_ids |>
  filter(is.na(city))

nrow(vessels_from_pims_split_addr__city_state__fix2_ok__good_ids__no_addr)
# 6

vessels_from_pims_split_addr__city_state__fix2_ok__good_ids__no_state <-
  vessels_from_pims_split_addr__city_state__fix2_ok__good_ids |>
  filter(is.na(state_fixed))

nrow(vessels_from_pims_split_addr__city_state__fix2_ok__good_ids__no_state)
# 0

## check for double ids/ports ----
vessels_from_pims_split_addr__city_state__fix2_ok__good_ids_short |> 
  distinct() |>
  select(vessel_official_number) |>
  count(vessel_official_number) |>
  filter(n > 1) |>
  nrow()
# 0, ok

# remove extra cols ----
vessels_from_pims_split_addr__city_state__fix2_ok__good_ids_short <-
  vessels_from_pims_split_addr__city_state__fix2_ok__good_ids |>
  select(vessel_official_number, ends_with("_fixed")) |> 
  distinct()

# check
# vessels_from_pims_split_addr__city_state__fix2_ok__good_ids |> 
#   filter(!state == state_fixed) |> 
#   View()

vessels_from_pims_split_addr__city_state__fix2_ok__good_ids |>
  filter(!city == city_fixed) |>
  select(-vessel_official_number) |> 
  distinct() |> 
  nrow()
# 47
# 50

# print out ----
out_dir <- file.path(my_paths$outputs,
            current_project_basename)

dir.create(out_dir)

out_path <- file.path(out_dir,
            "vessels_from_pims_ports.csv")

write_csv(
  vessels_from_pims_split_addr__city_state__fix2_ok__good_ids_short,
  out_path
)
