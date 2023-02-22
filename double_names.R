# Find vessels that have more than one vessel ID associated with them over time

# Get common functions
source("~/R_code_github/useful_functions_module.r")

## ---- set up ----
my_paths <- set_work_dir()

## ---- get safis data ----
csv_names_list = list("all_vessels_safis.csv")

# read csv file with EOF within quoted strings
read_csv_w_eofs <- function(my_paths, csv_names_list) {
    my_inputs <- my_paths$inputs
    # add input directory path in front of each file name.
    myfiles <- sapply(csv_names_list, function(x) file.path(my_inputs, x))
    
    # read all csv files
    contents <- sapply(myfiles, fread, header = TRUE)
    # convert the first one into a data frame
    # TODO change this function to deal with multiple files
    contents[, 1] %>% 
      as.data.frame() %>% 
      return()
}

csv_contents <- read_csv_w_eofs(my_paths, csv_names_list)
csvs_clean <- clean_headers(csv_contents)
# data_overview(csvs_clean)

## ---- convert dates ----
date_fields <- c("entrydate", "updatedate", "de", "dc")
date_format = "%m/%d/%Y"
safis_clean <- 
  change_fields_arr_to_dates(csvs_clean, date_fields, date_format)
  
# data_overview(safis_clean)

double_names_pairs <- 
  safis_clean %>%
    filter(coastguard != statereg) %>% 
    select(coastguard, statereg) %>% 
    unique()
# > dim(double_names_pairs)
# [1] 141670      2
# > dim(unique(double_names_pairs))
# [1] 138507      2

# %>%
  # unique() %>% dim()
# 16 ==
# 130649 !=

## ---- find FHIER correspondence and compliance data using both ----
## ----- get csv data into variables -----
temp_var <- get_compl_and_corresp_data(my_paths)
compl_clean <- temp_var[[1]]
corresp_clean <- temp_var[[2]]


used_doube_pairs <- double_names_pairs %>%
  filter(coastguard %in% compl_clean$vesselofficialnumber &
           statereg != "-") 
# 94

used_doube_pairs <- double_names_pairs %>%
  filter(statereg %in% compl_clean$vesselofficialnumber &
            coastguard != "-") %>% add_row(used_doube_pairs)
# 6

used_doube_pairs <- double_names_pairs %>%
  filter(coastguard %in% corresp_clean$vesselofficialnumber &
           statereg != "-") %>% add_row(used_doube_pairs)
# 74
used_doube_pairs <- double_names_pairs %>%
  filter(statereg %in% corresp_clean$vesselofficialnumber &
           coastguard != "-") %>% add_row(used_doube_pairs)
# 20

used_doube_pairs_u <- unique(used_doube_pairs)
# 100

# write.csv(used_doube_pairs_u, "used_doube_pairs_u.csv")

# check
corresp_clean %>% 
  filter(grepl("1303026|FL1815RG", vesselofficialnumber)) %>%
  select(vesselofficialnumber) %>% unique()
  # select(-starts_with("week")) %>% unique()

compl_clean %>% 
  filter(grepl("1303026|FL1815RG", vesselofficialnumber)) %>%
  select(vesselofficialnumber) %>% unique()

corresp_clean %>% 
  filter(grepl("1303026|FL1815RG", vesselofficialnumber)) %>%
  select(vesselofficialnumber) %>% unique()

# slow
# TODO add munbers to the output
foo <- function(x, output = "myfile.txt", num) {
  # browser()
  
  pair <- paste(x[[1]],
                x[[2]], sep = "|")
  
  in_corr <- corresp_clean %>% 
    filter(grepl(pair, vesselofficialnumber)) %>%
    select(vesselofficialnumber) %>% 
    # select(vesselofficialnumber, contact_freq) %>% 
    unique() 
  # %>%
    # lapply(write, file = output, append = TRUE)
  
  in_compl <- compl_clean %>% 
    filter(grepl(pair, vesselofficialnumber)) %>%
    select(vesselofficialnumber) %>% 
    unique() 
  # %>%
    # lapply(write, file = output, append = TRUE)
  if (nrow(in_corr) > 2 | nrow(in_compl) > 2) {
    # browser()
    print(pair)
  }
  else if (!identical(in_corr, in_compl)) {
    my_out <- paste0(num <- num + 1, "\n")
    # write(num, file = output, append = TRUE)
    my_out <- paste0(my_out, pair, "\n")
    # write(pair, file = output, append = TRUE)
    my_out <- paste0(my_out, "correspondence", "\n")
    # write("correspondence", file = output, append = TRUE)
    # browser()
    
    my_out <- paste0(my_out, paste(in_corr), "\n")
      # lapply(in_corr, function(x) paste0(my_out, x)) %>%
    # lapply(in_corr, paste0, my_out)
    # lapply(in_corr, write, file = output, append = TRUE)
    my_out <- paste0(my_out, "compliance", "\n")
    # write("compliance", file = output, append = TRUE)
    # lapply(in_compl, write, file = output, append = TRUE)
    my_out <- paste0(my_out, paste(in_compl), "\n")
    my_out <- paste0(my_out, "\n")
    
    write(my_out, file = output, append = TRUE)
  }
  
}

num = 0
apply(used_doube_pairs_u, 1, foo, output = 'outputfile.txt', num)

