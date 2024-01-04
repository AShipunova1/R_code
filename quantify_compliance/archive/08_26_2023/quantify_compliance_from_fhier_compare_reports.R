source("~/R_code_github/quantify_compliance/quantify_compliance_from_fhier_2022.R")

# compare vessels per region with Jenny's ----
# Download the permit data from the FHIER data management web application. Use the FHIER Compliance Count report to create a list of SA only permitted vessels. Set the start and end fields to the date range you are interested in. (You may have to download data in smaller chunks if it times out on you.) Only display the columns of data you need. Download the file as csv and put it in the same directory that has this R file. Rename the file PermitInfo.

## get data ----
j_file_name = r"(my_inputs\quantify_compliance\PermitInfo.csv)"

vessels_per_permit_from_j <-
  read_csv(j_file_name) %>% 
  clean_headers()

# print_df_names(vessels_per_permit_from_j)
vessels_per_permit_from_j_vsl <-
  vessels_per_permit_from_j %>% 
  select(vessel_official_number) %>% 
  dplyr::arrange(vessel_official_number) %>% 
  unique()
#   dim()
# 2205    

compl_clean_sa_vs_gom_m_int_22_sa <-
  compl_clean_sa_vs_gom_m_int %>%
  filter(year == "2022" &
           permit_sa_gom == "sa_only")

compl_clean_sa_vs_gom_m_int_22_sa_vsl <-
  compl_clean_sa_vs_gom_m_int_22_sa %>% 
  select(vessel_official_number) %>% 
  dplyr::arrange(vessel_official_number) %>%
  unique()

# dim(compl_clean_sa_vs_gom_m_int_22_sa_vsl)
# 2178    

## compare ----
identical(vessels_per_permit_from_j_vsl$vessel_official_number,
          compl_clean_sa_vs_gom_m_int_22_sa_vsl$vessel_official_number)
# F

in_j_only <-
  setdiff(
    vessels_per_permit_from_j_vsl$vessel_official_number,
    compl_clean_sa_vs_gom_m_int_22_sa_vsl$vessel_official_number
  )
# 137

in_a_only <-
  setdiff(
    compl_clean_sa_vs_gom_m_int_22_sa_vsl$vessel_official_number,
    vessels_per_permit_from_j_vsl$vessel_official_number
  )
# 110

## Search for in_j_only in a ----
head(in_j_only)
# [1] "1000051" "1020792" "1021103" "1035832" "1041705" "1041927"

in_a_and_j_dual <-
  compl_clean_sa_vs_gom_m_int %>%
  filter(year == "2022") %>% 
  filter(vessel_official_number %in% in_j_only) %>% 
  select(vessel_official_number, permitgroup) %>% 
  unique()

# dim(in_a_and_j_dual)
  # 74
# View(in_a_and_j_dual)
in_a_and_j_dual %>% 
  filter(!grepl("RCG|HRCG|CHG|HCHG", permitgroup, ignore.case = T))
# 0

# 74 out of 2205 had dual permits

## 137 - 74 = 63 why not in a? ----

setdiff(in_j_only,
        in_a_and_j_dual$vessel_official_number)

# no compliance info? 
# Ask Jenny where she got her compliance info

# grep("FL5722SJ", in_a_and_j_dual$vessel_official_number, 
#      ignore.case = T)
# T

grep("992461", in_j_only, 
     ignore.case = T)
# T

grep("992461", in_a_only, 
     ignore.case = T)
# F

compl_clean_sa_vs_gom_m_int %>%
  # filter(year == "2022") %>% 
  filter(vessel_official_number == "992461") %>% 
  View()

# no compliance info on FHIER

## Search for in_a_only in j ----

# print_df_names(vessels_per_permit_from_j_vsl)
vessels_per_permit_from_j_vsl %>%
  filter(vessel_official_number %in% in_a_only) %>% 
  # select(vessel_official_number, permitgroup) %>% 
  unique() %>%
  dim()
# 0

paste0(in_a_only, collapse = ", ")
# 8 of them are dual

## get all compliance count from fhier reports for 2022 ----
all_permits_from_fier_file_name <- r"(my_inputs\quantify_compliance\Vessel Compliance Count Details.csv)"

all_permits_from_fier <-
  readr::read_csv(all_permits_from_fier_file_name) %>% 
  clean_headers()
# Rows: 3528 Columns: 4                                                                             
print_df_names(all_permits_from_fier)

in_a_and_c_cnts_8 <-
  all_permits_from_fier %>%
  filter(vessel_official_number %in% in_a_only)
# 8

# 659890
# in compliance report - permit group == "(CDW)CDW,(CHS)CHS,(SC)SC"
# in compliance counts - dual
# in vessel dashboard CHG	
# err in compliance report?

# # in a and not in compl cnts ----
str(in_a_and_c_cnts_8)

in_a_and_not_in_compl_counts <-
  compl_clean_sa_vs_gom_m_int %>%
  filter(year == "2022") %>%
  filter(!(
    vessel_official_number %in% in_a_and_c_cnts_8$vessel_official_number
  )) %>%
  filter((vessel_official_number %in% in_a_only)) %>%
  unique()

in_a_and_not_in_compl_counts %>%
  select(vessel_official_number, permitgroup, year) %>%
  unique() %>%
  dim()
# 102

glimpse(in_a_and_not_in_compl_counts)
# SC5388DG not in count compl
# in_a_and_not_in_compl_counts %>% 
#   select(permitgroup) %>%
#   unique() %>%
#   View()

# in_a_and_not_in_compl_counts %>%
#   select(vessel_official_number, permitgroup, week_end) %>%
#   unique() %>%
#   dplyr::count(week_end) %>% 
#   View()

# Difference between FHIER Compliance report and compliance counts
