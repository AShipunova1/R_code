source("~/R_code_github/useful_functions_module.r")
my_paths <- set_work_dir()

# get data ----
con <- connect_to_secpr()

override_cmts_query <-
  "select * from
srh.srfh_vessel_comp@secapxdv_dblk.sfsc.noaa.gov
where comp_override_cmt is not NULL
"

override_cmts_data =
  ROracle::dbGetQuery(con,
                      override_cmts_query)

ROracle::dbDisconnect(con)

# look at data ----
dim(override_cmts_data)
# [1] 17132    23

data_overview(override_cmts_data)
# SAFIS_VESSEL_ID        1538
# VESSEL_OFFICIAL_NBR    1536
# COMP_YEAR                 3
# COMP_OVERRIDE_CMT      1492
#   COMP_YEAR      COMP_WEEK_START_DT              
# Min.   :2021   Min.   :2021-01-03 23:00:00.00  
# Max.   :2023   Max.   :2023-05-29 00:00:00.00  

# View(override_cmts_data)

# process comments ----

vesl_override_cmts <-
  grep("vesl", override_cmts_data$COMP_OVERRIDE_CMT, 
     ignore.case = T, value = T)
# length(vesl_override_cmts)
# 7521

# View(vesl_override_cmts)

override_cmts_data_sep_cmts <-
  override_cmts_data %>%
  mutate(vesl_comment = case_when(
    grepl("vesl", COMP_OVERRIDE_CMT,
          ignore.case = T) ~ COMP_OVERRIDE_CMT,
    .default = ""
  )) %>%
  mutate(etrips_comment = case_when(
    grepl("ETRIPS", COMP_OVERRIDE_CMT,
          ignore.case = T) ~ COMP_OVERRIDE_CMT,
    .default = ""
  )) %>%
  mutate(scdnr_comment = case_when(
    grepl("SCDNR", COMP_OVERRIDE_CMT,
          ignore.case = T) ~ COMP_OVERRIDE_CMT,
    .default = ""
  )) %>%
  mutate(safis_comment = case_when(
    grepl("SAFIS", COMP_OVERRIDE_CMT,
          ignore.case = T) ~ COMP_OVERRIDE_CMT,
    .default = ""
  )) %>%
  mutate(submitted_comment = case_when(
    grepl("SUBMITTED", COMP_OVERRIDE_CMT,
          ignore.case = T) ~ COMP_OVERRIDE_CMT,
    .default = ""
  )) %>%
  mutate(gom_comment = case_when(
    grepl("dual|gom|feb 23|lawsuit", COMP_OVERRIDE_CMT,
          ignore.case = T) ~ COMP_OVERRIDE_CMT,
    .default = ""
  )) %>%
  mutate(cancel_comment = case_when(
    grepl("CANCELED|CANCELLED", COMP_OVERRIDE_CMT,
          ignore.case = T) ~ COMP_OVERRIDE_CMT,
    .default = ""
  )) %>%
  arrange(desc(COMP_WEEK_END_DT)) %>% 
  filter(vesl_comment == "" & 
           etrips_comment == "" &
           scdnr_comment == "" &
           safis_comment == "" &
           submitted_comment == "" &
           gom_comment == "" &
           cancel_comment == "") 
# before gom_comment 2,932 entries

override_cmts_data_sep_cmts %>% 
  select(COMP_OVERRIDE_CMT, COMP_WEEK_END_DT) %>% 
  View()

override_cmts_data_sep_cmts %>%
  # count(COMP_OVERRIDE_CMT, COMP_YEAR) %>% View()
  filter(COMP_YEAR > "2021") %>%
  count(COMP_OVERRIDE_CMT) %>%
  filter(n > 1) %>%
  View()
# 303 

grep("PUSHED", override_cmts_data_sep_cmts$COMP_OVERRIDE_CMT,
          ignore.case = T, value = T) %>%
  # 35
  unique()
# [1] "7/31 HMS LOGBOOK, NOT PUSHED TO FHIER."                      
# [2] "7/20 AND 7/21 LOGBOOKS WITH HMS SPECIES, NOT PUSHED TO FHIER"
# [3] "HMS LOGBOOK, NOT PUSHED TO FHIER."                           
# [4] "LOGBOOK WITH HMS SPECIES, NOT PUSHED TO SRFH"                
# [5] "LOGBOOK WITH HMS SPECIES, NOT PUSHED TO SRFH."               
# [6] "LOGBOOKS WITH HMS SPECIES, NOT PUSHED TO SRFH"               
# [7] "NO FISHING REPORTS NOT PUSHED TO SRFH"                       
