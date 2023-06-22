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
  mutate(comment_group = case_when(
    grepl("vesl", COMP_OVERRIDE_CMT,
          ignore.case = T) ~ "vesl_comment",
    grepl("ETRIPS", COMP_OVERRIDE_CMT,
          ignore.case = T) ~ "etrips_comment",
    grepl("SCDNR", COMP_OVERRIDE_CMT,
          ignore.case = T) ~ "scdnr_comment",
    grepl("SAFIS", COMP_OVERRIDE_CMT,
          ignore.case = T) ~ "safis_comment",
    grepl("VMS", COMP_OVERRIDE_CMT,
          ignore.case = T) ~ "vms_comment",
    grepl("submitted_comment", COMP_OVERRIDE_CMT,
          ignore.case = T) ~ "submitted_comment",
    grepl("dual|gom|feb 23|lawsuit", COMP_OVERRIDE_CMT,
          ignore.case = T) ~ "gom_comment",
    grepl("CANCELED|CANCELLED", COMP_OVERRIDE_CMT,
          ignore.case = T) ~ "cancel_comment",
    .default = ""
  )) 

override_cmts_data_sep_cmts_other <-
  override_cmts_data_sep_cmts %>% 
  arrange(desc(COMP_WEEK_END_DT)) %>% 
  filter(comment_group == "")

override_cmts_data_sep_cmts_other %>% 
  select(COMP_OVERRIDE_CMT, COMP_WEEK_END_DT) %>% 
  View()

override_cmts_data_sep_cmts_other %>%
  # count(COMP_OVERRIDE_CMT, COMP_YEAR) %>% View()
  filter(COMP_YEAR > "2021") %>%
  count(COMP_OVERRIDE_CMT) %>%
  filter(n > 1) %>%
  View()

grep("PUSHED", override_cmts_data_sep_cmts_other$COMP_OVERRIDE_CMT,
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

