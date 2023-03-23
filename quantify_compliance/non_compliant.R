# month and year:
#   sa how many reports are missing
# gom

source(r"(~\R_code_github\quantify_compliance\quantify_compliance_start.R)")

## ---- how many reports are missing ----

## ---- count reports ----
compl_clean_sa_vs_gom_plus_dual_non_compl_rep <-
  compl_clean_sa_vs_gom_plus_dual %>%
  filter(tolower(compliant_) == "no") %>%
  # glimpse()
  # Rows: 42,162
  mutate(
    report_cnts = ifelse(
      permit == "sa_only",
      captainreports__ + negativereports__,
      captainreports__ + gom_permitteddeclarations__
    )
  )
# %>% select(report_cnts) %>% unique() %>%
#   glimpse()
# $ report_cnts <int> 0, 1, 6, 14, 2, 3, 7, 21, 5, 18, 17, 8, 16, 9, 10, 4, 51, 22, 1…

## ---- gom + dual ----
# compl_clean_sa_vs_gom_plus_dual %>% select(captainreports__, negativereports__, gom_permitteddeclarations__) %>% unique()
dim(compl_clean_sa_vs_gom_plus_dual_non_compl_rep)
# 42162
gom_non_compl <-
  compl_clean_sa_vs_gom_plus_dual_non_compl_rep %>%
  select(
    vessel_official_number,
    permit,
    week_start,
    year_month,
    year_quarter,
    year,
    report_cnts
  ) %>%
  filter(permit == "gom")

names(gom_non_compl)

gom_non_compl %>%
  data_overview()

gom_non_compl %>%
  select(vessel_official_number, week_start) %>%
  group_by(vessel_official_number) %>%
  summarise(n = n())

# 1000042                    2?
# Groups:   vessel_official_number [493]

compl_clean_sa_vs_gom_plus_dual_non_compl_rep %>%
  filter(vessel_official_number == "1000042") %>%
  glimpse()
#   dim()
# 2299    6

compl_clean_sa_vs_gom_plus_dual %>%
  filter(vessel_official_number == "1000042") %>%
  glimpse()
#   dim()
