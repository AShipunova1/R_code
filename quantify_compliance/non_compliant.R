# month and year:
#   sa how many reports are missing
# gom

source(r"(~\R_code_github\quantify_compliance\quantify_compliance_start.R)")

## ---- how many reports are missing ----

## ---- count reports ----
compl_clean_sa_vs_gom_plus_dual_rep <-
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

# compl_clean_sa_vs_gom_plus_dual %>% select(captainreports__, negativereports__, gom_permitteddeclarations__) %>% unique()

compl_clean_sa_vs_gom_plus_dual_rep