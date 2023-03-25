# GOM - should be equal # of decl and logb  
# For SA, they need either a no fishing report weekly (due Tuesday following the fishing week, which is Mon-Sun) or they need a logbook for every fishing trip (so at least 1 per week if no fishing report is submitted)
## non-compliant vessels for the GOM, if 65 vessels are missing 1 report (1) is that a missing declaration or logbook?, (2) how many did they submit correctly (e.g. this month in question maybe they are missing 1 logbook, but they submitted 20. So, 1/20 = 5% missing). That is a vessel level, so then perhaps take the average of all for the month, and then the year, by region, and plot it that way.

#
library(grid)
source(r"(~\R_code_github\quantify_compliance\quantify_compliance_start.R)")


compl_clean_sa_vs_gom_plus_dual_short <-
  compl_clean_sa_vs_gom_plus_dual %>%
  select(vessel_official_number,
         year,
         week,
         gom_permitteddeclarations__,
         captainreports__,
         negativereports__,
         complianceerrors__,
         compliant_,
         week_start,
         permit,
         year_month,
         year_quarter
  )
  
  # filter(permit == "sa_only")
# %>% names() %>% cat()
# select(-report2, -permit) # don't need' gom_permitteddeclarations__,
# )
  
## separate by permit
permit_dfs <- split(compl_clean_sa_vs_gom_plus_dual_short,
                    compl_clean_sa_vs_gom_plus_dual_short$permit)
setNames(permit_dfs, c("gom", "sa"))

str(permit_dfs)

# gom_compl_clean_sa_vs_gom_plus_dual_short <-
#   compl_clean_sa_vs_gom_plus_dual_short %>%
#   filter(permit == "gom")
# 
# identical(permit_dfs[[1]], gom_compl_clean_sa_vs_gom_plus_dual_short)
# T
