# 1) a decl for every logb, but no vise versa
# if it is a recreational or a not charter
# 2) Gom
# negative reports are too much in advance - err -> non_comnpliant
# 3) if it is non-compliant, then look for overriden and 
# non_compliant + overriden are compliant 
# Only the "No report" (or No Declaration for GOM) validation will affect the compliance, the rest of the validation errors will not affect the compliance. Let me know if this makes sense.


source(r"(~\R_code_github\quantify_compliance\quantify_compliance_start.R)")

View(compl_clean_sa_vs_gom_plus_dual)
compl_clean_short <-
  compl_clean_sa_vs_gom_plus_dual %>%
  select(
    captainreports__,
    complianceerrors__,
    compliant_,
    gom_permitteddeclarations__,
    negativereports__,
    overridden_,
    permit,
    permitgroup,
    permitgroupexpiration,
    vessel_official_number,
    week_end,
    week_num,
    week_start,
    year_month,
    year_quarter,
  )

# error descriptions ====
err_desc$enabled_

err_desc_used <-
  err_desc %>%
  mutate(affects_compliancy = case_when(
    grepl("check", enabled_, ignore.case = TRUE) &
      grepl("check", overridable_, ignore.case = TRUE) ~ "YES",
    .default = "NO"
  ))

err_desc_used %>%
  filter(grepl("check", enabled_, ignore.case = TRUE)) %>%
  arrange(overridable_) %>% View()

# === separate by permit ====

compl_clean_short_sa_vs_gom <- separate_permits_into_3_groups(compl_clean_short)

# test permit separation ----
# glimpse(compl_clean_short_sa_vs_gom)
compl_clean_short_sa_vs_gom %>%
  filter(permit == "sa_only" &
           grepl("G", permitgroup)) %>% head()
# 0

compl_clean_short_sa_vs_gom %>%
  filter(permit == "gom_only" &
           !grepl("G", permitgroup)) %>% head()
# 0
# === different permits in the report and the dashboard ====
# (!) has a gom permit in the "vessel dashboard" and
# no gom repots in the "permitgroup" column of the FHIER compliance report
sa_compliant__no_reports <-
  compl_clean_short_sa_vs_gom %>%
  filter(permit == "sa_only") %>%
  filter(tolower(compliant_) == "yes",
       (captainreports__ + negativereports__) == 0) %>%
  # 1408
    filter(!(week_start >= "2023-02-20"))
# 246

permit_names_list = r"(other\Permits_2023-03-29_1611_active_eff_after2020.csv)"

active_permits_from_pims <-
  load_csv_names(my_paths, permit_names_list)
View(active_permits_from_pims)
