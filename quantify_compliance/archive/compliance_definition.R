# "To Be Compliant (for data before week 9 of 2023)"
#
# Leeanne:
# GOM:
#   I'd take out rule 2. Having a logbook submitted for a recreational or non-fishing intended charter declaration isn't required but wouldn't make them non-compliant if they did submit it by accident.
#
# SA:
# After checking FHIER, I've actually seen vessels marked compliant for having only 1 DNF submitted. Sorry for the confusion! So I'd just edit the # of DNFs in your latest definition to 1
# ---
# For GOM:
#   1) Incorrect as there does not need to be an equal number of total declarations and logbooks for compliance. Non-fishing intended, recreational declarations can be submitted and these do not require a logbook. But specifically, the number of fishing intended charter declarations would need to be equal to logbooks to be compliant.
# 2) Trip negative reports are not required for GOM-only permitted vessels
#
# SA:
#   1) There should be at least one logbook or 7 DNFs filed for that week. If Michelle could confirm this - It is technically compliant only if DNFs are submitted for all 7 days within the week
# 2) DNFs can be submitted up to 30 days in advance. So too much is 31 or more days in advance
#
# Dual permits:
#   Correct that before week 9 of 2023, dual permitted vessels had followed GOM reqs.
#
# And correct for noncompliant + overridden are compliant

# Michelle
# to respond to Leeanne - technically they need a DNF for each day of the week, but I think we import those as a weekly DNF report (for compliance tracking in FHIER purposes). This could easily be confirmed in FHIER though - just pick a compliant SA vessel, and see if there is a DNF for each day of a given week, or just 1 for the week.

compl_clean_short_sa_vs_gom %>%
  filter(permit == "sa_only") %>%
  filter(tolower(compliant_) == "yes") %>% dplyr::glimpse()
# negativereports__ == 7


# ---- def ----
# GOM:
# 1) There should be a declaration for every logbook (the number of fishing intended charter declarations would need to be equal to logbooks to be compliant)
# 2) There could be no logbooks for a declaration if it is a recreational ("non-fishing intended, recreational declaration") or not a charter
# 3) declarations can be submitted no more than 30 days in advance

# SA:
# 1) There should be at least one logbook or 7 DNFs filed for any given week except the last one (can submit the following Tuesday).
# 2) DNFs should not be submitted more than 30 days in advance

# Dual:
# Before week 9 of 2023, dual permitted vessels had followed GOM reqs.

# All:
# Noncompliant + overridden are compliant

# =====

# 1) a decl for every logb, but no vise versa
# if it is a recreational or a not charter
# Sorry if I worded that poorly. GOM must have both  (1) a logbook for any fishing intended declaration, and (2) an intended fishing declaration for any logbook. I mean any of the other compliance errors types such as TRIP REPORT SUBMITTED BEFORE DECLARATION or TRIP VALIDATION ERROR will not affect the compliance of the vessel. These other compliance error types will notify us of the error so we can reach out to the fisherman for correction.

# 2) Gom
# negative reports are too much in advance - err -> non_comnpliant
# I think that because it is rejected by the system for being submitted too early it is considered a No Report error

# 3) if it is non-compliant, then look for overriden and
# non_compliant + overriden are compliant

# Only the "No report" (or No Declaration for GOM) validation will affect the compliance, the rest of the validation errors will not affect the compliance. Let me know if this makes sense.

# Manual:
# weekly compliance will roll up to a month, and when a week straddles two months the module will check compliance in both months.

# “Compliance information for (vessel ID)” at the bottom of Vessel Dashboard
# “View” on the left side of the reporting week shows information for that specific compliance period, including the specific compliance errors and all reports submitted during that reporting week.

# GOM:
# In FHIER, click “Reports” then “FHIER Compliance Report”.
# For “Has Error,” select the reporting year and compliance error you want to look at. For Gulf vessels, this is “Declaration without trip report” and “Trip report without declaration.”

# For Gulf-permitted vessels, calls are made to vessels that have the compliance errors of “declaration without trip report” and/or “trip report without declaration”.

# Note: in eTrips, no-fish reports can only be submitted 90 days in the past. If no-fish reports prior to that time period are confirmed by the fisherman, then override for compliance in FHIER.

# ====
source(r"(~\R_code_github\quantify_compliance\quantify_compliance_start.R)")

# View(compl_clean_sa_vs_gom_plus_dual)
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
# err_desc$enabled_
# View(err_desc)

err_desc_used <-
  err_desc %>%
  dplyr::mutate(affects_compliancy = dplyr::case_when(
    grepl("check", enabled_, ignore.case = TRUE) &
      grepl("check", overridable_, ignore.case = TRUE) ~ "YES",
    .default = "NO"
  ))

# err_desc_used %>%
# filter(grepl("check", enabled_, ignore.case = TRUE)) %>%
# dplyr::arrange(overridable_) %>% View()

# === separate by permit ====

compl_clean_short_sa_vs_gom <-
  separate_permits_into_3_groups(compl_clean_short)

# test permit separation ----
# dplyr::glimpse(compl_clean_short_sa_vs_gom)
compl_clean_short_sa_vs_gom %>%
  filter(permit == "sa_only" &
           grepl("G", permitgroup)) %>% head()
# 0

compl_clean_short_sa_vs_gom %>%
  filter(permit == "gom_only" &
           !grepl("G", permitgroup)) %>% head()
# 0
# ===
compl_clean_short_sa_vs_gom %>%
  filter(permit == "sa_only") %>%
  filter(tolower(compliant_) == "no",
         (captainreports__) > 0,
         tolower(overridden_) == "no") %>% View()
# 1 Feb 2023
# ==== check if the same vessel can be sa_only in one week and gom or dual the other ====

sa_only_vessel_id <-
  compl_clean_short_sa_vs_gom %>%
  filter(permit == "sa_only") %>%
  select(vessel_official_number) %>%
  unique()
# 2223

gom_or_dual_vessel_id <-
  compl_clean_short_sa_vs_gom %>%
  filter(!(permit == "sa_only")) %>%
  select(vessel_official_number) %>%
  unique()
# 1518

intersect(sa_only_vessel_id, gom_or_dual_vessel_id) %>% dplyr::glimpse()
# 0

### SA compliant & no reports:
# Leeanne Delrosario - NOAA Affiliate
# to me, Chris, Michelle
#
# Hey Anna, Chris and I finished taking a look at that report you sent over. Majority of those vessels had week 8 of 2023 as the listed week that was compliant when no reports were submitted for the SA vessels. If the report was run on 2/24/23 (during week 8), then it makes sense that those vessels are marked compliant with no reports as the deadline to submit reports for that week had not passed yet (the following Tuesday after the reporting week ends).
#
#
# After filtering out week 8, the rest of the vessels with compliant weeks for no reports were dual Gulf/SA permit holders and it is correct that they would be compliant even with no reports submitted.
#
#

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


# %>%
# dplyr::glimpse()
# ---- compare permits in pims anf fhier compliance ----
str(sa_only_vessel_id)
str(active_permits_from_pims)

g_permits_from_sa_only <-
  inner_join(active_permits_from_pims, sa_only_vessel_id) %>%
  filter(grepl("g", tolower(permit_code))) %>%
  select(vessel_official_number, permit_code, type, ends_with("date")) %>%
  dplyr::arrange(expiration_date)
# %>% View()
# filter(expiration_date > "2021-01-01")

g_permits_types <-
  active_permits_from_pims %>% filter(grepl("g", tolower(permit_code))) %>%
  select(permit_code, type) %>% unique()
# 14

# View(g_permits_types)

g_permits_from_sa_only %>%
  select(permit_code, type) %>%
  dplyr::arrange(type) %>%
  unique()
# GOM
# CHG: Gulf Charter/headboat For Coastal Migratory Pelagic Fish
# RCG: Gulf Charter/headboat For Reef Fish
##? SPGM: Gulf Of Mexico Shrimp
##? GRRS: Gulf Royal Red Shrimp Endorsement

gom_permits_from_sa_only <-
  g_permits_from_sa_only %>%
  filter(permit_code %in% c("CHG", "RCG")) %>%
  # 24
  filter(effective_date < "2023-02-23") %>%
  # 12
  dplyr::arrange(effective_date)
  # filter(year(effective_date) == 2022)
  # 7

gom_permits_from_sa_only_short1 <-
  gom_permits_from_sa_only %>%
  select(vessel_official_number, permit_code)

combined_permits <-
  aggregate(. ~ vessel_official_number,
            data = gom_permits_from_sa_only_short1,
            paste, sep = ","
  )

gom_permits_from_sa_only_short2 <-
  gom_permits_from_sa_only %>%
  select(vessel_official_number, c("effective_date", "expiration_date")) %>%
  dplyr::mutate(across(c("effective_date", "expiration_date"), .fns = as.character))

# Dot notation
# --- combine dates ---
head(gom_permits_from_sa_only_short2)

combined_effective_date <-
  aggregate(. ~ vessel_official_number,
            data = gom_permits_from_sa_only_short2,
            paste, sep = ","
            
  )

combined_permit_dates <-
  inner_join(combined_permits,
           combined_effective_date,
             by = "vessel_official_number")


# ---
gom_permits_from_sa_only_short <-
  gom_permits_from_sa_only %>%
  select(vessel_official_number, permit_code, effective_date, expiration_date) %>%
  dplyr::mutate(across(c("effective_date", "expiration_date"), .fns = as.character))

# str(gom_permits_from_sa_only_short)

combined_permit_date <-
  aggregate(. ~ vessel_official_number,
            data = gom_permits_from_sa_only_short,
            paste, sep = ","
  )

# view(combined_permit_date)

# str(combined_permit_date)

# --- repeat for all dates ----
gom_permits_from_sa_only_all <-
  g_permits_from_sa_only %>%
  filter(permit_code %in% c("CHG", "RCG")) %>%
  dplyr::arrange(effective_date)

gom_permits_from_sa_only_short_all <-
  gom_permits_from_sa_only_all %>%
  select(vessel_official_number, permit_code, effective_date, expiration_date) %>%
  dplyr::mutate(across(c("effective_date", "expiration_date"), .fns = as.character))

gom_permits_from_sa_only_short_all_combined <-
  gom_permits_from_sa_only_short_all %>%
  combine_rows_based_on_multiple_columns_and_keep_all_unique_values(c("vessel_official_number"))

write_csv(gom_permits_from_sa_only_short_all_combined, "gom_permits_from_sa_only_short_all_combined.csv")
