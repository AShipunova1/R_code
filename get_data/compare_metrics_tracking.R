source("~/R_code_github/useful_functions_module.r")
my_paths <- set_work_dir()

common_dir <-
  file.path(my_paths$inputs,
  r"(from_Fhier\Detail Report - via Valid and Renewable Permits Filter (SERO_NEW Source))")

fhier_reports_metrics_tracking_file_names_2022_old <-
  file.path(common_dir,
            "Detail_Report_12312021_12312022__08_23_2023.csv")

fhier_reports_metrics_tracking_file_names_2022_new <-
  file.path(common_dir, "Detail_Report_12312021_12312022__01_26_2024.csv"
    )

fhier_reports_metrics_tracking_2022_old <-
  read.csv(fhier_reports_metrics_tracking_file_names_2022_old,
           stringsAsFactors = FALSE) |>
  clean_headers()
# data = read.csv("data.csv", encoding="UTF-8")

# View(fhier_reports_metrics_tracking_2022_old)

fhier_reports_metrics_tracking_2022_new <-
  read.csv(fhier_reports_metrics_tracking_file_names_2022_new,
           stringsAsFactors = FALSE
           ) |>
  clean_headers()

fhier_reports_metrics_tracking_2022_old[, sapply(fhier_reports_metrics_tracking_2022_old, is.character)] <-
  sapply(fhier_reports_metrics_tracking_2022_old[, sapply(fhier_reports_metrics_tracking_2022_old, is.character)],
         iconv,
         "WINDOWS-1252",
         "UTF-8")

fhier_reports_metrics_tracking_2022_new[, sapply(fhier_reports_metrics_tracking_2022_new, is.character)] <-
  sapply(fhier_reports_metrics_tracking_2022_new[, sapply(fhier_reports_metrics_tracking_2022_new, is.character)],
         iconv,
         "WINDOWS-1252",
         "UTF-8")

# View(fhier_reports_metrics_tracking_2022_old)

dim(fhier_reports_metrics_tracking_2022_old)
# 3634
dim(fhier_reports_metrics_tracking_2022_new)
# 3646

diffdf::diffdf(fhier_reports_metrics_tracking_2022_old,
               fhier_reports_metrics_tracking_2022_new)

# diff

vessels_in_old_only <-
  setdiff(
    fhier_reports_metrics_tracking_2022_old$vesselofficialnumber,
    fhier_reports_metrics_tracking_2022_new$vesselofficialnumber
  )
# [1] "1194571"  "SC1063DR" "FL2488TE" "NC1075EA" "FL1767LJ" "FL4491NW" "1206370"
# [8] "FL2940RH" "FL5799MK"
length(vessels_in_old_only)
# 9

vessels_in_new_only <-
  setdiff(
    fhier_reports_metrics_tracking_2022_new$vesselofficialnumber,
    fhier_reports_metrics_tracking_2022_old$vesselofficialnumber
  )
length(vessels_in_new_only)
# 21

vessels_in_both <-
  intersect(
    fhier_reports_metrics_tracking_2022_new$vesselofficialnumber,
    fhier_reports_metrics_tracking_2022_old$vesselofficialnumber
  )

length(vessels_in_both)
# 3625

n_distinct(fhier_reports_metrics_tracking_2022_old$vesselofficialnumber)
# 3634

n_distinct(fhier_reports_metrics_tracking_2022_new$vesselofficialnumber)
# 3646 (== total vessels, ok)

# Total Vessels	Total Vessels With SA Only	Total Vessels With GOM Permit Only	Total Dual (SA & GOM) Permitted Vessels
# 3,646	2,316	1,022	308

# check the diff for intersect

old_inters_sort <-
  fhier_reports_metrics_tracking_2022_old |>
  filter(vesselofficialnumber %in% vessels_in_both) |>
  arrange(vesselofficialnumber, permits, effectivedate, enddate)

# names(fhier_reports_metrics_tracking_2022_new)

new_inters_sort <-
  fhier_reports_metrics_tracking_2022_new |>
  filter(vesselofficialnumber %in% vessels_in_both) |>
  arrange(vesselofficialnumber, permits, effectivedate, enddate)

glimpse(old_inters_sort)
glimpse(new_inters_sort)

dir.exists(file.path(my_paths$outputs, "get_data"))

sink(file = file.path(my_paths$outputs, "get_data", "metrics_tracking_diff.txt"))
diffdf::diffdf(old_inters_sort, new_inters_sort)
sink()

# the diff:
  #              Variable               No of Differences
  # ------------------------------------------------------
  #             vesselname                      55
  #            effectivedate                     1
  #               enddate                       13
  #  totaltripnotificationsfishingi...           4
  #  totaltripnotificationsnofishin...           6
  #            totallogbooks                   148
  #       totaldidnotfishreports               155

# First 10 of 55 rows are shown in table below
#
#   ==============================================================
#     VARIABLE   ..ROWNUMBER..        BASE            COMPARE
#   --------------------------------------------------------------
#    vesselname        85       GONE FISHINâ¿¿ V  GONE FISHIN’ V
#    vesselname        87          LAST HOOK        MISS PENNY
#    vesselname       232         TAIL2 SAILS      TAIL 2 SAILS
#    vesselname       243        XXX                    XXX
#    vesselname       269           UNNAMED       FISHPROCHARTERS
#    vesselname       519         MIA AMORÃ¿        MIA AMORÈ
#    vesselname       538           FAM JAM           FAM BAM
#    vesselname       575           NO DOUBT        NO DOUBT 2
#    vesselname       658           HOMERUN          HOME RUN
#    vesselname       674           KEY LIME       TACKLE CENTER
