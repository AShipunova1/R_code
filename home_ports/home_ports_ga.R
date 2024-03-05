# Q:
# 63 federally permitted vessels home ported in Georgia for 2024 (us)
# I redownloaded that FOIA spreadsheet (attached) just now, and when I filter it down to just the three South Atlantic charter permits (CDW, CHS and SC) for the “vessel state” field limited to “GA” and deleted duplicate vessel names, I get 44 unique vessels. (GA)

# select by year, port and permit ----
vessels_from_pims__ga_vessel_ids <-
  vessels_from_pims |>
  filter(grepl(", GA", hailing_port)) |>
  select(official__) |> 
  distinct()

# vessels_from_pims__ga |> 
#   select(hailing_port) |> 
#   distinct() |> 
#   View()

permits_from_pims__split1_short__split2_short <- 
permits_from_pims__split1_short__split2 |> 
    select(vessel_official_number, permit, effective_date, expiration_date) |> 
    distinct()

permits_from_pims__split1_short_2023 <-
  permits_from_pims__split1_short__split2_short |>
  filter(effective_date <= my_end2 &
           expiration_date >= my_beginning2) |>
  select(vessel_official_number, permit) |>
  distinct()

dim(permits_from_pims__split1_short_2023)
# [1] 12880     2

vessel__permit__join <- 
  left_join(vessels_from_pims__ga_vessel_ids,
            permits_from_pims__split1_short_2023,
            join_by(official__ == vessel_official_number),
            relationship = "many-to-many")

vessel__permit__join %>%
  filter(complete.cases(.)) |> 
  # tibble [169 × 2] (S3: tbl_df/tbl/data.frame)
  filter(permit %in% c("CDW", "CHS", "SC")) |> 
  # tibble [90 × 2] (S3: tbl_df/tbl/data.frame)
  select(official__) |> 
  distinct() |> 
  nrow()
# 34


# 1 639616     CDW   
# 2 639616     CHS   
# 3 639616     SC    
# 4 693851     ADW   

# compare with GA results ----
ga_xlsx_path <-
  file.path(
    my_paths$inputs,
    r"(home_ports\FOIA+Vessels+All as of NOAA permit website 2_26_24.xlsx)"
  )

# file.exists(ga_xlsx_path)

ga_xlsx1 <-
  read_xlsx(
    
    ga_xlsx_path,
    sheet = 1,
    col_types = c(
      "text",
      "skip",
      "skip",
      "text",
      "skip",
      "skip",
      "skip",
      "skip",
      "skip",
      "text",
      "date",
      "date",
      "date",
      "date"
    )
    # .name_repair = clean_headers
  )

# OFFICIAL_NUMBER	VESSEL_NAME	HAILING_PORT_CITY	VESSEL_STATE	ENTITY_NAME	ADDRESS	CITY	POSTAL_CODE	ADDRESS_STATE	FISHERY_NAME_ABBR	PERMIT_EFFECTIVE_DATE	PERMIT_EXPIRATION_DATE	PERMIT_TERMINATION_DATE	PERMIT_END_DATE

# View(ga_xlsx1)

ga_xlsx1_ga_only_short_23 <-
  ga_xlsx1 |>
  select(OFFICIAL_NUMBER,
         VESSEL_STATE,
         FISHERY_NAME_ABBR,
         starts_with("PERMIT")) |>
  filter(VESSEL_STATE == "GA") |>
  filter(FISHERY_NAME_ABBR %in% c("CDW", "CHS", "SC")) |>
  filter(PERMIT_EFFECTIVE_DATE <= my_end2 &
           PERMIT_EXPIRATION_DATE >= my_beginning2)
  
n_distinct(ga_xlsx1_ga_only_short_23$OFFICIAL_NUMBER)
# 43

# View(ga_xlsx1_ga_only_short_23)

from_non_compl_areas <- 
  read_csv(r"(my_outputs\home_ports\ga_vessel_permits_state_fixed_23_vessel_ids.csv)")

in_ga_xlsx <- 
  setdiff(ga_xlsx1_ga_only_short_23$OFFICIAL_NUMBER,
        from_non_compl_areas$vessel_official_number)
  # [1] "639616"  "1178074" "1322973"

in_mine <- 
  setdiff(from_non_compl_areas$vessel_official_number,
          ga_xlsx1_ga_only_short_23$OFFICIAL_NUMBER)

glimpse(in_mine)
 # chr [1:22] "1187937" "1307944" "1311002" "1323935" "542775" "906483" ...

in_mine_only <-
  permits_from_pims__split1_short__split2 |>
  filter(vessel_official_number %in% in_mine)

in_mine_only |> 
  select(permit) |> 
  distinct()
# 10

n_distinct(in_mine_only$vessel_official_number)
# 21

in_mine_only |> 
  filter(permit %in% c("CDW", "CHS", "SC")) |> 
  select(vessel_official_number) |> 
  distinct()
# 21

# check dates ----
vessel__permit__join__mine_ga <-
  vessel__permit__join |>
  filter(official__ %in% in_mine)

View(vessel__permit__join__mine_ga)
