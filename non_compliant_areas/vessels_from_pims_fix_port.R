vessels_from_pims_fix_port__space <-
  vessels_from_pims |>
  select(official__, vessel_id, owner, hin, hailing_port) |>
  filter(grepl(" ,", hailing_port) | 
           grepl("  ", hailing_port)) |> 
  distinct()
nrow(vessels_from_pims_fix_port__space)
# 349   
# 357 with "  "

write_csv(
  vessels_from_pims_fix_port__space,
  file.path(current_output_dir,
            "vessels_from_pims_fix_port__space.csv")
)

vessels_from_pims_fix_port__digit <-
  vessels_from_pims |>
  select(official__, vessel_id, owner, hin, hailing_port) |>
  filter(grepl("\\d", hailing_port)) |> 
  distinct() |> 
  filter(!official__ %in% c("NC6421AU",
                            "1301930",
                            "GA1769JL"))
# OK names
# FIGURE 8 ISLAND, NC
# 22411 GENO LANE, AL
# 117 HAWK LANDING LN, GA

nrow(vessels_from_pims_fix_port__digit)
# 53

write_csv(
  vessels_from_pims_fix_port__digit,
  file.path(current_output_dir,
            "vessels_from_pims_fix_port__digit.csv")
)

vessels_from_pims_fix_port__none <-
  vessels_from_pims |>
  select(official__, vessel_id, owner, hin, hailing_port) |>
  filter(is.na(hailing_port)) |>
  distinct()

nrow(vessels_from_pims_fix_port__none)
# 18

write_csv(
  vessels_from_pims_fix_port__none,
  file.path(current_output_dir,
            "vessels_from_pims_fix_port__none.csv")
)
