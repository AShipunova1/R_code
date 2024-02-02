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

duplicated_addr_vsls <-
  vessels_from_pims__vessels_from_metrics_short_addr__fixed |>
  group_by(vessel_official_number) |>
  mutate(nn = n_distinct(city)) |>
  filter(nn > 1) |>
  select(vessel_official_number) |> 
  distinct()

vessels_from_pims_duble_addr <- 
  vessels_from_pims |> 
  filter(official__ %in% duplicated_addr_vsls$vessel_official_number) |> 
  arrange(official__)

write_csv(vessels_from_pims_dubles,
          file.path(current_output_dir,
                    "vessels_from_pims_duble_addr.csv"))

to_fix_list_df <-
  to_fix_list |> 
  as.data.frame(
    col.names = NULL,
    row.names = c("wrong_addr", "fixed_addr"),
    # fix.empty.names = TRUE,
    # check.names = !optional,
    stringsAsFactors = FALSE
  ) |> 
  t() |> 
  as.tibble() |>
  mutate(across(everything(), ~str_replace(., "#", ", ")))

# View(to_fix_list_df)
# class(to_fix_list_df)

# rownames(to_fix_list_df) <- NULL

write_csv(to_fix_list_df,
          file.path(current_output_dir,
                    "vessels_from_pims_addr_fixed.csv"))

