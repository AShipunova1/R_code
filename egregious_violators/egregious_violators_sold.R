# check if sold info is correct ----
prev_result |> 
    select(vessel_official_number, notes) |> unique() |> print()

prev_result |>
    select(vessel_official_number, notes) |>
    filter(!is.na(notes)) |>
    unique() |>
    filter(grepl("sold", notes, ignore.case = T)) |>
    select(vessel_official_number) |>
    print()

prev_result |>
  filter(
    confirmed_egregious___permits_must_still_be_active_till_2024_06_16__missing_past_6_months__and__1__they_called_emailed_us__incoming___or__2__at_least_2_contacts__outgoing__with_at_least_1_call_other__voicemail_counts__and_at_least_1_email_ == "No"
  ) |>
  head(1) |>
  str() |>
  glimpse()

prev_result |>
  filter(
    confirmed_egregious___permits_must_still_be_active_till_2024_06_16__missing_past_6_months__and__1__they_called_emailed_us__incoming___or__2__at_least_2_contacts__outgoing__with_at_least_1_call_other__voicemail_counts__and_at_least_1_email_ == "No"
  ) |>
  # View()
  # filter(vessel_official_number == '527954') |>
  head(1) |>
  select(vessel_official_number) |> 
  str()
 # $ vessel_official_number: chr "527954"

prev_result |>
  filter(vessel_official_number == "527954") |>
  str()

prev_result |>
  filter(vessel_official_number == '527954') |> 
  glimpse()
