# check if sold info is correct ----
prev_result |> 
    select(vessel_official_number, notes) |> unique() |> print()

prev_result |>
    select(vessel_official_number, notes) |>
    filter(!is.na(notes)) |>
    unique() |>
    filter(grepl("sold|own", notes, ignore.case = T)) |>
    select(vessel_official_number) |>
    paste(sep = ", ")

