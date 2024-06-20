# check if sold info is correct ----
prev_result |> 
    select(vessel_official_number, notes) |> unique() |> print()
