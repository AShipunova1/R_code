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

# check if there was a permit for the whole period ----
permit_info_from_db |> 
    glimpse()

have_sa_permits <- 
    permit_info_from_db |> 
    filter(!grepl("G", TOP))

dim(have_sa_permits)
# [1] 24988    22

have_sa_permits_new <- 
    have_sa_permits |> 
    filter(lubridate::year(EFFECTIVE_DATE) >= '2023')

dim(have_sa_permits_new)
# [1] 8803   22

have_sa_permits_new |> 
    glimpse()

the_current_period <- 
    lubridate::interval(start = half_year_ago,
                        end = permit_expired_check_date)

# [1] 2023-12-15 UTC--2024-07-21 UTC

tictoc::tic("have_sa_permits_new__int")
have_sa_permits_new__int <-
    have_sa_permits_new |>
    rowwise() |>
    mutate(
        first_end = min(EXPIRATION_DATE, END_DATE, na.rm = TRUE),
        permit_interval =
               lubridate::interval(start = EFFECTIVE_DATE, 
                                   end = first_end)) |>
    ungroup()
tictoc::toc()
# have_sa_permits_new__int: 37.39 sec elapsed

glimpse(have_sa_permits_new__int)
