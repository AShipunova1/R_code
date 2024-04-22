# check if this vessels have logbooks
sc_unlicensed_fed_charters_path <-
  file.path(
    annas_path$inputs,
    current_project_basename,
    "unlicensedFedCharters_04162024.xlsx"
  )

file.exists(sc_unlicensed_fed_charters_path)
T

sc_unlicensed_fed_charters <-
  read.xlsx(sc_unlicensed_fed_charters_path,
            sep.names = "_") |>
  clean_headers()
# View(sc_unlicensed_fed_charters)


# glimpse(compl_override_data__renamed_m_short__m_compl)

# Join sc unlicensed with compliance info ----

sc_unlicensed_fed_charters__compliance__join <-
  left_join(
    sc_unlicensed_fed_charters,
    compl_override_data__renamed_m_short__m_compl,
    join_by(vessel_id == vessel_official_number)
  )

sc_unlicensed_fed_charters__compliance__join |>
  filter(!is.na(is_comp)) |>
  select(vessel_id) |>
  distinct() |>
  count()
# 20

# Join sc unlicensed with logbooks ----

logbooks |> View()
