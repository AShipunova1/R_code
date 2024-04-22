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
