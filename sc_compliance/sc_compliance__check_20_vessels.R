# check if this vessels have logbooks
#
# Eric Hiltz HiltzE@dnr.sc.gov via scdnr3.onmicrosoft.com
# Attachments
# 2024-04-22 10:36 AM
# We noticed that there are a few (20) federally permitted vessels that either have their state listed as SC or have a vessel registration # beginning with SC that do not have a current SC charter license.  I’ve attached the vessels as well as their permits and the last time that vessel had a charter license in SC.  Are any of these 20 vessels reporting positive trips to you all?  If so, we’ll have to do some outreach.  These vessels could also be contributing to the differences we’re seeing in compliance rates.

# set up ----
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
