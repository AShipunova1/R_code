library(readxl)  # reading in .xlsx
# the file is from Jeannette Oct 17 2023
v_list_file_name <-
  r"(C:\Users\anna.shipunova\Documents\R_files_local\vessels_permits\SA.Permitted.Vessels.Among_revised.Lists.xlsx)"

sheets <- seq(1:4)
all_sheets_l <-
  map(sheets,
      function(sheet_num) {
        read_excel(
          v_list_file_name,
          sheet = sheet_num,
          # use my fix_names function for col names
          # .name_repair = fix_names,
          guess_max = 21474836,
          # read all columns as text
          col_types = "text",
          .name_repair = "universal"
        )
      })

# names(all_sheets_l[[4]])
# names(all_sheets_l[[1]])
vessels_22_sa <-
  all_sheets_l[[4]] |>
  filter(group %in% c(1, 3)) |>
  select(permit_vessel_id) |>
  rbind(all_sheets_l[[1]])

dim(vessels_22_sa)
# [1] 2321    1

# compare with db_data ----

all_get_db_data_result_l
