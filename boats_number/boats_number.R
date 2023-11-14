source("~/R_code_github/useful_functions_module.r")
my_paths <- set_work_dir()
current_project_dir_path <-
  get_current_file_directory()
current_project_dir_name <- basename(current_project_dir_path)

source(file.path(my_paths$git_r,
                 r"(get_data\all_logbooks_db_data_2022_short_p_region_prep.R)"))

# all_get_db_data_result_l
# all_logbooks_db_data_2022_short_p_region
# ls()

# View(all_logbooks_db_data_2022_short_p_region)

# shorten ----
all_logbooks_db_data_2022_short_p_region_short <-
  all_logbooks_db_data_2022_short_p_region |>
  select(vessel_id,
         vessel_official_nbr,
         permit_region,
         start_port,
         end_port) |>
  distinct()

# how many SEFHIER vessels start at a different location than they end; ----
all_logbooks_db_data_2022_short_p_region_short |>
  filter(!start_port == end_port) |>
  select(vessel_id,
         vessel_official_nbr,
         permit_region) |>
  distinct() |>
  # dim()
  # Rows: 397
  count(permit_region)
# 1  gom_and_dual 198
# 2       sa_only 199

# if keep ports:
# dim()
# 499
# 1  gom_and_dual 273
# 2       sa_only 226

# how many vessels have variable landing locations (i.e., in the winter they are in one state while in the summer they fish in another); ----

all_logbooks_db_data_2022_short_p_region_short_all_ports_by_vsl <-
  all_logbooks_db_data_2022_short_p_region_short |>
  group_by(vessel_id, vessel_official_nbr) |>
  mutate(all_start_ports = toString(unique(start_port)),
         all_end_ports   = toString(unique(end_port))) |>
  mutate(all_start_ports_num = length(str_split(all_start_ports, ",")),
         all_end_ports_num   = length(str_split(all_end_ports, ","))) |>
  ungroup()


all_logbooks_db_data_2022_short_p_region_short_all_ports_by_vsl |>
  # View()
  filter(all_start_ports_num > 1) |>
  glimpse()

  # filter(!start_port == end_port)
  # select(vessel_id,
  #        vessel_official_nbr,
  #        permit_region) |>
  # distinct() |>


