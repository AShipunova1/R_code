source("~/R_code_github/useful_functions_module.r")
my_paths <- set_work_dir()
current_project_dir_path <-
  get_current_file_directory()
current_project_dir_name <- basename(current_project_dir_path)

source(file.path(my_paths$git_r,
                 r"(get_data\misc_info.R)"))

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
         start_port_name,
         end_port_name,
         start_port,
         end_port) |>
  distinct()

all_logbooks_db_data_2022_short_p_region_short |>
  filter(!start_port == end_port &
           start_port_name == end_port_name) |>
  glimpse()
# 2

# all_logbooks_db_data_2022_short_p_region_short |>
#   filter(start_port == end_port &
#            !start_port_name == end_port_name) |>
# str()
# 0

# how many SEFHIER vessels start at a different location than they end; ----
all_logbooks_db_data_2022_short_p_region_short |>
  # filter(!start_port == end_port) |>
  filter(!start_port_name == end_port_name) |>
  select(vessel_id,
         vessel_official_nbr,
         permit_region) |>
  distinct() |>
  # dim()
  # Rows: 397
  count(permit_region)
# 1  gom_and_dual 198
# 2       sa_only 199
# by name:
# 1  gom_and_dual 198
# 2       sa_only 197

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

all_logbooks_db_data_2022_short_p_region_short_all_port_names_by_vsl <-
  all_logbooks_db_data_2022_short_p_region_short_all_ports_by_vsl |>
  group_by(vessel_id, vessel_official_nbr) |>
  mutate(all_start_port_names = toString(unique(start_port_name)),
         all_end_port_names   = toString(unique(end_port_name))) |>
  mutate(all_start_port_names_num = length(str_split(all_start_port_names, ",")),
         all_end_port_names_num   = length(str_split(all_end_port_names, ","))) |>
  ungroup()

?unite()
all_logbooks_db_data_2022_short_p_region_short_all_ports_by_vsl |>
  # View()
  filter(all_start_ports_num > 1) |>
  dim()
# [1] 1890    9

all_logbooks_db_data_2022_short_p_region_short_all_port_names_by_vsl |>
  filter(!all_end_ports_num == all_end_port_names_num) |>
  dim()
# 0

all_logbooks_db_data_2022_short_p_region_short_all_port_names_by_vsl |>
  filter(vessel_official_nbr == 1000042) |>
  View()

# quantify the # of vessels who fish in both the gulf and S Atl. ;
all_logbooks_db_data_2022_short_p_region_port <-
  all_logbooks_db_data_2022_short_p_region |>
  select(vessel_id,
         vessel_official_nbr,
         permit_region,
         contains("port"),
         -starts_with("notif")) |>
  remove_empty_cols() |>
  distinct()

dim(all_logbooks_db_data_2022_short_p_region_port)
# [1] 3579   19
# [1] 3011   11 -starts_with("notif")

all_logbooks_db_data_2022_short_p_region_port |>
  select(start_port_state) |>
  distinct() |>
  head(2)

names(state.abb) <- state.name
names(state.name) <- state.abb

# my_state_name[tolower("FL")]
# "Florida"

all_logbooks_db_data_2022_short_p_region_port_states <-
  all_logbooks_db_data_2022_short_p_region_port |>
  mutate(
    start_port_state_name = my_state_name[tolower(start_port_state)],
    end_port_state_name   = my_state_name[tolower(end_port_state)]
  ) |>
  mutate(
    start_port_reg =
      case_when(
        tolower(start_port_state_name) %in% tolower(sa_council_states) ~
          "sa_council_state",
        tolower(end_port_state_name) %in% tolower(east_coat_states$gom) ~
          "gom_state",
        .default = "sa_state"
      )
    # diff_reg = case_when(!start_port_state == end_port_state)
  )

glimpse(all_logbooks_db_data_2022_short_p_region_port_states)



# look at permit home port vs where they take trip. ----

