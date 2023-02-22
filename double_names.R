# find some of the vessels that have more than one vessel ID associated with them over time
# Get common functions
source("~/R_code_github/useful_functions_module.r")

# ----set up----
my_paths <- set_work_dir()

## ---- get csv data into variables ----
temp_var <- get_compl_and_corresp_data(my_paths)
compl_clean <- temp_var[[1]]
corresp_clean <- temp_var[[2]]

# data_overview(compl_clean)
# vesselofficialnumber      3715

# df1 %>%
#   group_by_(.dots = names(df1)[3:6]) %>%
#   filter(n_distinct(c1) > 1)
compl_clean %>%
  select(vesselofficialnumber, name) %>%
  unique() %>%
  # group_by(name) %>%
  add_count(name, vesselofficialnumber, name = "id_freq") %>% 
  filter(id_freq > 1) %>% str()
    # filter(n_distinct(vesselofficialnumber) > 1)
  0
  