# spp. is 0
# region is not SA or GOM
glimpse(logbooks_content)

# spp. is 0 ----
logbooks_content %>%
  filter(!!sym(itis_field_name) == "0") %>% glimpse()
# Rows: 89

# grep("common", names(logbooks_content), value = T)
# common_name

logbooks_content %>%
  filter(!!sym(itis_field_name) == "0") %>%
  select(common_name) %>% unique()
# NA

logbooks_content %>%
  filter(!!sym(itis_field_name) == "0") %>%
  select(trip_start_date) %>% unique()
# 70

logbooks_content %>%
  filter(!!sym(itis_field_name) == "0") %>%
  write.csv(file = "logbooks_content_sp0.csv", row.names = F)
