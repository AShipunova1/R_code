# compare field names ----

# if (!require('taxize')) {
#   install.packages('taxize')
#   library('taxize')
# }

if (!require('ritis')) {
  install.packages('ritis')
  library('ritis')
}

lgb_join_i1__t_diff_short__w_int_all_dup_rm__int_dup_rm_short |>
  print_df_names()

db_logbooks_2022 |> 
  print_df_names()

survey_data_l_2022$i3 |> 
  # print_df_names()
  glimpse()

w_i3 <-
  survey_data_l_2022$i3 |>
  right_join(
    lgb_join_i1__t_diff_short__w_int_all_dup_rm__int_dup_rm_short,
    join_by(id_code),
    relationship = "many-to-many"
  )
# ℹ Row 1780 of `x` matches multiple rows in `y`.
# ℹ Row 242 of `y` matches multiple rows in `x`.

# View(w_i3)

w_lgb <-
  db_logbooks_2022 |>
  right_join(
    lgb_join_i1__t_diff_short__w_int_all_dup_rm__int_dup_rm_short,
    join_by(TRIP_ID)
  )

# View(w_lgb)

fields_df <-
  names(db_logbooks_2022) |> t() |> t() |>
  as.data.frame()

names(fields_df) <- "db_logbooks"

# fields_df

fields_survey_data_l <-
  survey_data_l_2022 |>
  purrr::map(names)

str(fields_survey_data_l)
# tidyr::fill

## add logb names ----
fields_survey_data_l[[length(fields_survey_data_l) + 1]] <- 
  names(db_logbooks_2022)

length(fields_survey_data_l)
# 6

# View(fields_survey_data_l)

all_fieds_df <-
  tibble::tibble(V = fields_survey_data_l) %>%
  unnest_wider(V, names_sep = "") |> 
  t()

str(all_fieds_df)
colnames(all_fieds_df) <- c(names(fields_survey_data_l)[1:(length(names(fields_survey_data_l)) - 1)], "logbooks")

# View(all_fieds_df)

# names(fields_survey_data_l)

# View(fields_survey_data__df)

all_fieds_df |>
  as.data.frame() |> 
  readr::write_csv("all_fieds_validation_s__lgb.csv")

## compare permit fields ----
survey_data_l_2022$i1$permit_number1 |> head()
db_logbooks_2022$ACCSP_PERMIT_LICENSE_NBR |> head()
db_logbooks_2022$SERO_VESSEL_PERMIT |> head()

intersect(survey_data_l_2022$i1$permit_number1,
          db_logbooks_2022$SERO_VESSEL_PERMIT)  |> 
  head()
  # length()
1 (NA)

intersect(survey_data_l_2022$i1$permit_number1,
          db_logbooks_2022$ACCSP_PERMIT_LICENSE_NBR) |>
  head()
# 4

intersect(survey_data_l_2022$i1$permit_number1,
          db_logbooks_2022$GARFO_VESSEL_PERMIT) |>
  head()
1 (NA)

View(db_logbooks_2022)

db_logbooks_2022$CATCH_SPECIES_ITIS |> 
  unique() |> 
  head()

taxize::get_tsn("BLACKFIN TUNA")

taxize::classification("172427", db = "itis")
# 17   Thunnus atlanticus      species 172427

taxize::downstream("172427", db = "itis", downto = "species")

taxize::itis_downstream("172418", downto = "species")

taxize::lowest_common("172427", db = "itis")
taxize::tax_name("172427", get = "species")
# 0
taxize::tax_rank("172427", db = "itis")
---
ritis::accepted_names("172427")

any_match_count("172427")

common_names("172427")

full_record("172427")

# $scientificName$combinedName
# [1] "Thunnus atlanticus"

# $scientificName$unitName1
# taxonName
rank_name("172427")

sci_n <- ritis::scientific_name("172427")
sci_n$combinedname
# [1] "Thunnus atlanticus"