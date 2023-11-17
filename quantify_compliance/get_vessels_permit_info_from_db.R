dates_filter <- " (end_date >= TO_DATE('01-JAN-21', 'dd-mon-yy')
    OR expiration_date >= TO_DATE('01-JAN-21', 'dd-mon-yy') )
  AND effective_date <= CURRENT_DATE
"
# Use that "dates_filter" in all parts of the union below.

# The 3 part union is needed because while the permit table has only one vessel id, the vessel table has 3 different columns for that (sero_official_number, coast_guard_nbr and state_reg_nbr) and we want to join tables by all 3 in turn.
# stringr::str_glue is a function that allows you to create strings with placeholders for variable values. It works by using curly braces {} to enclose variable names within a string.
vessels_permits_query <-
  stringr::str_glue("SELECT
  *
FROM
       srh.mv_sero_fh_permits_his@secapxdv_dblk.sfsc.noaa.gov p
  JOIN safis.vessels@secapxdv_dblk.sfsc.noaa.gov
  ON ( p.vessel_id = sero_official_number )
WHERE {dates_filter}
UNION ALL
SELECT
  *
FROM
       srh.mv_sero_fh_permits_his@secapxdv_dblk.sfsc.noaa.gov p
  JOIN safis.vessels@secapxdv_dblk.sfsc.noaa.gov
  ON ( p.vessel_id = coast_guard_nbr )
WHERE
  {dates_filter}
UNION ALL
SELECT
  *
FROM
       srh.mv_sero_fh_permits_his@secapxdv_dblk.sfsc.noaa.gov p
  JOIN safis.vessels@secapxdv_dblk.sfsc.noaa.gov
  ON ( p.vessel_id = state_reg_nbr )
WHERE
{dates_filter}
")

try(con <- connect_to_secpr())

tic("get vessels_permits")
vessels_permits <-
  dbGetQuery(con,
             vessels_permits_query)
toc()
# get vessels_permits: 23.75 sec elapsed

dim(vessels_permits)
# [1] 81162    51

my_file_path <- 
  file.path(my_paths$inputs,
            "quantify_compliance",
            "vessels_permits.rds")

readr::write_rds(vessels_permits,
                 my_file_path)
