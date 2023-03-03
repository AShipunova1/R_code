# Compare catch in survey vs logbook
# all data for 2022

## ---- Workflow ----
## 1) FHIER data
### need permitgroup or landing info, scientific name (to use for SP_CODE)
### add SP_CODE
### separate gulf and sa permits or fishing place

## 2) MRIP data
### download for region 6 & 7 (GoM by state, SA by state) from https://www.st.nmfs.noaa.gov/SASStoredProcess/do?#
### use SP_CODE, SUB_REG, AREA_X?, TOT_CAT
### combine AREA_X by SUB_REG (sum(TOT_CAT) for each SUB_REG)

## 3) For (1) and (2) get total caught (numbers) for each unique species by permit type or region
# compare counts

# we will need to start looking at catch in comparison to the validation survey and to what MRIP is reporting. I think the 3 of us can start working on this together, but for now - Anna if you can create some code to pull catch by species. So, you'd need to ID the breath of species caught in all logbooks (2022+). Do this by region (gulf vs s atl vessels). Then the total caught (numbers) for each unique species. THen we will move to comparing it to the Validation survey data and MRIP. I have validation survey data outputs, that I have not touched yet - no idea the format. I don't know how to get MRIP estimates, but we can figure it out together.

# All logbook data can be downloaded using FHIER (sometimes lags out), ROracle or SQL developer. I actually just pulled the logbook data yesterday, with ROracle, for a data request. I just put it in the google folder here (https://drive.google.com/drive/folders/1iBSiEt30DkTrzzijOb6IuZwQ1r5zvqhs), where I created some new code for the economist who requested an analysis. It's a big file, so it may not show up in there until later today or tomorrow.

## ---- MRIP fields ----
# landing	Total Harvest (A+B1)	The total number of fish removed from the fishery resource.  May be obtained by summing catch types A (CLAIM) and B1 (HARVEST).
# tot_cat	Total Catch (A+B1+B2)	The number of fish caught but not necessarily brought ashore.  May be obtained by summing catch types A (CLAIM), B1 (HARVEST), and B2 (RELEASE).

# sp_code	Species Code	Species code of fish
# sub_reg	Region	" Subregion code for region of trip
# 4   = North Atlantic (ME; NH; MA; RI; CT) 
# 5   = Mid-Atlantic (NY; NJ; DE; MD; VA) 
# 6   = South Atlantic (NC; SC; GA; EFL) 
# 7   = Gulf of Mexico (WFL; AL; MS; LA) 
# 8   = West Pacific (HI) 
# 11 = U. S. Caribbean (Puerto Rico and Virgin Islands"
# ? We need 6 & 7

## ---- check MRIP year----
# mrip_estimate %>%
#   select(YEAR) %>% unique()
# YEAR
# 1 2022