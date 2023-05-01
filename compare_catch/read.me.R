# To prepare FHIER and MRIP data
# 1) change directories in the source code (twice).
# 1) have useful_functions_module.r in the working directory
# 2) run the compare_catch_data_preparation.R

# Compare catch in survey vs logbook
# all data for 2022

## ---- Workflow ----
# prepare data separately for comparison
# join the data

# ---- ACL data preparation according to Mike and Dominique ----
# ab1 - counts
# a 6 = "sa" and a 7 = "gom"
# 2022 
# new_mode	2=headboat, 3=charterboat, 5=charter/headboat
# new_moden		alpha description of ‘new_mode’
# surveys ?
# filter(!(ds == "SRHS"))
# change NAs to 0

## 1) FHIER data FHIER / Reports / Species Count by Disposition
# or from logbooks
### need permitgroup or landing info, scientific name (to use for SP_CODE)
### add SP_CODE
### separate gulf and sa permits or fishing place

## 2) MRIP data
### O:\Fishery Data\ACL Data\MRIP Based Rec Data(CHTS)\MRIPACLspec_rec81_22wv6_01mar23\mripaclspec_rec81_22wv6_01mar23.csv
# or
#  MRIPACLspec_rec81_22wv6_01Mar23.zip and is available on the Google drive at https://drive.google.com/drive/folders/12wpQ1J7W9_wA2OBqDuxP5tTfi201mTZT?usp=sharing
# 

## 3) For (1) and (2) get total caught (numbers) for each unique species by permit type or region
# compare counts

# we will need to start looking at catch in comparison to the validation survey and to what MRIP is reporting. I think the 3 of us can start working on this together, but for now - Anna if you can create some code to pull catch by species. So, you'd need to ID the breath of species caught in all logbooks (2022+). Do this by region (gulf vs s atl vessels). Then the total caught (numbers) for each unique species. THen we will move to comparing it to the Validation survey data and MRIP. I have validation survey data outputs, that I have not touched yet - no idea the format. I don't know how to get MRIP estimates, but we can figure it out together.

# All logbook data can be downloaded using FHIER (sometimes lags out), ROracle or SQL developer. I actually just pulled the logbook data yesterday, with ROracle, for a data request. I just put it in the google folder here (https://drive.google.com/drive/folders/1iBSiEt30DkTrzzijOb6IuZwQ1r5zvqhs), where I created some new code for the economist who requested an analysis. It's a big file, so it may not show up in there until later today or tomorrow.

## ---- MRIP data ----
# “2022 should be in there minus TX and some LA Bio files”
# ignore the MRIP fes- that is just for effort data, and we’re not there yet
# She said grab MRIP fes data
# That is the charter for-hire survey
# Jessica is wondering if when you pulled from MRIP is there a way, or did you, just select the charter catch and not the full recreational catch
# Charter vs private recreational

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

# Use all
# area_x	Fishing Area	" Collapsed area of fishing 
# 1 = Ocean <= 3 mi (all but WFL) 
# 2 = Ocean > 3 mi (all but WFL) 
# 3 = Ocean <= 10 mi (WFL only) 
# 4 = Ocean > 10 mi (WFL only) 
# 5 = Inland"	CHAR

# ===
# "O:\Fishery Data\ACL Data\MRIP Based Rec Data(CHTS)\MRIPACLspec_rec81_22wv6_01mar2\ACL request_rec documentation_MRIP_01Mar23.docx"

# mode_fx	MRFSS/MRIP only; collapsed fishing mode code (1=man-made, 2=beach/bank, 3=shore, 4=headboat, 5=charter boat, 6=headboat/charter boat, 7=private/rental boat)

# new_mode	recoded mode of fishing used by SFD (1=shore, 2=headboat, 3=charterboat, 4=private boat, 5=charter/headboat, 6=priv/shore)
# ? to use  mode_fx = 5, 6, new_mode = 3, 5
# or agg_moden		aggregation of mode estimates; ‘For-Hire’ or ‘Private’

# ab1			type A + type B1 catch estimate (number of fish killed or kept)

# species		Southeast Region Headboat Survey species code

# itis_code		IT IS species code 

## ---- check MRIP year----
# mrip_estimate %>%
#   select(YEAR) %>% unique()
# YEAR
# 1 2022

# ==== From Mike ====


# Below is what I had in mind to discuss at the meeting today but feel free to edit or modify:
  
  # Start with a landings comparison. I can provide the following MRIP data:
  # *MRIP Charter landings estimates by Gulf of Mexico state, year, and two-month wave.
# *Data for the years of 2021 and 2022
# *I can provide this data for the following species: red snapper, greater amberjack, gag, and gray triggerfish

# If I can get the same data for SEFHIER then we can start comparisons.

# Mike