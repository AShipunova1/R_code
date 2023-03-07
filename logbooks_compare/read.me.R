# Comparing the logbook to logbook data between the Validation Survey and the logbooks in FHIER
# From Michelle:
# For the Survey to FHIER logbook comparison, we want to compare the accuracy/inaccuracy, at a trip level. So, are they sending us the same information that they reported to the Surveyor? We'll need to estimate any differences at trip-level reporting, and quantify those differences (if any) across all trips. Just use 2022 data for now.

# the Survey data are in: 
# https://drive.google.com/drive/folders/1D1ksBarjJzvbmqWa5cEE-k0s6ZJi0VAM

## ---- Workflow ----

# From John Foster
# Speaking personally, I think there are two broad and sequenced tasks here: 1) performing a descriptive analysis on the two datasets, and 2) developing the more formal estimation procedures.
# 
# For 2, the answer is definitely 'yes' there are standard estimation procedures for a capture-recapture design, which will involve weighted estimation.  It could all be written in R, and some portions may be done using the package you linked to while others will likely need original coding.
# 
# For 1, I don't know of a specific standard documented approach to follow in this specific situation (but there may be some out there).  In general, I would suggest starting with the unweighted data, generating simple summary count information for domains of interest for both datasets.  Then look at distributions of variables of interest (catch by species and type, others).  I would also look at some data quality measures - range and logic checks, item nonresponse or missingness, digit bias checks, others.  Then I would move on to looking specifically at the differences in catch.  I would suggest calculating the differences as ratios at the trip level as you indicated, then plotting the distributions of those ratios for domains of interest, looking into any patterns and potentially drilling down into the underlying data.  At some point these ratios will need to be calculated using weighted estimation that takes into account the survey design.  That will be necessary to produce unbiased catch estimates, but it needn't be done right away as part of a preliminary descriptive analysis.
# 
# These are just my initial thoughts and certainly aren't exhaustive or definitive in terms of what can or should be done.  Hopefully, they can help to get started, and others may have different ideas to share on how to approach things.