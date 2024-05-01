\name{read.me}
\alias{read.me}

\title{egregious_violators}

Data download:
FHIER / Correspondence
FHIER / Reports / FHIER Compliance Report

South Atlantic vessels egregious violators are:
27+ weeks non compliance, starting 6 month ago
have direct communication

===
1) If there was 1 call or 2 emails (out and in, bc they got the email, we shared the information and received a confirmation) with a direct communication.
# to investigation (to NEIS)

2) need an email:
# no direct contact or no phone number or all are voicemails
# and exclude if they had an email and if they had at least one phone call!

3) find if not compliant and no correspondence

===
# download the compliance details and filter out those South Atlantic vessels that have never reported, and then check that short list against # of calls/emails (need at least 2, and if never spoken too/responded then they'd need a certified letter from OLE)
# From Leeanne:
# You can download that report from the FHIER compliance report. Within that report you can refine the search parameters like the 2022 year and for "Has Error" at the top, dplyr::select "No report". The "no report" error will limit the report to Atlantic/South Atlantic vessels that are non-compliant for no reports. You will have to specifically filter out the vessels that are egregious.
# An egregious violator, in the past, is considered as a vessel that has not reported at all (either all 52 weeks out of the year or since permit issue if they were issued new permits throughout the year) but has been contacted (called/emailed) at least twice since the program began Jan 4, 2021. 
# *) pull all, filter "no report" in R instead of "has error"
# TODO: compare with the given

# --- Workflow: ---
# get compliance report for 2022-2023 (Reports/FHIER COMPLIANCE REPORT)
# get correspondence report for all years
# upload to R
# add correspondence counts
# only SA permits, exclude those with Gulf permits
# Gulf of Mexico (Gulf) federal for-hire permits: 
#   Charter/Headboat for Reef fish permit (RCG)
#   Historical Captain Charter/Headboat for Reef fish permit (HRCG)
#   Charter/Headboat for Coastal Migratory Pelagic fish permit (CHG) 
#   Historical Captain Charter/Headboat for Coastal Migratory Pelagic fish (HCHG) permit 
# South Atlantic/Atlantic (South Atlantic) federal for-hire permits: 
#   South Atlantic Charter/Headboat for Coastal Migratory Pelagic fish (CHS) permit
#   Atlantic Charter/Headboat for Dolphin/wahoo (CDW) permit
#   South Atlantic Charter/Headboat for Snapper-grouper fish (SC) permit
# keep only specific entries (not a voicemail) (no filter by a reason) bc they should already know about the program from any kind of communication
# 12 months of compliance errors (no reports) for one vessel (or since permit issue if they were issued new permits throughout the year)
# # GOM PermittedDeclarations	# CaptainReports	# NegativeReports	# ComplianceErrors
# # 0	0	0	1
# contacttype: other == email for now

# Filters:
# 1) SA permits only
#  & > 51 week with no reports
#  & (a direct contact call | 
#     emails in and out)
# TODO: add "have permit less than 52 weeks, but no reports"

## Output:
# For the egregious output files, we'll need the following columns:
# Vessel name
# vessel ID
# Permit type(s) list
# Permit expirations, in order of types, as list
# Owner name
# owner address
# owner phone #
# list of non-compliant weeks
# list of contact dates and contact type in parentheses 
