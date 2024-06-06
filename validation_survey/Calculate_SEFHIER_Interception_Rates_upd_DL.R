################################################################################
############           DETERMINE SEFHIER INTERCEPTION RATE           ###########
################################################################################

##  LOAD PACKAGES YOU WILL NEED:
pkgs <- c(
  "dplyr",
  "readr",
  "readxl",
  "pivottabler",
  "ggplot2",
  "haven",
  "writexl",
  "sas7bdat",
  "data.table",
  "openxlsx",
  "scales",
  "RColorBrewer",
  "tidyr"
)
lapply(pkgs, require, character.only = TRUE)

##  CREATE THEME FOR PLOTTING
ptheme <-  theme(
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  #  text = element_text(family = "serif"),
  axis.title = element_text(face = "bold", size = "16"),
  axis.text.x = element_text(size = 14),
  axis.text.y = element_text(size = 14),
  strip.text.x = element_text(size = 16),
  strip.text.y = element_text(size = 16),
  legend.title = element_text(size = 16, face = "bold"),
  legend.text = element_text(size = 14),
  plot.title = element_text(face = "bold", size = "24", hjust =
                              0.5)
)
## CUSTOM COLOR PALETTES
or_pal <- c("#E67919", "#FBC02D", "#FAE8CF", "#F0AC96", "#BB3F3F")
pur_pal <- c("#8E24AA", "#CE93D8", "#D1C4E9", "#673AB7", "#463B7A")
gr_pal <- c("#AFB42B", "#9CCC65", "#C7E9C0", "#41AB5D", "#006D2C")
bl_pal <- c("#496389", "#6BAED6", "#BBDEFB", "#2196F3", "#2734D2")

pal5 <- c("#E67919", "#673AB7", "#2734D2", "#41AB5D", "")
pal5 <- c("#009E73", "#56B4E9", "#0072B2", "#D55E00", "#CC79A7")

## PREFIXES FOR SAS FILENAMES
out1 <- "C:\\Users\\Dominique.lazarre\\Documents\\IPTs\\Gulf\\SEFHIER Investigations\\plots\\"

## PREFIXES FOR SAS FILENAMES
pre <- "C:\\Users\\Dominique.lazarre\\Documents\\IPTs\\Gulf\\SEFHIER Investigations\\SEFHIER DATA\\"
## INDICATE LOCATION OF ALL INPUT FILES
inp1 <- paste0(pre, "aga_10_21to02_23.csv")
inp2 <- paste0(pre, "i1_10_21to02_23.csv")
inp3 <- paste0(pre, "Trip Notifications Detail (1.01.22-6.30.23).csv")
inp4 <- paste0(pre, "SAFIS_TripsDownload_1.1.22-8.17.23.csv")
inp5 <- paste0(pre, "Valid and Renewable Permits - SEFHIER.csv")
inp6 <- paste0(pre, "ref_10_21to02_23.csv")

## READ IN FILES
aga <- read_csv(inp1, guess_max = 200000)
i1 <- read_csv(inp2, guess_max = 200000)
dec <- read_csv(inp3, guess_max = 200000)
slog <- read_csv(inp4, guess_max = 200000)
perm <- read_csv(inp5, guess_max = 200000)
ref <- read_csv(inp6, guess_max = 200000)

##############                  PERMIT DATA                   #############
## SELECT ONLY PERTINENT PERMIT DATA (VESSEL NUMBER, VESSEL NAME, PERMITS) &
## RENAME COLUMNS TO CREATE SHORTER MORE EASILY USED NAMES
perms <- perm %>% mutate(
  vsl_num = `Vessel Official Number`,
  vsl_name = `Vessel Name`,
  SA_PERM = `SA Permits?`,
  GOM_PERM = `GOM Permits?`,
  Perm_Group = `Permit Grouping Region`
) %>%
  ## SELECT ONLY THE RENAMED COLUMNS
  select(vsl_num, vsl_name, SA_PERM, GOM_PERM, Perm_Group)

##############                  VALIDATION DATA                   #############

## CREATE ID CODE VARIABLE IN AGA DATA
i1 <- i1 %>% mutate(id_code = format(ID_CODE, scientific = FALSE),
                    asg_code = substr(id_code, 1, 13))

aga <- aga %>% mutate(asg_code = format(ASG_CODE, scientific = FALSE))

ref <- ref %>% mutate(id_code = format(ID_CODE, scientific = FALSE),
                      asg_code = substr(id_code, 1, 13))

## SELECT ONLY THE NEEDED, NON-DUPLICATIVE FIELDS IN THE A FILES
a_files <- aga %>% select(asg_code,
                          ASG_CODE,
                          ALL_SITE_COMMENTS,
                          REASON1,
                          REASON2,
                          SITEHRS)

## JOIN THE I1 AND LIMITED A DATA
i1a <- i1 %>% left_join(a_files, by = c("asg_code")) %>%
  ## NO TYPE DISCARD OR CATCH RECORDS
  mutate(
    nodat_ints = case_when((NUM_TYP2 == 0 & NUM_TYP3 == 0) ~ 1, TRUE ~ 0),
    ## ALL RECORDS
    comp_ints = 1,
    ## IDENTIFY STATE WITH WORDS
    State = case_when(
      ST == "01" ~ "AL",
      ST == "12" ~ "FL",
      ST == "22" ~ "LA",
      ST == "28" ~ "MS",
      ST == "48" ~ "TX",
      ## ADD STATE NAMES BY COUNTY FOR STATES WITH NA IN ST FIELD
      is.na(ST) == TRUE & CNTY == 3 ~ "AL",
      is.na(ST) == TRUE & CNTY == 47 ~ "MS",
      is.na(ST) == TRUE & CNTY == 315 ~ "TX",
      is.na(ST) == TRUE &
        CNTY %in% c(5, 15, 17, 33, 71, 81, 87, 91, 103, 115) ~ "FL",
      is.na(ST) == TRUE &
        CNTY == 75 & INTSITE %in% c(6, 232, 311) ~ "LA",
      is.na(ST) == TRUE & CNTY == 315 ~ "TX"
    ),
    ## CREATE MONTH AND YEAR VARIABLES
    MONTH = as.character(substr(id_code, 10, 11)),
    YEAR = as.character(YEAR),
    YR_MON = paste0(YEAR, "_", MONTH),
    ## RENAME VSL_NUM AND MAKE ALL LETTERS UPPERCASE
    vsl_num = toupper(VSL_NUM)
  ) %>%
  ## KEEP ONLY 2022 DATA (06/2022-12/2022)
  filter(YEAR == "2022") %>%
  ## JOIN THE PERMIT DATA WITH THE VALIDATION SURVEY DATA RECORDS
  left_join(perms, by = "vsl_num") %>%
  ## DESIGNATE GULF TRIPS (AL, TX, MS, AND LA TRIPS ARE ALWAYS CODED AS GOM)
  mutate(
    GulfTrip = case_when(
      State %in% c("AL", "TX", "MS", "LA") ~ 1,
      ## DESIGNATE ALL FL TRIPS IN GULF COUNTIES AS GULF TRIPS
      State == "FL" &
        CNTY %in% c(
          33,
          113,
          91,
          131,
          5,
          45,
          37,
          129,
          65,
          123,
          29,
          75,
          17,
          53,
          101,
          57,
          103,
          81,
          115,
          15,
          71,
          21
        ) ~ 1,
      ## LIST OF GULF COUNTY NAMES IN THE SAME ORDER AS COUNTY CODES ABOVE
      # escambia, santa rosa, okaloosa, walton, bay, gulf, franklin, wakulla,
      # jefferson, taylor, dixie, levy, citrus, hernando, pasco, hillsborough,
      # pinellas, manatee, sarasota, charlotte, lee, collier
      ## FL TRIPS IN MONROE COUNTY WITH GULF PERMITS ARE CODED AS GULF
      State == "FL" & CNTY == 87 & Perm_Group == "GOM" ~ 1,
      ## ASSIGN ANY RECORD WITH NO STATE, BUT VERIFIED GOM PERMIT ARE CODED AS GULF
      is.na(State == T) & Perm_Group == "GOM" ~ 1,
      ## ASSIGN RECORDS IN COUNTY=75 WITH NO STATE TO GULF (FL OR LA)
      is.na(State == T) & CNTY %in% c(75) ~ 1,
      TRUE ~ 0
    )
  )

## CHECK WHICH TRIPS DON'T FALL INTO THE GULF TRIP UMBRELLA - DEALT WITH ABOVE
## STEPWISE, - 29 RECORDS REMOVED (24 - SA ONLY PERMIT, 5 - KEYS WITH NO PERMIT DATA)
chk_gom <- i1a %>% filter(GulfTrip == 0)
table(chk_gom$State, chk_gom$Perm_Group)
table(chk_gom$CNTY, chk_gom$Perm_Group)
table(i1a$GulfTrip)

## FILTER OUT THE SA TRIPS USING THE GULF TRIP FILTER
i1a <- i1a %>% filter(GulfTrip == 1)

## SUM OF THE INTERVIEW COUNTS BY STATE, YEAR AND MONTH
int_cnts_stym <- i1a %>% group_by(State, YEAR, MONTH) %>%
  summarize(tot_nodat = sum(nodat_ints),
            tot_comp = sum(comp_ints))

#SELECT ONLY THE NEEDED FIELDS FOR REFUSAL FILES
refs <- ref %>% mutate(
  refs = 1,
  ## CREATE MONTH AND YEAR VARIABLES
  MONTH = as.character(substr(id_code, 10, 11)),
  YEAR = as.character(YEAR),
  ## IDENTIFY STATE WITH WORDS
  State = case_when(
    ST == "01" ~ "AL",
    ST == "12" ~ "FL",
    ST == "22" ~ "LA",
    ST == "28" ~ "MS",
    ST == "48" ~ "TX",
    ## ADD STATE NAMES BY COUNTY FOR STATES WITH NA IN ST FIELD
    is.na(ST) == TRUE & CNTY == 3 ~ "AL",
    is.na(ST) == TRUE & CNTY == 47 ~ "MS",
    is.na(ST) == TRUE & CNTY == 315 ~ "TX",
    is.na(ST) == TRUE &
      CNTY %in% c(5, 15, 17, 33, 71, 75, 81, 87, 91, 103, 115) ~ "FL",
    is.na(ST) == TRUE & CNTY == 75 &
      INTSITE %in% c(6, 232, 311) ~ "LA",
    is.na(ST) == TRUE & CNTY == 315 ~ "TX"
  )
)

## SUM OF THE INTERVIEW COUNTS BY STATE, YEAR AND MONTH
ref_cnts_stym <- refs %>% group_by(State, YEAR, MONTH) %>%
  summarize(tot_refs = sum(refs))

int_cnts_stym <- int_cnts_stym %>% left_join(ref_cnts_stym, by = c("YEAR", "MONTH", "State")) %>%
  ## REPLACE NA'S IN REF FIELD WITH 0S
  mutate_at(c('tot_refs'), ~ replace_na(., 0)) %>%
  mutate(tot_ints = tot_comp + tot_refs)

## PLOT OF COMPLETED INTERVIEWS BY MONTH AND STATE
p1 <- ggplot(data = int_cnts_stym,
             aes(
               x = MONTH,
               y = tot_comp,
               group = State,
               color = State,
               shape = State
             )) +
  geom_line(linewidth = 2) +
  geom_point(size = 4, stroke = 2) +
  xlab("Month") + ylab("Completed Interviews") +
  scale_shape_manual("State", values = c(15, 1, 2, 9, 19)) +
  scale_color_manual("State", values = pal5) +
  theme_bw() + ptheme
## SAVE GRAPH
ggsave(
  p1,
  file = paste0(out1, "ints_month.png"),
  width = 10,
  height = 7
)

## PLOT OF REFUSALS BY MONTH AND STATE
p1 <- ggplot(data = int_cnts_stym,
             aes(
               x = MONTH,
               y = tot_refs,
               group = State,
               color = State,
               shape = State
             )) +
  geom_line(linewidth = 2) +
  geom_point(size = 4, stroke = 2) +
  xlab("Month") + ylab("Refusals") +
  scale_shape_manual("State", values = c(15, 1, 2, 9, 19)) +
  scale_color_manual("State", values = pal5) +
  theme_bw() + ptheme
## SAVE GRAPH
ggsave(
  p1,
  file = paste0(out1, "refs_month.png"),
  width = 10,
  height = 7
)

##############                  DECLARATION DATA                   #############

## ADD MONTH / YEAR TO THE DECLARATION DATA
dec <- dec %>% mutate(
  MONTH = as.character(substr(
    as.Date(TRIP_START_DATE, format = "%m/%d/%Y"), 6, 7
  )),
  YEAR = as.character(substr(
    as.Date(TRIP_START_DATE, format = "%m/%d/%Y"), 1, 4
  )),
  State = ARRIVAL_PORT_STATE,
  YR_MON = paste0(YEAR, "_", MONTH),
  vsl_num = VESSEL_OFFICIAL_NUMBER
)

## MERGE PERMIT DATA WITH DECLARATION DATA
decs <- dec %>% left_join(perms, by = "vsl_num")

## CHECK PERMIT DATA FOR MIS-MATCHES IN MERGING
chk_perm <- decs %>% filter(is.na(Perm_Group) == T) ## SOME TRIPS JUST MISSING PERMIT CONFIRMATION

## FILTER DATA TO INCLUDE ONLY CHARTER AND HEADBOAT TRIPS
decs <- decs %>% filter(
  TRIP_TYPE %in% c("CHARTER", "HEADBOAT") &
    INTENDED_FISHING_FLAG == "YES" &
    ## FILTER DATA TO INCLUDE ONLY 2022
    (YEAR == "2022")
) %>%
  ## DESIGNATE GULF TRIPS (AL, TX, MS, AND LA TRIPS ARE ALWAYS CODED AS GOM)
  mutate(
    GulfTrip = case_when(
      State %in% c("AL", "TX", "MS", "LA") ~ 1,
      ## FL TRIPS THAT INDICATE GOM STAT ZONES CODED AS GULF
      State == "FL" &
        STAT_ZONE %in% c(
          "003",
          "004",
          "005",
          "006",
          "007",
          "008",
          "009",
          "010",
          "018",
          "019",
          "021"
        ) ~ 1,
      ## FL TRIPS IN KEYS STAT_ZONE WITH GULF PERMITS ARE CODED AS GULF
      State == "FL" &
        STAT_ZONE %in% c("001", "002", "748") & Perm_Group == "GOM" ~ 1,
      TRUE ~ 0
    ),
    ##CREATE DUMMY VARIABLE FOR EACH RECORD, EACH IS A UNIQUE DECLARATION
    dec = 1
  )

## CHECK WHICH TRIPS DON'T FALL INTO THE GULF TRIP UMBRELLA - 539 RECORDS
## THAT ARE IN MONROE WITH NO SPECIFIED GULF PERMIT, OR ONLY SA PERMIT / SA COUNTIES
## E.G. BROWARD, COLLIER, DUVAL, MIAMI-DADE
chk_gom <- decs %>% filter(GulfTrip == 0)
table(chk_gom$STAT_ZONE, chk_gom$Perm_Group)
table(chk_gom$ARRIVAL_PORT_COUNTY, chk_gom$Perm_Group)
table(chk_gom$ARRIVAL_PORT_COUNTY)

## FILTER TRIPS TO ONLY INCLUDE NEWLY DESIGNATED GULF TRIPS
## CHECK PERMIT DATA FOR MIS-MATCHES IN MERGING
chk_perm <- decs %>% filter(is.na(Perm_Group) == T)
table(chk_perm$State)

## FILTER OUT THE SA TRIPS USING THE GULF TRIP FILTER
decs <- decs %>% filter(GulfTrip == 1)

## SUM OF THE DECLARATIONS BY STATE, YEAR AND MONTH
dec_cnts_stym <- decs %>% group_by(State, YEAR, MONTH) %>%
  summarize(tot_decs = sum(dec))

## PLOT OF DECLARATIONS BY MONTH
p1 <- ggplot(data = dec_cnts_stym,
             aes(
               x = MONTH,
               y = tot_decs,
               group = State,
               color = State,
               shape = State
             )) +
  geom_line(linewidth = 2) +
  geom_point(size = 4, stroke = 2) +
  xlab("Month") + ylab("Declarations") +
  scale_shape_manual("State", values = c(15, 1, 2, 9, 19)) +
  scale_color_manual("State", values = pal5) +
  theme_bw() + ptheme
## SAVE GRAPH
ggsave(
  p1,
  file = paste0(out1, "decs_month.png"),
  width = 10,
  height = 7
)

##############                  LOGBOOK DATA                   #############

## REMOVE DUPICATE RECORDS (ELIMINATE DUPLICATIVE CATCH RELATED RECORDS)
logb <- slog %>% distinct(TRIP_ID, .keep_all = TRUE)

## CREATE MONTH AND YEAR FIELDS
logb <- logb %>% mutate(
  MONTH = as.character(substr(
    as.Date(TRIP_START_DATE, format = "%m/%d/%Y"), 6, 7
  )),
  YEAR = as.character(substr(
    as.Date(TRIP_START_DATE, format = "%m/%d/%Y"), 1, 4
  )),
  State = START_PORT_STATE,
  vsl_num = VESSEL_OFFICIAL_NBR
) %>%
  ## FILTER TO ONLY INCLUDE CHARTER / HEADBOAT TRIPS WITH INTENTION TO FISH / FISHING EFFORT
  filter(
    TRIP_TYPE_NAME %in% c("CHARTER", "HEADBOAT") &
      !ACTIVITY_TYPE_NAME == "TRIP NO INTENTION OF FISHING" &
      ## LIMIT RECORDS TO GULF STATES
      START_PORT_STATE %in% c("AL", "FL", "MS", "LA", "TX") &
      ## FILTER DATA TO INCLUDE ONLY 2022
      (YEAR == "2022")
  )

## MERGE PERMIT DATA WITH LOGBOOK DATA
logbs <- logb %>% left_join(perms, by = "vsl_num")

## CHECK PERMIT DATA FOR MIS-MATCHES IN MERGING
chk_perm <- logbs %>% filter(is.na(Perm_Group) == T) ## SOME TRIPS JUST MISSING PERMIT CONFIRMATION

## FILTER DATA TO INCLUDE ONLY CHARTER AND HEADBOAT TRIPS
logbs <- logbs %>% ## DESIGNATE GULF TRIPS (AL, TX, MS, AND LA TRIPS ARE ALWAYS CODED AS GOM)
  mutate(
    GulfTrip = case_when(
      State %in% c("AL", "TX", "MS", "LA") ~ 1,
      ## FL TRIPS WITH A GULF COUNTY AS A START COUNTY ARE CODED AS GULF
      State == "FL" &
        START_PORT_COUNTY %in% c(
          "ESCAMBIA",
          "SANTA ROSA",
          "OKALOOSA",
          "WALTON",
          "BAY",
          "GULF",
          "FRANKLIN",
          "WAKULLA",
          "JEFFERSON",
          "TAYLOR",
          "DIXIE",
          "LEVY",
          "CITRUS",
          "HERNANDO",
          "PASCO",
          "HILLSBOROUGH",
          "PINELLAS",
          "MANATEE",
          "SARASOTA",
          "CHARLOTTE",
          "COLLIER",
          "LEE"
        ) ~ 1,
      ## FL TRIPS IN MONROE COUNTY WITH GOM PERMIT GROUP ARE CODED AS GULF
      State == "FL" &
        START_PORT_COUNTY == "MONROE" & Perm_Group == "GOM" ~ 1,
      ## UNSPECIFIED COUNTIES IN FLORIDA WITH A GOM PERMIT ARE CODED AS GULF
      State == "FL" &
        START_PORT_COUNTY == "NOT-SPECIFIED" & Perm_Group == "GOM" ~ 1,
      TRUE ~ 0
    ),
    ## CREATE A DUMMY VARIABLE FOR EACH LOGBOOK RECORD
    logb = 1
  )

## CHECK WHICH TRIPS DON'T FALL INTO THE GULF TRIP UMBRELLA - REMOVES 13,268
## RECORDS (EAST COAST COUNTIES / SA PERMITS)
chk_gom <- logbs %>% filter(GulfTrip == 0)
table(chk_gom$AREA_CODE, chk_gom$Perm_Group)
table(chk_gom$START_PORT_COUNTY, chk_gom$Perm_Group)

## FILTER OUT THE SA TRIPS USING THE GULF TRIP FILTER
logbs <- logbs %>% filter(GulfTrip == 1)

## SUM OF THE DECLARATIONS BY STATE, YEAR AND MONTH
logb_cnts_stym <- logbs %>% group_by(State, YEAR, MONTH) %>%
  summarize(tot_logs = sum(logb))

## PLOT OF LOGBOOKS BY MONTH
p1 <- ggplot(data = logb_cnts_stym,
             aes(
               x = MONTH,
               y = tot_logs,
               group = State,
               color = State,
               shape = State
             )) +
  geom_line(linewidth = 2) +
  geom_point(size = 4, stroke = 2) +
  xlab("Month") + ylab("Logbooks") +
  scale_shape_manual("State", values = c(15, 1, 2, 9, 19)) +
  scale_color_manual("State", values = pal5) +
  theme_bw() + ptheme
## SAVE GRAPH
ggsave(
  p1,
  file = paste0(out1, "logs_month.png"),
  width = 10,
  height = 7
)

##############        MERGE THE SUMMARY DATASETS TOGETHER          #############
## MERGE THE SUMMARY DATASETS TOGETHER (BY YEAR, MONTH AND STATE)
cnts_stym <- int_cnts_stym %>% left_join(dec_cnts_stym, by = c("YEAR", "MONTH", "State")) %>%
  left_join(logb_cnts_stym, by = c("YEAR", "MONTH", "State"))
## CALCULATE THE REFUSAL RATE, AND INTERCEPTION RATES (USING TOTAL INTERVIEWS
## VS COMPLETED INTERVIEWS - AS THE NUMERATOR AND DECLARATIONS OR LOGBOOKS AS
## THE DENOMINATOR)
cnts_stym <- cnts_stym %>% mutate(
  ref_perc = 100 * (tot_refs / tot_ints),
  com_dec_perc = 100 * (tot_comp / tot_decs),
  com_log_perc = 100 * (tot_comp / tot_logs),
  int_dec_perc = 100 * (tot_ints / tot_decs),
  int_log_perc = 100 * (tot_ints / tot_logs)
)

## CREATE LISTS OF DATA OR LABELS TO USE IN GRAPH LOOPS
ys <- c("ref_perc",
        "com_dec_perc",
        "com_log_perc",
        "int_dec_perc",
        "int_log_perc")
ylabs <- c("Refusals (%)", rep("Intercepted Trips (%)", 4))
titles <- c(
  "Percent of Refusals (Incomplete/All Interviews)",
  "Interception Rate - Completed Interviews/Declarations",
  "Interception Rate - Completed Interviews/Logbooks",
  "Interception Rate - All Interviews/Declarations",
  "Interception Rate - All/Logbooks"
)
## LOOP FOR CREATING FIGURES FOR EACH RATE (REFUSAL AND INTERCEPTION X 4)
## BY MONTH AND STATE
for (i in 1:length(ys)) {
  p <- cnts_stym %>% select (MONTH, ys[i], State) %>% mutate(yval = ys[i]) %>%
    filter(!is.na(State))
  p1 <- ggplot(p,
               aes_string(
                 x = "MONTH",
                 y = ys[i],
                 group = "State",
                 color = "State",
                 shape = "State"
               )) +
    geom_line(linewidth = 2) +
    geom_point(size = 4, stroke = 2) +
    xlab("Month") + ylab(ylabs[i]) +
    ggtitle(titles[i]) +
    scale_shape_manual("State", values = c(15, 1, 2, 9, 19)) +
    scale_color_manual("State", values = pal5) +
    theme_bw() + ptheme
  print(p1)
  ## SAVE GRAPH
  ggsave(
    p1,
    file = paste0(out1, "st_", ys[i], ".png"),
    width = 10,
    height = 7
  )
}

##############       AGGREGATE COUNTS BY YEAR AND MONTH            #############
cnts_ym <- cnts_stym %>% mutate_at(c('tot_decs', 'tot_logs'), ~ replace_na(., 0)) %>%
  group_by(YEAR, MONTH) %>%
  summarize(
    tot_refs = sum(tot_refs),
    tot_ints = sum(tot_ints),
    tot_comp = sum(tot_comp),
    tot_decs = sum(tot_decs),
    tot_logs = sum(tot_logs)
  )
## CALCULATE THE REFUSAL RATE, AND INTERCEPTION RATES (USING TOTAL INTERVIEWS
## VS COMPLETED INTERVIEWS - AS THE NUMERATOR AND DECLARATIONS OR LOGBOOKS AS
## THE DENOMINATOR)
cnts_ym <- cnts_ym %>% mutate(
  ref_perc = 100 * (tot_refs / tot_ints),
  com_dec_perc = 100 * (tot_comp / tot_decs),
  com_log_perc = 100 * (tot_comp / tot_logs),
  int_dec_perc = 100 * (tot_ints / tot_decs),
  int_log_perc = 100 * (tot_ints / tot_logs)
)

## LOOP FOR CREATING FIGURES FOR EACH RATE (REFUSAL AND INTERCEPTION X 4)
## BY MONTH
for (i in 1:length(ys)) {
  p <- cnts_ym %>%  mutate(MON = as.numeric(MONTH))
  p1 <- ggplot(p, aes_string(x = "MON", y = ys[i])) +
    geom_line(linewidth = 2, color = "#0570B0") +
    geom_point(size = 4,
               stroke = 2,
               color = "#0570B0") +
    xlab("Month") + ylab(ylabs[i]) +
    ggtitle(titles[i]) +
    scale_x_continuous(breaks = seq(1, 12, 1)) +
    scale_shape_manual("State", values = c(15, 1, 2, 9, 19)) +
    scale_color_manual("State", values = pal5) +
    theme_bw() + ptheme
  print(p1)
  ## SAVE GRAPH
  ggsave(
    p1,
    file = paste0(out1, "ym_", ys[i], ".png"),
    width = 10,
    height = 7
  )
  
}

## SELECT THE COMPLETED INTERVIEW VALUES BY YEAR AND MONTH
tots_comp <- cnts_ym %>% select(YEAR, MONTH, tot_comp) %>% mutate(tots =
                                                                    tot_comp, cnt_type = "Completed Interviews") %>% select(YEAR, MONTH, tots, cnt_type)
## SELECT THE LOGBOOK VALUES BY YEAR AND MONTH
tots_all <- cnts_ym %>% select(YEAR, MONTH, tot_logs) %>% mutate(tots =
                                                                   tot_logs, cnt_type = "Total Logbooks") %>% select(YEAR, MONTH, tots, cnt_type)
## SELEC THE INTERCEPTION RATE BY YEAR AND MONTH
tots_ir <- cnts_ym %>% select(YEAR, MONTH, com_log_perc)
## BIND THE ROWS AND JOIN IN THE INTERCEPTION RATE VALUES
tot_graph <- bind_rows(tots_comp, tots_all) %>%
  left_join(tots_ir, by = c("YEAR", "MONTH"))

## GRAPH THE HORIZONTAL BARS FOR INTERVIEWS/LOGBOOKS
p1 <- ggplot(tot_graph, aes(x = MONTH, y = tots)) +
  geom_col(
    position = "dodge",
    stat = "identity",
    aes(fill = cnt_type),
    width = 0.8
  ) +
  coord_flip() +
  ylab("Number of Interviews / Logbooks") + xlab("Month") +
  scale_x_discrete(limits = rev) +
  scale_fill_manual("Record Type", values = c("darkslategray1", "#0570B0")) +
  theme_bw() + theme(legend.position = "bottom") + ptheme
## SAVE GRAPH
ggsave(
  p1,
  file = paste0(out1, "int_log_bars.png"),
  width = 10,
  height = 7
)

## GRAPH THE INTERCEPTION RATE OVER TOP OF HORIZONTAL BARS FOR INTERVIEWS/LOGBOOKS
p1 <- ggplot(tot_graph, aes(x = MONTH, y = tots)) +
  geom_col(
    stat = "identity",
    position = "dodge",
    aes(fill = cnt_type),
    width = 0.8
  ) +
  # geom_line(data=tot_graph,aes(x=MONTH,y=com_log_perc*1000,group=cnt_type),col="#8E24AA",lty=5,lwd=1.5)+
  geom_line(
    data = tot_graph,
    aes(x = MONTH, y = com_log_perc * 1000, group = cnt_type),
    col = "#F0AC96",
    lty = 5,
    lwd = 1.5
  ) +
  scale_y_continuous("Number of Interviews / Logbooks",
                     sec.axis = sec_axis( ~ . / 1000, name = "Interception Rate")) +
  coord_flip() +
  ylab("Number of Interviews / Logbooks") + xlab("Month") +
  scale_x_discrete(limits = rev) +
  scale_fill_manual("Record Type", values = c("darkslategray1", "#0570B0")) +
  theme_bw() + theme(legend.position = "bottom") + ptheme
## SAVE GRAPH
ggsave(
  p1,
  file = paste0(out1, "int_log_bars_dual.png"),
  width = 10,
  height = 7
)

## COLORS USED IN SEFHIER PRESENTATION
c(
  "darkslategray1",
  "darkslategray4",
  "skyblue1",
  "#74A9CF",
  "#0570B0",
  "deepskyblue",
  "deepskyblue4"
)

##############              AGGREGATE COUNTS BY STATE              #############
cnts_st <- cnts_stym %>% mutate_at(c('tot_decs', 'tot_logs'), ~ replace_na(., 0)) %>%
  group_by(State) %>%
  summarize(
    tot_refs = sum(tot_refs),
    tot_ints = sum(tot_ints),
    tot_comp = sum(tot_comp),
    tot_decs = sum(tot_decs),
    tot_logs = sum(tot_logs)
  )
## CALCULATE THE REFUSAL RATE, AND INTERCEPTION RATES (USING TOTAL INTERVIEWS
## VS COMPLETED INTERVIEWS - AS THE NUMERATOR AND DECLARATIONS OR LOGBOOKS AS
## THE DENOMINATOR)
## BY STATE
cnts_st <- cnts_st %>% mutate(
  ref_perc = 100 * (tot_refs / tot_ints),
  com_dec_perc = 100 * (tot_comp / tot_decs),
  com_log_perc = 100 * (tot_comp / tot_logs),
  int_dec_perc = 100 * (tot_ints / tot_decs),
  int_log_perc = 100 * (tot_ints / tot_logs)
)

## PLOT OF INTERCEPTION RATE BY STATE - COMPLETED INTERVIES / LOGBOOKS
p1 <- ggplot(data = subset(cnts_st, !is.na(State)),
             aes(
               x = State,
               y = com_log_perc,
               group = State,
               fill = State
             )) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_fill_manual("State", values = pal5) +
  ylab("Intercepted Trips (%)") +
  ggtitle("Interception Rate - Completed Interviews/Logbooks") +
  theme_bw() + ptheme
## SAVE GRAPH
ggsave(
  p1,
  file = paste0(out1, "int_state.png"),
  width = 10,
  height = 7
)

##############            AGGREGATE COUNTS BY YEAR ONLY            #############
cnts_y <- cnts_stym %>%  mutate_at(c('tot_decs', 'tot_logs'), ~ replace_na(., 0)) %>%
  group_by(YEAR) %>%
  summarize(
    tot_refs = sum(tot_refs),
    tot_ints = sum(tot_ints),
    tot_comp = sum(tot_comp),
    tot_decs = sum(tot_decs),
    tot_logs = sum(tot_logs)
  )
## CALCULATE THE REFUSAL RATE, AND INTERCEPTION RATES (USING TOTAL INTERVIEWS
## VS COMPLETED INTERVIEWS - AS THE NUMERATOR AND DECLARATIONS OR LOGBOOKS AS
## THE DENOMINATOR)
## BY YEAR
cnts_y <- cnts_y %>% mutate(
  ref_perc = 100 * (tot_refs / tot_ints),
  com_dec_perc = 100 * (tot_comp / tot_decs),
  com_log_perc = 100 * (tot_comp / tot_logs),
  int_dec_perc = 100 * (tot_ints / tot_decs),
  int_log_perc = 100 * (tot_ints / tot_logs)
)
## CALCULATE REFUSAL PERCENTAGE OVER JUST THE MONTHS WITH DATA
ref_y <- cnts_stym %>% filter(MONTH %in% c("05", "06", "07", "08", "09", "10", "11", "12")) %>%
  group_by(YEAR) %>%
  summarize(tot_refs = sum(tot_refs),
            tot_ints = sum(tot_ints)) %>%
  mutate(ref_perc = 100 * (tot_refs / tot_ints))

## CREATE TABLE WITH REQUESTED VALUES
tab_val <- c(
  cnts_y$tot_refs,
  cnts_y$tot_comp,
  cnts_y$tot_ints,
  cnts_y$ref_perc,
  cnts_y$com_log_perc
)
tab_names <- c(
  "Total Refusals",
  "Completed Interviews",
  "Total Interviews",
  "Overall Refusal Rate",
  "Overall Interception Rate"
)
## FINAL TABLE
tab_overall <- as.data.frame(cbind(tab_names, tab_val))
##############   FINAL NUMBERS NEEDED - CNTs_Y DATAFRAME  #############

## TOTAL INCOMPLETE REFUSALS - 19
## TOTAL INCOMPLETE INTERVIEWS - 165
## TOTAL COMPLETED INTERVIEWS - 1641
## TOTAL INTERVIEWS - 1806
## OVERALL REFUSAL RATE - 1.052049
## OVERALL INTERCEPTION RATE - 4.768685

##############   PIVOT TABLES FOR EACH VARIABLE / RATE CALCULATED  #############


## PIVOT TABLE OF INCOMPLETE INTERVIEWS BY STATE AND MONTH
piv_all3 <- pivot_wider(
  int_cnts_stym,
  names_from = State,
  values_from = c(tot_inc, tot_comp, tot_ints)
)
## PIVOT TABLE OF INCOMPLETE INTERVIEWS BY STATE AND MONTH
piv_inc <- int_cnts_stym %>% select(YEAR, MONTH, State, tot_inc) %>%
  pivot_wider(names_from = State, values_from = tot_inc)
## PIVOT TABLE OF ALL INTERVIEWS BY STATE AND MONTH
piv_all <- int_cnts_stym %>% select(YEAR, MONTH, State, tot_ints) %>%
  pivot_wider(names_from = State, values_from = tot_ints)
## PIVOT TABLE OF COMPLETE INTERVIEWS BY STATE AND MONTH
piv_comp <- int_cnts_stym %>% select(YEAR, MONTH, State, tot_comp) %>%
  pivot_wider(names_from = State, values_from = tot_comp)

## PIVOT TABLE OF DECLARATIONS BY STATE AND MONTH
piv_dec <- dec_cnts_stym %>% pivot_wider(names_from = State, values_from = tot_decs)

## PIVOT TABLE OF  LOGBOOKS BY STATE AND MONTH
piv_log <- logb_cnts_stym %>% pivot_wider(names_from = State, values_from = tot_logs)
