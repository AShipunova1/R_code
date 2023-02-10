# download the compliance details and filter out those South Atlantic vessels that have never reported, and then check that short list against # of calls/emails (need at least 2, and if never spoken too/responded then they'd need a certified letter from OLE)

# ----set up----
add_path_egr <- "egr_violators"
add_path_corresp <- "Correspondence"
add_path_compl <- "FHIER Compliance"

main <- function() {
  source("~/GitHub/R_code/start_module.r")
  source("~/GitHub/R_code/useful_functions_module.r")
  my_paths <- set_work_dir()
  csv_names_list = list(file.path(add_path_corresp,  "Correspondence21_23.csv"), 
                        file.path(add_path_egr, "egr2022.csv"),
                        file.path(add_path, "egr2023.csv"))
  csv_contents_egr <- load_csv_names(my_paths, csv_names_list)
  csvs_clean1 <- clean_all_csvs(csv_contents_egr)
# glimpse(csvs_clean1)
  corresp_arr <- csvs_clean1[[1]]
  compl_arr <- list(csvs_clean1[[2]], csvs_clean1[[3]])
  
  all_data_df <- join_all_csvs(corresp_arr, compl_arr)
  all_data_df_cleen <- change_classes(all_data_df)
  return(list(my_paths, all_data_df_cleen))

  # csvs_clean0 <- lapply(csv_contents_egr, clean_headers)

  # all_data_df <- join_all_csvs(csv_contents_egr)
  # all_data_df_cleen <- change_classes(all_data_df)
}

temp_var <- main()
my_paths <- temp_var[[1]]
all_data_df_cleen <- temp_var[[2]]

glimpse(all_data_df_cleen)

## ----count contacts----
all_data_df_cleen %>%
  mutate(was_contacted = if_else(is.na(contactdate), "no", "yes")) %>% 
  group_by(vesselofficialnumber, was_contacted) %>%
  summarise(total_count=n(), .groups = 'drop') %>%
  as.data.frame() ->
  all_data_df_cleen_contact_cnts

glimpse(all_data_df_cleen_contact_cnts)

## ----get 2_plus contacts----
all_data_df_cleen_contact_cnts %>%
  filter(total_count > 1) ->
  all_data_df_cleen_contact_cnts_2_plus
  
glimpse(all_data_df_cleen_contact_cnts_2_plus)

## ----add counts back to the main df----
egr_w_cnts <- merge(all_data_df_cleen, all_data_df_cleen_contact_cnts_2_plus, all=T)

glimpse(egr_w_cnts)

## ----look at the data 1----
egr_w_cnts %>%
  select(vesselofficialnumber, calltype, contacttype, total_count) %>%
  group_by(vesselofficialnumber) %>% glimpse()
  
## ----look at the data 2----
egr_w_cnts %>% 
  filter(total_count > 1) %>%
  ggplot(aes(x = total_count,
             y = reorder(vesselofficialnumber, 
                         as.integer(factor(total_count)), FUN = min), 
             # colour = contacttype, 
             size = 1)) + 
  labs(title = "Total contacts for no report",
       x ="Total contacts", y = "Vessel official number") +
  theme(axis.text.x = element_text(angle = 90),
        axis.text.y = element_blank()) +
  geom_point()


## ----look at the data 3----
june_bad_list <- c("536567", "GA6691SF", "1069870", "1217685", "1220969", "592507", "NJ7100HM", "1285077", "NY8714GL", "VA9236AV", "VA3738ZA")

egr_w_cnts %>% 
  filter(vesselofficialnumber %in% june_bad_list) %>%
  group_by(vesselofficialnumber) %>% glimpse()

egr_w_cnts %>% 
  filter(vesselofficialnumber %in% june_bad_list) %>%
  select(vesselofficialnumber) %>% unique()

## ----vesslenumbers for egr only----
egr_w_cnts %>% 
  filter(!is.na(total_count)) %>%

    select(vesselofficialnumber) %>%
  unique() -> egr_vsl_names
  
glimpse(egr_vsl_names)

## ----Just to see what's there----
egr_w_cnts %>% 
  select(total_count) %>%
  # filter(total_count == 1) # 0
  # filter(is.na(total_count)) # 454
  filter(!is.na(total_count)) %>%
  unique() %>% 
  glimpse()

# "C:\Users\anna.shipunova\Documents\R_files_local\my_inputs\SEFHIER vessels--non-reporting after contact - SA no reports.csv"

## ----upload data from the egr spreadsheet----
csv_names_list = list(file.path(add_path_egr, "sa_egr_given.csv"))
given_egr_csv_arr <- load_csv_names(my_paths, csv_names_list)
given_egr_csv <- clean_headers(given_egr_csv_arr[[1]])

glimpse(given_egr_csv)
colnames(given_egr_csv)

## ----Date format; use names as variables----
date_col_names <- c("x1stcontact", "x2ndcontact")
test_name <- date_col_names[[1]]
test_name

glimpse(given_egr_csv[test_name])

given_egr_csv %>%
  mutate_at(date_col_names, as.POSIXct, format = "%m/%d/%Y") ->
  given_egr_csv_clean

glimpse(given_egr_csv)
glimpse(given_egr_csv_clean)

str(unique(given_egr_csv_clean[test_name]))

## ----prepare given ids for comparison----
glimpse(given_egr_csv_clean$vesselid)

given_egr_csv_vessels <- given_egr_csv_clean %>% 
  select(vesselid) %>% unique() %>% glimpse()

names(given_egr_csv_vessels) <- "vesselofficialnumber"

glimpse(names(given_egr_csv_vessels))

# remove wrong row
filter(given_egr_csv_vessels, vesselofficialnumber == "Vessel ID") %>% glimpse()

given_egr_csv_vessels %<>%
  filter(vesselofficialnumber != "Vessel ID")

glimpse(given_egr_csv_vessels)
#331

## ----compare vessel names----

## ----Venn diagram----
library(VennDiagram)
library(RColorBrewer)

myCol <- brewer.pal(8, "Pastel2")[c(1, 2)]

venn_plot_cat0 <- list(egr_vsl_names$vesselofficialnumber, given_egr_csv_vessels$vesselofficialnumber)

# to a file
venn.diagram(venn_plot_cat0
             , category.names = c("Script result", "From the manual spreadsheet")
             , filename = file.path(my_paths$outputs, "venn2.png")
             , output=T
             , fill=myCol)

venn_plot_cat0 <- list(egr_vsl_names$vesselofficialnumber, given_egr_csv_vessels$vesselofficialnumber)

venn_plot_cat1 <- sapply(venn_plot_cat0, length)
venn_plot_cat_w_intersect <- list(venn_plot_cat1[[1]],
                      venn_plot_cat1[[2]],
                      length(intersect(x[[1]], x[[2]])))

grid.newpage();

venn.plot <- draw.pairwise.venn(venn_plot_cat_w_intersect[[1]],
                                venn_plot_cat_w_intersect[[2]],
                                venn_plot_cat_w_intersect[[3]],
                                c("Script result", "G. spreadsheet"),
                                fill = c("#AFEEEE", "#FFE4B5"),
                                ext.pos = (180),
                                cat.pos = c(-45, 15)
                                , cat.dist = rep(0.035, 2)
                                # , cat.dist = rep(0.025, 2)
                                );
grid.newpage();
grid.draw(venn.plot);

## ----only in mine:----
glimpse(egr_vsl_names)

only_in_mine <- setdiff(egr_vsl_names, given_egr_csv_vessels)
str(only_in_mine)
#2353

## ----only in given:----
only_in_given <- setdiff(given_egr_csv_vessels, egr_vsl_names)
str(only_in_given)
# 49

## ----in both----
in_both <- intersect(egr_vsl_names, given_egr_csv_vessels)
str(in_both)
#282

# why there is 1 empty row?

## ----export to csv----

write.csv(only_in_given, file.path(my_paths$outputs, "only_in_given.csv"), row.names=FALSE)

given_egr_csv_clean %>%
  filter(vesselid %in% only_in_given$vesselofficialnumber) %>%
  write.csv(file.path(my_paths$outputs, "only_in_given_info.csv"), row.names=FALSE)

## ----striken ids----
csv_names_list = list(file.path(add_path_egr, "given_striken.csv"))
given_egr_striken_arr <- load_csv_names(my_paths, csv_names_list)
given_egr_striken <- clean_headers(given_egr_striken_arr[[1]])
names(given_egr_striken) <- "vesselofficialnumber"

glimpse(given_egr_striken)

## ----compare----

## ----is in only_in_given, not striked out in google sh
setdiff(only_in_given, given_egr_striken) %>% glimpse()
#40
filter(only_in_given, vesselofficialnumber == "1131617")

## ----striked out in google sh, but is not in only_in_given
setdiff(given_egr_striken, only_in_given) %>% glimpse()
#42 $ vesselofficialnumber <chr> "1243727", "1267873", "1283753", "1288532~

filter(only_in_given, vesselofficialnumber == "1243727")

intersect(given_egr_striken, only_in_given) %>% glimpse()
#9

## ----get all_compl 22-23---
csv_names_list_compl = list(file.path(add_path_corresp, "Correspondence2022.csv"), 
                            file.path(add_path_corresp, "Correspondence2023.csv"),
                            file.path(add_path_compl, "FHIER Compliance 2022.csv"),
                            file.path(add_path_compl, "FHIER Compliance 2023.csv"))
csv_compl_contents <- load_csv_names(my_paths, csv_names_list_compl)
csvs_clean1 <- clean_all_csvs(csv_compl_contents)
corresp_arr1 <- list(csvs_clean1[[1]], csvs_clean1[[2]])
compl_arr1 <- list(csvs_clean1[[3]], csvs_clean1[[4]])

compl_data_df <- join_all_csvs(corresp_arr, compl_arr)
compl_data_df_cleen <- change_classes(compl_data_df)
glimpse(compl_data_df_cleen)

## ----look up only_in_given info----
str(only_in_given)
compl_data_df_cleen %>%
  filter(vesselofficialnumber %in% only_in_given$vesselofficialnumber) %>% 
  #glimpse() %>%
#1,239
  select(vesselofficialnumber) %>%
  unique() %>% str()
# 26 ids

## ----look up only_in_mine info----
compl_data_df_cleen %>%
  filter(vesselofficialnumber %in% only_in_mine$vesselofficialnumber) %>% 
#  glimpse()
# 264,525
  select(vesselofficialnumber) %>%
  unique() %>% str()
# 2335

##----info for only_in_given----
compl_data_df_cleen %>%
  filter(vesselofficialnumber %in% only_in_given$vesselofficialnumber) %>%
#  arrange(vesselofficialnumber, year, as.factor(week)) ->
  arrange(vesselofficialnumber, as.Date.factor(week_start)) ->
  all_info_for_only_in_given

# View(all_info_for_only_in_given)

names_short_list <- c("vesselofficialnumber", "name", "year", "xgompermitteddeclarations", "xcaptainreports", "xnegativereports", "xcomplianceerrors", "compliant", "setpermitsonhold", "overridden", "overridedate", "contactdate", "calltype", "voicemail", "contacttype", "contactreason", "contactrecipientname", "contactphonenumber", "contactemailaddress", "contactcomments", "srfhuser", "createdon", "followupnbr", "srhsvessel", "week_start")

all_info_for_only_in_given %>%
  select(all_of(names_short_list)) %>% 
  filter(compliant == "YES") %>%
  group_by(vesselofficialnumber) %>%
  summarise(any(xcaptainreports)) %>% str()
# tibble [26 x 2] (S3: tbl_df/tbl/data.frame)
# $ vesselofficialnumber: chr [1:26] "1069364" "1184980" "1244719" "1258086" ...
# $ any(xcaptainreports): logi [1:26] TRUE TRUE TRUE TRUE TRUE TRUE
# looks like they are not egregious any more

str(only_in_given)
# 49

## ----check each one----
all_info_for_only_in_given %>%
  arrange(vesselofficialnumber) %>% 
  #View()
  filter(vesselofficialnumber == "1131617")

View(all_info_for_only_in_given)

all_info_for_only_in_given %>%
  select(vesselofficialnumber) %>% unique()

##----not in 2022-23----
setdiff(only_in_given$vesselofficialnumber, all_info_for_only_in_given$vesselofficialnumber)

##----check only in given for 2022-23----
# 1)
all_info_for_only_in_given %>%
  filter((vesselofficialnumber %in% only_in_given$vesselofficialnumber) &
           year == "2022")%>%
  group_by(vesselofficialnumber) %>%
#  summarise(is_compliant = list(compliant)) 
  summarise(
    #any_report = any(xcaptainreports == 1),
            any_compliant = any(compliant == "YES")) %>% 
  # Rows: 26
  # Columns: 2
# $ vesselofficialnumber <chr> "1069364", "1184980", "1244719", "1258086", "295322", "6…
# $ any_compliant        <lgl> TRUE, TRUE, TRUE
  
  filter(!any_compliant) %>% # all have at least one "YES"
  str()

#2)
all_info_for_only_in_given %>%
  filter(vesselofficialnumber %in% only_in_given$vesselofficialnumber) %>%
  count(vesselofficialnumber, xcaptainreports, compliant) %>%
head()

## ----check if compliant with no reports----
all_data_df_cleen %>% 
  group_by(vesselofficialnumber) %>%
  reframe(
    no_reports = (xcaptainreports == 0 & xnegativereports == 0),
    all_compliant = all(compliant == "YES")) %>% 
  filter(no_reports & all_compliant) %>% 
  str()
 # tibble [470 × 4] (S3: tbl_df/tbl/data.frame)
# $ vesselofficialnumber: chr [1:470] "1020057" "1021879" "1021879" "1030778" ...
# $ no_reports          : logi [1:470] TRUE TRUE ...

all_data_df_cleen %>% 
  filter(xcaptainreports == 0 & xnegativereports == 0 & compliant == "YES") %>%
    group_by(vesselofficialnumber) %>%
  summarise(no_err = any(xcomplianceerrors == 0)) %>%
  # filter(no_err) %>% # 0
  # select(vesselofficialnumber, xcomplianceerrors) %>%
  # unique() %>%
  str()
# tibble [1,389 × 2] (S3: tbl_df/tbl/data.frame)
# $ vesselofficialnumber: chr [1:1389] "1000164" "1020057" "1020822" "1021417" ...
# $ no_err              : logi [1:1389] FALSE FALSE ...

# group_by(vesselofficialnumber, xcomplianceerrors) %>%
# gropd_df [1,389 × 3] (S3: grouped_df/tbl_df/tbl/data.frame)
# $ vesselofficialnumber: chr [1:1389] "1000164" "1020057" "1020822" "1021417" ...
# $ xcomplianceerrors   : int [1:1389] 1 1 1 1 1 1 1 1 1 1 ...
# $ no_err              : logi [1:1389] FALSE

## ----get spreadsheet fields from the script results----
# TODO
# select(vesselofficialnumber, name, contactphonenumber, contact_date_1, contact_date_2, how_many_missing_since)

## ----look at contact types----
dim(egr_w_cnts)
# [1] 167058     38
egr_w_cnts %>%
  filter(calltype == "Outgoing") %>%
  select(vesselofficialnumber, contacttype, voicemail, contactreason) %>%
  group_by(vesselofficialnumber) %>%
  summarise(total_count=n(), .groups = 'drop') %>%
  as.data.frame() %>%
  # select(vesselofficialnumber, calltype, contacttype, total_count) %>%
#  group_by(vesselofficialnumber) %>% 
  str()

egr_w_cnts %>%
  filter(calltype == "Outgoing") %>%
  select(contacttype) %>%
  unique()
# contacttype
# 1         Call
# 72       Other

## ----all egr ids for outgoing, not all voicemails, no reports, 2+ contacts
egr_w_cnts %>% 
  filter(calltype == "Outgoing") %>%
  group_by(vesselofficialnumber) %>%
  reframe(
    no_reports = (xcaptainreports == 0 & xnegativereports == 0),
    all_vm = all(voicemail == "YES")) %>% 
  filter(no_reports & !all_vm) %>% 
  select(vesselofficialnumber) %>%
  unique() ->
  no_reports_not_all_vm_ids

dim(no_reports_not_all_vm_ids)
# 1652
# filter(calltype == "Outgoing") %>%
# 1632

## ----not all voicemail Venn diagram----
venn_plot_cat0 <- list(no_reports_not_all_vm_ids$vesselofficialnumber, given_egr_csv_vessels$vesselofficialnumber)

venn_plot_cat1 <- sapply(venn_plot_cat0, length)
venn_plot_cat_w_intersect <- list(venn_plot_cat1[[1]],
                                  venn_plot_cat1[[2]],
                                  length(intersect(x[[1]], x[[2]])))

grid.newpage();

venn.plot <- draw.pairwise.venn(venn_plot_cat_w_intersect[[1]],
                                venn_plot_cat_w_intersect[[2]],
                                venn_plot_cat_w_intersect[[3]],
                                c("Script result", "G. spreadsheet"),
                                fill = c("#AFEEEE", "#FFE4B5"),
                                ext.pos = (180),
                                cat.pos = c(-45, 15)
                                , cat.dist = rep(0.035, 2)
                                # , cat.dist = rep(0.025, 2)
);
grid.newpage();
grid.draw(venn.plot);

## ----add other info----
egr_w_cnts %>% 
  # dim()
  # 144792     38
    filter((vesselofficialnumber %in% no_reports_not_all_vm_ids$vesselofficialnumber) & 
             contactreason == "Compliance") %>%
  select(vesselofficialnumber, contactdate, followup, loggroup, calltype, voicemail, contacttype, contactreason, contactrecipientname, contactphonenumber, contactemailaddress, contactcomments, srfhuser, createdon, followupnbr) ->
  egr__not_all_vm__outgoing__no_reports__about_compl

  # select(contactreason) %>% unique()
# 1                    Compliance
# 38             Validation Error
# 42        General communication
# 18316 VMS Related Communication

  # select(loggroup) %>% unique()
# 1                                             
# 377           Pending followup for Trevor Hope
# 721       Pending followup for Natalie Slayden
# 1059   Pending followup for Leeanne Delrosario
# 2016      Pending followup for Shannon Stotler
# 2159         Pending followup for Dylan Miller
# 2185                    Pending followup for  
# 23055    Pending followup for Kendall Brancart
# 47296       Pending followup for Michelle Masi
# 62306       Pending followup for Alicia Breton
# 68099      Pending followup for Omar Rodriguez
# 131938     Pending followup for Victoria Tozer

  # str()
# 'data.frame':	144792 obs. of  14 variables:
  
## ----minimize output----
  egr__not_all_vm__outgoing__no_reports__about_compl %>%
  select(vesselofficialnumber, contactdate, voicemail, contacttype, contactrecipientname, contactphonenumber, contactemailaddress, contactcomments) %>% 
    arrange(vesselofficialnumber, contactdate) %>%
    group_by(vesselofficialnumber, voicemail) %>%
    summarise(Freq=n())
  # # A tibble: 2,407 × 3
  # # Groups:   vesselofficialnumber [1,533]
  # vesselofficialnumber voicemail  Freq
  # <chr>                <chr>     <int>
  #   1 1000164              No           37
  # 2 1020057              No            2
  # 3 1020822              No          100
  # 4 1020822              Yes          25
  # 5 1021417              N/A          57
  # 6 1021417              No           57
  # 7 1021417              Yes         114
  
count_by_diff_columns <- function(my_df, columns_list) {
  my_df %>%
    arrange(vesselofficialnumber, contactdate) %>%
    group_by_at(vars(columns_list))
    summarise(Freq=n())
}

my_df <- egr__not_all_vm__outgoing__no_reports__about_compl
col_list = list("vesselofficialnumber", "voicemail")