################################################################################
#####      COMBINE SEFHIER DATASETS INTO MASTER A, I1, AND REF FILES        ####
################################################################################

##  LOAD PACKAGES YOU WILL NEED: 
pkgs<-c("dplyr", "readr", "readxl", "pivottabler", "ggplot2", "haven","writexl", 
        "sas7bdat", "data.table", "openxlsx","scales","RColorBrewer","tidyr") 
lapply(pkgs, require, character.only = TRUE) 

#########                CREATE FILE DIRECTORY LISTS                  ##########

## PREFIX FOR FILE LOCATION OF SEFHIER FILES
  pre<-"C:\\Users\\Dominique.lazarre\\Documents\\IPTs\\Gulf\\SEFHIER Investigations\\SEFHIER DATA\\"
## CREATE SEQUENCE OF YEARS ASSOCIATED WITH FILES IMPORTED
  yr_seq<-c(rep("_2022_",8),rep("_2023_",2))
## CREATE SEQUENCE OF WAVES ASSOCIATED WITH FILES IMPORTED
  wv_seq<-c(5:12,1:2)
## CREATE A REPEATING LIST ASSOCIATED WITH THE NUMBER OF FILES BEING IMPORTED
## FOR EACH FILE TYPE (A, I1, REF)
  a_seq<-c(rep("aga",10)); i1_seq<-c(rep("i1",10)); i2_seq<-c(rep("i2",10))
  i3_seq<-c(rep("i3",10)); ref_seq<-c(rep("ref",10)); 
  ## ADDITIONAL REF FILES
  ref2_seq<-c("ref_2021_10.sas7bdat","ref_2021_11.sas7bdat","ref_2022_04.sas7bdat")
## CREATE A REPEATING LIST OF THE FILETYPE ASSOCIATED WTIH EACH SAS FILE
  f_seq<-c(rep(".sas7bdat"))
## CONCATENATE LIST OF DIRECTORY LOCATIONS / FILE NAMES FOR EACH FILE TYPE (A, I1, REF)
a_inps<-c(paste0(pre,"original\\","aga_20215",".sas7bdat"),paste0(pre,"original\\",a_seq,yr_seq,wv_seq,f_seq))
i1_inps<-c(paste0(pre,"original\\","i1_20215",".sas7bdat"),paste0(pre,"original\\",i1_seq,yr_seq,wv_seq,f_seq))
i2_inps<-c(paste0(pre,"original\\","i2_20215",".sas7bdat"),paste0(pre,"original\\",i2_seq,yr_seq,wv_seq,f_seq))
i3_inps<-c(paste0(pre,"original\\","i3_20215",".sas7bdat"),paste0(pre,"original\\",i3_seq,yr_seq,wv_seq,f_seq))
ref_inps<-c(paste0(pre,"original\\",ref2_seq), paste0(pre,"original\\",ref_seq,yr_seq,wv_seq,f_seq))

#########                USE  LOOP TO LOAD ALL AGA FILES               ##########
for(val in seq_along(a_inps)) {
  obj_name <- paste0("a", val)     # create name of object to assign file to
  df_file <- read_sas(a_inps[val]) # Import file
  assign(obj_name, df_file)        # Assign it to a separate object in global environment
  rm(df_file) }                    # Ensure no df_file remains in environment when loop ends
## BIND A FILE ROWS TOGETHER
  aga<- a1 %>% bind_rows(a2,a3,a4,a5,a6,a7,a8,a9,a10,a11)
## OUTPUT AGA FILES TO A CSV FILE
  write.csv(aga,paste0(pre,"aga_10_21to02_23.csv"),row.names=FALSE)
  
#########                USE  LOOP TO LOAD ALL I1 FILES               ##########
  for(val in seq_along(i1_inps)) {
    obj_name <- paste0("i1_", val)     # create name of object to assign file to
    df_file <- read_sas(i1_inps[val]) # Import file
    assign(obj_name, df_file)        # Assign it to a separate object in global environment
    rm(df_file) }                    # Ensure no df_file remains in environment when loop ends

## BIND A FILE ROWS TOGETHER
  i1<- i1_1 %>% bind_rows(i1_2,i1_3,i1_4,i1_5,i1_6,i1_7,i1_8,i1_9,i1_10,i1_11)
## OUTPUT I1 FILES TO A CSV FILE
  write.csv(i1,paste0(pre,"i1_10_21to02_23.csv"),row.names=FALSE)
  
#########                USE  LOOP TO LOAD ALL I2 FILES               ##########
  for(val in seq_along(i2_inps)) {
    obj_name <- paste0("i2_", val)     # create name of object to assign file to
    df_file <- read_sas(i2_inps[val]) # Import file
    assign(obj_name, df_file)        # Assign it to a separate object in global environment
    rm(df_file) }                     # Ensure no df_file remains in environment when loop ends
## BIND A FILE ROWS TOGETHER
  i2<- i2_1 %>% bind_rows(i2_2,i2_3,i2_4,i2_5,i2_6,i2_7,i2_8,i2_9,i2_10,i2_11)
## OUTPUT I1 FILES TO A CSV FILE
  write.csv(i2,paste0(pre,"i2_10_21to02_23.csv"),row.names=FALSE)
  
#########                USE  LOOP TO LOAD ALL I3 FILES               ##########
  for(val in seq_along(i3_inps)) {
    obj_name <- paste0("i3_", val)     # create name of object to assign file to
    df_file <- read_sas(i3_inps[val]) # Import file
    assign(obj_name, df_file)        # Assign it to a separate object in global environment
    rm(df_file) }                     # Ensure no df_file remains in environment when loop ends
## BIND A FILE ROWS TOGETHER
  i3<- i3_1 %>% bind_rows(i3_2,i3_3,i3_4,i3_5,i3_6,i3_7,i3_8,i3_9,i3_10,i3_11)
## OUTPUT I1 FILES TO A CSV FILE
  write.csv(i3,paste0(pre,"i3_10_21to02_23.csv"),row.names=FALSE)  
  
#########                USE  LOOP TO LOAD ALL REF FILES               ##########
  for(val in seq_along(ref_inps)) {
    obj_name <- paste0("ref", val)     # create name of object to assign file to
    df_file <- read_sas(ref_inps[val]) # Import file
    assign(obj_name, df_file)        # Assign it to a separate object in global environment
    rm(df_file) }                     # Ensure no df_file remains in environment when loop ends
## BIND A FILE ROWS TOGETHER
  ref<- ref1 %>% bind_rows(ref2,ref3,ref4,ref5,ref6,ref7,ref8,ref9,ref10)
## OUTPUT I1 FILES TO A CSV FILE
  write.csv(ref,paste0(pre,"ref_10_21to02_23.csv"),row.names=FALSE)    
