
# source("201accord-update-data_Info.R") # This file

rm(list= ls())

#--- Create mandatory `df_initInfo` list and `work_data_init' dataframe
source("200accord_olink_analytical_dataset040323.R") 
#print(data_initInfo)


#---- Create `data_Info` list by modifying  selected components of `data_initInfo` list ---------

dx <- data_initInfo                  # Copy `data_initInfo` list
#dx$CCH_Info <- NULL                 # CCH info ignored
dx$dfin_Info$cfilter                 <- "FEMALE == 1 "  # Subset of Subcohort
dx$dfin_Info$cfilter_comment         <- "Females only"
dx$dfin_Info$time_horizon <- c(10, 9.99)  # if vector with two elements use second element to define tm_cut (Inf no truncation)
data_Info <- dx

#==== Typically no changes, below

source("./src/process_work_data.R")


keep_objects <- c("work_data", "prj_Info",  "data_Info")

# print(df_Info)

# Cleanup (No changes below)
ls_objects <- ls()
rm_objects  <- c(setdiff(ls_objects, keep_objects), "ls_objects")
rm(list = rm_objects)
rm(rm_objects)
