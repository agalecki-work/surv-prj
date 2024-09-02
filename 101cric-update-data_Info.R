# source("101cric-update-data_Info.R")

rm(list=ls())
require(survival)

#--- Create mandatory `df_initInfo` list and `work_data_init' dataframe
source("100cric_olinknpx_112023_v1_imputed0.R") # Data frame `cric_imputed` created


#---- Create `data_Info` list by modifying  selected components of `data_initInfo` list ---------
dx       <- data_initInfo
dx$dfin_Info$time_horizon <- c(10, 9.99)  # 10 years, Note: Inf no truncation
dx$dfin_Info$cfilter <- "SEX==1"       # Filter (optional)

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
