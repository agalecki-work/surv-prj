# source("250accord-fit-coxnet.R") # This file


require(tidymodels)
require(stringr)
require(glue)
require(splines)
require(glmnet)

rm(list= ls())

#--- Create mandatory `df_initInfo` list and `work_data' dataframe
srcf <- "201accord-update-data_Info.R"
message("====>********* Source ` ", srcf, "`: Starts")
source(srcf) 
print(ls())


#print(data_Info)
srcf <- "./R/zzz_Rfuns.R"
message("====> Source ` ", srcf, "`: Starts")
source(srcf) 
message("-- Source ` ", srcf ,"`: Ended")

message("======>  250accord-fit-coxph.R  Starts" ) # This file


srcf <- "205-create-aux-objects.R"
message("====> Source ` ", srcf, "`: Starts")
source(srcf) 
message("-- Source ` ", srcf ,"`: Ends")


srcf <- "207-create-cxterms.R"
message("====> Source ` ", srcf, "`: Starts")
source(srcf) 
message("-- Source ` ", srcf ,"`: Ends")

cxterms <- cxterms2  # Vector

message("----> coxnet_Info ")
coxnet_Info <- list(
  wght           = "CCH_Self",       # ... CCH_Self,  CCH_SelfPrentice, CCH_BorganI
  id             = "MASKID",
  cxterms        = cxterms,          # vector with cxterms
  skip_tt        = TRUE,             # Time split into small intervals
  pen_xterms     = rep(1, times=length(cxterms)),
  # alphas          = c(0.5, 1),     # Stored in attr("alphas" "alpha_pos"
  tt_split_length  = 0.1             # 0.1, 0.01 Length of tt_split_interval used to create tt expanded data
)


#==== source
srcf <- "./src/11-unpack-data_Info.R"
message("-- Source ` ", srcf, "`: Starts")
source(srcf) 
message("-- Source ` ", srcf ,"`: Ended")


srcf <- "./src/12-unpack-coxnet_Info.R"
message("-- Source ` ", srcf, "`: Starts")
source(srcf) 
message("-- Source ` ", srcf ,"`: Ended")


srcf <- "./src/13-process-cxterms.R"
message("=====> Source ` ", srcf, "`: Starts")
source(srcf) 
message("-- Source ` ", srcf ,"`: Ends")

srcf <- "./src/14-create_fgdata.R"
message("-- Source ` ", srcf, "`: Starts")
source(srcf) 
message("-- Source ` ", srcf ,"`: Ended")


#==== source
srcf <- "./src/15-fit_coxnet0.R"
message("-- Source ` ", srcf, "`: Starts")
source(srcf) 
message("-- Source ` ", srcf ,"`: Ended")


#Select one fit
#cox0_fit  <- cox0_xfits[[1]]
#zph_test <- cox.zph(cox0_fit)

#cox0_zphs <- lapply(cox0_xfits, FUN= function(fitx) cox.zph(fitx))
#names(cox0_zphs) <-  cpatt_nms
#print(cox0_zphs)

#==== source


# source("./src/fit_model_tt.R") 


keep0_objects <- if (ntvarsx  == 2) {
   descript <- paste0("Cox for time to event: ",  tv_tnms[1], ',', tv_tnms[2])
   c("train_data", "val_data")
  } else {
   descript <- paste0("Competing risks for: ", tv_tnms[1], ',', tv_tnms[2])
   c("train_fgdata", "val_fgdata")
  }
  
    descript <- paste0(descript,  ". ", length(cox0_FITs), " models stored in `cox0_FITs` fitted using coxph()")  
    c(keep0_objects, "cox0_args", "cox0_FITs")

#print(keep0_objects)

keep_objects <- c(keep0_objects ,  "data_Info","prj_Info", "coxph_Info","cox0_FITs", "cox0_FITs_glance")


Rdata_nm <- paste0("./out/", project_name, "/", DV_name, "-coxph.Rdata") 
save(list = keep_objects, file=Rdata_nm)

# print(df_initInfo)

# Cleanup (No changes below) 
ls_objects <- ls()
rm_objects  <- c(setdiff(ls_objects, keep_objects), "ls_objects")
# rm(list = rm_objects)
rm(rm_objects)




