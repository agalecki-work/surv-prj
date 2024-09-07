# source("220accord-fit-coxph.R") # This file

require(tidymodels)
require(stringr)
require(glue)
require(splines)

rm(list= ls())

#--- Create mandatory `df_initInfo` list and `work_data' dataframe
srcf <- "201accord-update-data_Info.R"
message("====>********* Source ` ", srcf, "`: Starts")
source(srcf) 
print(ls())

message("======>  220accord-fit-coxph.R  Starts" ) # This file


#print(data_Info)
srcf <- "./R/zzz_Rfuns.R"
message("====> Source ` ", srcf, "`: Starts")
source(srcf) 
message("-- Source ` ", srcf ,"`: Ended")



srcf <- "205-create-aux-objects.R"
message("====> Source ` ", srcf, "`: Starts")
source(srcf) 
message("-- Source ` ", srcf ,"`: Ends")


srcf <- "207-create-cxterms.R"
message("====> Source ` ", srcf, "`: Starts")
source(srcf) 
message("-- Source ` ", srcf ,"`: Ends")


message("----> coxph_Info ")
coxph_Info <- list(
  wght           = "CCH_Self",       # ... CCH_Self,  CCH_SelfPrentice, CCH_BorganI
  id             = "MASKID",
  cxterms        = cxterms1,         # Matrix or vector with cxterms
  skip_tt        = FALSE,             # Ignore tt terms, if any
  tt_split_length  = 0.1             # 0.1, 0.01 Length of tt_split_interval used to create tt expanded data
)

#print(coxph_Info)


#==== source
srcf <- "./src/11-unpack-data_Info.R"
message("====> Source ` ", srcf, "`: Starts")
source(srcf) 
message("-- Source ` ", srcf ,"`: Ends")


srcf <- "./src/12-unpack-coxph_Info.R"
message("=====> Source ` ", srcf, "`: Starts")
source(srcf) 
message("-- Source ` ", srcf ,"`: Ends")
#print(mod_tt_split_length)

#print(head(mod_cxterms_mtx))


srcf <- "./src/13-process-cxterms.R"
message("=====> Source ` ", srcf, "`: Starts")
source(srcf) 
message("-- Source ` ", srcf ,"`: Ends")

#stop("xyz")

srcf <- "./src/14-create_fgdata.R"
message("=====> Source ` ", srcf, "`: Starts")
source(srcf) 
message("-- Source ` ", srcf ,"`: Ends")


#==== source
srcf <- "./src/15-fit_coxph0.R"
message("====> Source ` ", srcf, "`: Starts")
source(srcf) 
message("-- Source ` ", srcf ,"`: Ends")
# if (TRUE) stop("xyz")


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
#rm(list = rm_objects)
rm(rm_objects)




