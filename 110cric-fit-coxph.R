# source("110cric-fit-coxph.R") # This file
library(tidymodels)

rm(list= ls())

#--- Create mandatory `df_initInfo` list and `work_data' dataframe
source("101cric-update-data_Info.R") 
#print(data_Info)
source("./R/zzz_Rfuns.R")    # R functions loaded
message("====> ?10*.R  STARTS")

require(splines)


# Auxiliary objects created for later use

train_data <- work_data %>% filter(initSplit == 1)
message("======> `train_data` : ", nrow(train_data), "x" , ncol(train_data))

val_data <- work_data %>% filter(initSplit == 0)
message("======> `val_data` : ", nrow(val_data), "x" ,ncol(val_data))


BM_vars <- paste("OL", 1:21, sep = "")
xvars    <- c("AGE_INTEGER")
nsdf3_vars <- c(BM_vars, xvars) # Splines withh df=3 will be generated for these variables

ns_df3 <- sapply(nsdf3_vars, FUN= function(cx){
  splinex <- ns(train_data[, cx], df=3) 
  attr(splinex, "knots")
})  # matrix with spline knots for selected vars,  df=3. Colnames correspond to var names


#---- Create `cxterms_mtx`
cxterms_pattern1 <- paste("OL", 1:21, sep = "")
#                           ns(BM1         , knots = ns_df3[,'BM1'])
cxterms_pattern2 <- paste( "ns(OL", 1:21, ", knots = ns_df3[,'OL", 1:21, "'])", sep ="" )
cxterms_pattern <- cbind(cxterms_pattern1, cxterms_pattern2) 

cxterms_common  <- c("AGE_INTEGER") # , "ns(AGE_INTEGER         , knots = ns_df3[,'AGE_INTEGER'])", "Log2_UACR")
cxterms_mtx <- create_cxterms_mtx(cxterms_pattern1, cxterms_common) 
print(head(cxterms_mtx))


message("====> coxph_Info ")
coxph_Info <- list(
  id             = "PID",
  cxterms_mtx    = cxterms_mtx,
  cxterms_mtx_tt = c(2), # select columns in `cxterms_mtx`. Possibly NULL
  tt_split_length  = 0.1          # 0.1, 0.01 Length of tt_split_interval used to create expanded data
)
rm(cxterms_mtx, cxterms_pattern, cxterms_common) 

#==== source
srcf <- "./src/11-unpack-data_Info.R"
message("-- Source ` ", srcf, "`: Starts")
source(srcf) 
message("-- Source ` ", srcf ,"`: Ended")


srcf <- "./src/12-unpack-coxph_Info.R"
message("-- Source ` ", srcf, "`: Starts")
source(srcf) 
message("-- Source ` ", srcf ,"`: Ended")


#==== source
srcf <- "./src/15-create_coxph0_fit.R"
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

keep_objects <- c(keep0_objects , "descript",  "data_Info", "coxph_Info", "cox0_FITs", "cox0_FITs_glance")


Rdata_nm <- paste0("./out/", project_name, "/", DV_name, "-coxph.Rdata") 
save(list = keep_objects, file=Rdata_nm)


# print(df_initInfo)

# Cleanup (No changes below) 
ls_objects <- ls()
rm_objects  <- c(setdiff(ls_objects, keep_objects), "ls_objects")
rm(list = rm_objects)
rm(rm_objects)




