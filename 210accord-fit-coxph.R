# source("210accord-fit-coxph.R") # This file


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


#print(data_Info)
srcf <- "./R/zzz_Rfuns.R"
message("====> Source ` ", srcf, "`: Starts")
source(srcf) 
message("-- Source ` ", srcf ,"`: Ended")

message("======>  210accord-fit-coxph.R  Starts" ) # This file



# Auxiliary objects created for later use

train_data <- work_data %>% filter(initSplit == 1)
message("======> `train_data` : ", nrow(train_data), "x" , ncol(train_data))

val_data <- work_data %>% filter(initSplit == 0)
message("======> `val_data` : ", nrow(val_data), "x" ,ncol(val_data))


BM_vars <- paste("BM", 1:21, sep = "")
xvars    <- c("AGE")
nsdf3_vars <- c(BM_vars, xvars) # Splines withh df=3 will be generated for these variables

ns_df3 <- sapply(nsdf3_vars, FUN= function(cx){
  splinex <- ns(train_data[, cx],df=3) 
  attr(splinex, "knots")
})  # matrix with spline knots for selected vars,  df=3. Colnames correspond to var names

#---- Create `cxterms_mtx`
#

nx <- 21   # Number of coxph models in `cxterms_mtx`

nb <- 21   # Number of biomarkers

model_nr <- tibble(nb = 1:nb)

common_cxterms <- c(AGE      = "AGE", 
                   AGE_ns3  = "ns(AGE , knots = ns_df3[,'AGE'])",
                   BASE_UCR = "BASE_UACR"
                   )
common_cxterms_mtx <- matrix(rep(common_cxterms, times =nx), nrow=nx, byrow = TRUE)              
colnames(common_cxterms_mtx) <- names(common_cxterms)
rownames(common_cxterms_mtx) <- paste("M", 1:nx, sep="")
common_cxterms_mtx[1, "AGE"] <- ""
print(head(common_cxterms_mtx))  
# 

# Vector with sequence of cterms:   ----ns(BM#   , knots = ns_df3[,'BM#'])
seq_BMns3 <- model_nr %>% str_glue_data("ns(BM{nb}, knots = ns_df3[,'BM{nb}'])")
  
cxterms_mtx <- cbind(common_cxterms_mtx, seq_BMns3) 



# cxterms_mtx2 <- rep(c("BM1:{tt}"), 21)


message("====> coxph_Info ")
coxph_Info <- list(
  wght           = "CCH_Self",       # ... CCH_Self,  CCH_SelfPrentice, CCH_BorganI
  id             = "MASKID",
  cxterms_mtx    = cxterms_mtx,
  tt_data        = FALSE,         # Time 
  tt_split_length  = 0.1          # 0.1, 0.01 Length of tt_split_interval used to create tt expanded data
)

#==== source
srcf <- "./src/11-unpack-data_Info.R"
message("====> Source ` ", srcf, "`: Starts")
source(srcf) 
message("-- Source ` ", srcf ,"`: Ended")


srcf <- "./src/12-unpack-coxph_Info.R"
message("=====> Source ` ", srcf, "`: Starts")
source(srcf) 
message("-- Source ` ", srcf ,"`: Ended")
print(mod_tt_split_length)

print(head(mod_cxterms_mtx))


srcf <- "./src/14-create_fgdata.R"
message("=====> Source ` ", srcf, "`: Starts")
source(srcf) 
message("-- Source ` ", srcf ,"`: Ended")


#==== source
srcf <- "./src/15-fit_coxph0.R"
message("====> Source ` ", srcf, "`: Starts")
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
rm(list = rm_objects)
rm(rm_objects)




