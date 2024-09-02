## source("100cric_olinknpx_112023_v1_imputed0.R")

rm(list =ls())


require(dplyr)

# STEP 1: Create data frames
current_folder <-  getwd()
datain_basename <- "cric_olinknpx_112023_v1_imputed0" # Without extension
datain_extension <- "Rdata"

load(paste0(current_folder, "/data/", datain_basename, ".", datain_extension), verbose=TRUE)

cric_imputed <- dtx_imputed0
message("==>===> `cric_imputed` data: ", nrow(cric_imputed), " x ", ncol(cric_imputed)) 


#---- Derive variables ( if any)

#----  Prepare info 

#--- `tvar?` lists (one list will be selected)


tvar1 <- list(
    dv_nick = "1ESKD",
    tnms     = c("TIME_ESRD", "ESRD"),  #  Variables' names used to create Surv objects
    tlabels  =  c("Time to primary outcome or censoring (years)", "Primary outcome (ESKD/Dialysis, 0=NO, 1=YES)"),
    slevels = 0:1,                         # event status variable values
    slabels = c("0-censored", "1-ESKD/Dialysis"))

tvars2 <- c("TIME_LOSS50", "LOSS50")
tvars3 <- c("TIME_LOSS50_ESRD", "LOSS50_ESRD")
tvars4 <- c("TIME_DEATH", "DEAD")          # Competing risk (if any) included in the last row of `tvars_all` matrix

   
#   Select one tvar list !!!
tvar_Info <- tvar1 # 

#---- Mandatory list
prj_Info <- list(
   nick             = "1cric",
   cdir             = current_folder,
   script_name      = "cric_olinknpx_112023_v1_imputed0",          # R script (this file) 
   dfin_base        = datain_basename,                             # External file with input dataset (without extension)
   dfin_ext         = datain_extension,                            
   dfnms_all        = "cric_imputed",                              # Data frame names created by this script
   tvar_Info        = tvar_Info                                    # dv_nick, tnms, tlabels, slevels, slabels
)


# ---- Prepare dfin_Info list

# Create `keep_cxvars` vector (DO NOT include time/status vars)
nx        <- 21
BM_cvars  <- paste("OL",1:nx, sep="")
cvars0    <- c("PID", "recno")
cvars1    <- c("Log2UACR", "EGFR_CKD_EPI", "HEMOGLOBIN_A1C_v5") 
cvars2    <- c("SEX","AGE_INTEGER", "BMI","RACE_CAT_1" )
keep_cxvars <- c(BM_cvars, cvars0, cvars1, cvars2)
dfvars_in   <- unique(c("PID", tvar_Info$tnms, keep_cxvars))

dfin_Info <- list(
   varsin     = dfvars_in,
   cfilter    = character(0),  # string with filter stmnt 
   cfilter_comment  = "All data used",
   time_horizon    = c(10,9.99)    # Second element used as a cut-off 
)

work_data_init <- cric_imputed %>% select(all_of(dfvars_in)) 
message("-- `work_data_init` (selected rows/vars): ", nrow(work_data_init), " x ",  ncol(work_data_init))



#-- split_Info

split_Info <- list(
   seed             = numeric(0),  # seed
   initSplit        = 0.8,         # "X" for external,  0.8 implies 80/20 split
   nfolds           = 10
)

# Mandatory list `Data_initInfo`
data_initInfo <- list(
   dfin_Info        = dfin_Info,
   split_Info       = split_Info              # seed, initSplit, nfolds
)

message("===> Project `", prj_Info$nick, "`.  DV nick = `", tvar_Info$dv_nick, "`")

keep_objects <- c("cric_imputed", "prj_Info", "data_initInfo", "work_data_init") # Objects mandatory to keep

# print(df_initInfo)

# Cleanup (No changes below) 
ls_objects <- ls()
rm_objects  <- c(setdiff(ls_objects, keep_objects), "ls_objects")
rm(list = rm_objects)
rm(rm_objects)
