## source("200accord_olink_analytical_dataset040323.R")  # This file

rm(list =ls())
require(survival)
require(dplyr)

# STEP 1: Create data frames (and auxiliary objects)
current_folder <-  getwd()
datain_basename <- "accord_olink_analytical_dataset040323" # Without extension
datain_extension <- "Rdata"

load(paste0(current_folder, "/data/", datain_basename, ".", datain_extension), verbose =TRUE)


message("==>===> `olink_analytical_dataset040323` data: ",
                 nrow(olink_analytical_dataset040323), " x ", ncol(olink_analytical_dataset040323)) 

accord <- olink_analytical_dataset040323 %>% filter(YRS_PRIMARY > 0)

message("==>===> `accord` data: ", nrow(accord), " x ", ncol(accord)) 



#--- Derive variables

#`strtm` variable created

accord <- accord %>%
  mutate(strtm = case_when(
    BPTRIAL == 0 & GLYINT == 0 & FIBRATE == 0 ~ 1,
    BPTRIAL == 0 & GLYINT == 0 & FIBRATE == 1 ~ 2,
    BPTRIAL == 0 & GLYINT == 1 & FIBRATE == 0 ~ 3,
    BPTRIAL == 0 & GLYINT == 1 & FIBRATE == 1 ~ 4,
    BPTRIAL == 1 & GLYINT == 0 & BPINT == 0 ~ 5,
    BPTRIAL == 1 & GLYINT == 0 & BPINT == 1 ~ 6,
    BPTRIAL == 1 & GLYINT == 1 & BPINT == 0 ~ 7,
    BPTRIAL == 1 & GLYINT == 1 & BPINT == 1 ~ 8,
    TRUE ~ NA_integer_  # This handles any other cases that do not match the above conditions
  ))

  
##############################################################################################
# Split by alb status ( skipped)
# -- normo <- filter(accord, UACR_gp1 == 1) #n=961
# -- alb   <- filter(accord, UACR_gp1 == 2) #n=432
##############################################################################################

#----  Prepare info 

#--- `tvar?` lists (one list will be selected
tvar1 <- list(
    dv_nick = "1ESKD",  
    tnms  = c("YRS_PRIMARY", "PRIMARY"),  #  Pair of variables used to create Surv objects for Cox model
    tlabels = c("Time to primary outcome or censoring (years)", "Primary outcome (ESKD/Dialysis, 0=NO, 1=YES)"),
    slevels = 0:1,                         # event status variable values
    slabels = c("0-censored", "1-ESKD/Dialysis"))

tvar2 <- list(
    dv_nick = "2ESKD40",
    tnms  = c("YRS_CASE40_JUN", "CASE40_JUNE"),  #  Pair of variables used to create Surv objects for Cox model
    tlabels = c("Time to secondary outcome (yrs)", "Secondary outcome (ESKD/Dialysis/eGFR)"),
    slevels = 0:1,                         # event status variable values
    slabels = c("0-censored", "1-ESKD/Dialysis/eGFR"))
    
tvar3 <- list(
    dv_nick = "3EGFR40",
    tnms  = c("YRS_DECLINE40_PLUS", "DECLINE40_PLUS"),  #  Pair of variables used to create Surv objects for Cox model
    tlabels = c("Time to 40pct eGFR decline event or cens. (yrs)", "40pct eGFR decline"),
    slevels = 0:1,                         # event status variable values
    slabels = c("0-censored", "1: 40pct eGFR decline"))
    
tvar4 <- list(
    dv_nick = "4Death",
    tnms  = c("FU_TM_ACCORDION", "TM_ACCORDION"),  #  Pair of variables used to create Surv objects for Cox model
    tlabels = c("Time to death from any cause (years)", "Death from any cause"),
    slevels = 0:1,                         # event status variable values
    slabels = c("0-censored", "1-Death from any cause"))

tvar5 <- list(
    dv_nick = "5ESKD_cr",  
    tnms  = c("YRS_PRIMARY", "STATUS_PRI"),  #  Pair of variables used to create Surv objects for competing risk model
    tlabels = c("Time to primary outcome (ESKD/Dialysis (years)", "Status for primary outcome"),
    slevels = 0:2,                         # event status variable values
    slabels = c("0-censored", "1-Primary event", "2-Death before primary outcome"))

tvar6 <- list(
    dv_nick = "2ESKD40_cr",
    tnms  = c("YRS_CASE40_JUN", "STATUS_SEC"),  #  Pair of variables used to create Surv objects for competing risk model
    tlabels = c("Time to secondary outcome (yrs)", "Status for secondary outcome"),
    slevels = 0:2,                         # event status variable values
    slabels = c("0-censored", "1-Secondary event", "2-Death before secondary outcome"))
   
#   Select one tvar list !!!
tvar_Info <- tvar5 # 


#---- Mandatory list
prj_Info <- list(
   nick             = "2accord",                                       # Project nickname
   cdir             = current_folder,
   script_name      = "200accord_olink_analytical_dataset040323",  # R script (this file) 
   dfin_base        = datain_basename,                             # External file with dataset (without extension)
   dfin_ext         = datain_extension,                            
   dfnms_all        = "accord",                                   # Data frame names created by this script
   tvar_Info        = tvar_Info                                   # dv_nick, tnms, tlabels, slevels, slabels
)




# Prepare `keep_cvars` vector (include id, weight, filter vars, CCH_vars if any. DO NOT include time variables)
nx        <- 21
BM_cvars  <- paste("BM",1:nx, sep="")
BMQ_cvars <- paste("BMQ",1:nx, sep="")
CCN_cvars <- paste("CCN", 1:7, sep="")
cvars0    <- c("MASKID", "BPTRIAL", "GLYINT", "BPINT", "strtm", "SUBCO15")
cvars1    <- c("BASE_UACR", "BASE_GFR", "HBA1C") 
cvars2    <- c("FEMALE","AGE")
keep_cxvars <- c(BM_cvars, cvars0, cvars1, cvars2) # Variables to keep (DO NOT include time variables, variables) 
dfvars_in   <- unique(c("MASKID", tvar_Info$tnms, keep_cxvars))


# `work_data  
dfin_Info <- list(
  varsin   = dfvars_in,
  cfilter  = character(0),                        # Filter expression (by default all observations included) 
  cfilter_comment  = "All data used",
  time_horizon     = Inf         # Inf -> no time truncation , Second element (if present) will be used as `tm_cut`
)


work_data_init <- accord %>% select(all_of(dfvars_in)) %>% filter(MASKID != 106172) 
message("-- `work_data_init` (selected rows/vars): ", nrow(work_data_init), " x ",  ncol(work_data_init))

#-- `CCH_info` list for CCH data
CCH_Info <- list(
   subcohort    = "SUBCO15",      # Subcohort variable name (string) for data from CCH studies
   n_total      = 8807
)

# print(CCH_Info)

#-- split_Info

split_Info <- list(
   seed             = numeric(0),  # seed
   initSplit        = 0.8,         # "X" for external,  0.8 implies 80/20 split
   nfolds           = 10
)



# Mandatory list `Data_initInfo`
data_initInfo <- list(
   dfin_Info        = dfin_Info,              # name, varsin, cfilter, cfilter_comment, time_horizon
   CCH_Info         = CCH_Info,               # subcohort, weight, n_total
   split_Info       = split_Info              # seed, initSplit, nfolds
 
)

## CCH_info ( \2023_Joslin_AD_HS\_admin\2023-02-atg-notes\project_update4_2AD.pptx)
# colnames(accord)  # Data dictionary Jan112022.xlsx
#  outside_SUBCO <- accord %>% filter(SUBCO15 ==0)
#  inside_SUBCO <- accord %>% filter(SUBCO15 ==1)
# with(inside_SUBCO, table(PRIMARY, TM_ACCORDION, useNA = "always"))
#>       TM_ACCORDION
#> PRIMARY   0   1 <NA>
#>   0    914 195    0   1109 non-cases(including 195 deaths) inside subcohort
#>   1     35   6    0   41 cases

# with(outside_SUBCO, table(PRIMARY, TM_ACCORDION, useNA = "always"))
#>        TM_ACCORDION
#> PRIMARY   0   1 <NA>
#>   0    528 116    0 Depending on CCH_tvars exclude n=528 or 528 + 151 subjects
#>   1    151  64    0 n=215 cases outside of subcohort

message("===> Project `", prj_Info$nick, "`.  DV nick = `", tvar_Info$dv_nick, "`")

keep_objects <- c("accord", "prj_Info", "data_initInfo", "work_data_init") # Objects mandatory to keep

# print(df_initInfo)

# Cleanup (No changes below) 
ls_objects <- ls()
rm_objects  <- c(setdiff(ls_objects, keep_objects), "ls_objects")
rm(list = rm_objects)
rm(rm_objects)
