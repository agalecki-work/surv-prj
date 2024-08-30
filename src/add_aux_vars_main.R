# Called by `add_aux_vars.R` script
# Note: df_Info list unpacked

# Prep steps 

message("==>==>==> # Data frame no ", df_no, ": `", dfin_name, "`-> `", dfnew_name,  "` modified -----")

dfin_datax <- eval(as.name(dfin_name)) # working copy
dfdim_txt  <- paste(dim(dfin_datax), collapse =", ")
message("---  Dim (nrows, ncols) in original data: ", dfdim_txt) 

#----- prep steps
tvars_mtx <- tvars_select
message(paste0("---> Surv vars for the event of interest: ", tvars_mtx[1,1], ", ",  tvars_mtx[1,2]))

ntvarsx <- nrow(tvars_mtx)

txt0  <- if (ntvarsx ==1) "Competing risk NOT defined" else paste0("Competing risk: ",  tvars_mtx[2,2])
message(paste0("---> ", txt0))


# --- Checking whether variables are present
 varNms <- colnames(dfin_datax)
 vars_not_found <- if (CCH_data) {
       setdiff(c(as.vector(tvars_mtx), id, subcohort, cch_case),varNms) 
       } else {
          setdiff(c(as.vector(tvars_mtx), id),varNms) 
       }
 tmp_nm <- paste0("vars_not_found",df_no)
 assign(tmp_nm, vars_not_found, envir = .GlobalEnv)
 if (length( vars_not_found) ==0){
    message("---  Vars not found (blank expected):  ... OK")
    } else {
    message("---  Vars not found (blank expected):", eval(as.name(tmp_nm)), " ...???")
    }
 rm(tmp_nm)
 if (length(vars_not_found) == 0) rm(vars_not_found)
 

#---- Step 1:  cfilter applied

message(paste0("---> Step1: Filter is defined as: ", cfilter)) 
if (length(cfilter) > 0){
  message("---  Filter ", cfilter, " applied")
  dfdim <- paste(dim(dfin_datax), collapse =", ")
  message("---  Dim (nrows, ncols _before_ filter): ", dfdim) 
  cfilter_stmnt <- paste0("dfin_datax %>% filter(", cfilter,")")
  assign("dfin_datax", eval(parse(text= cfilter_stmnt)))
  dfdim <- paste(dim(dfin_datax), collapse =",")
  message("---  Dim (nrows, ncols _after_ filter): ", dfdim)
  rm(cfilter_stmnt)
} else {
  dfdim <- paste(dim(dfin_datax), collapse =",")
  message("---  Dim filter is NULL:", dfdim) 
} # if cfilter
rm(dfdim)


# ---- Step 1a: Filter out non-cases outside of subcohort
if (CCH_data){
  cch_cstmnt <- paste0("dfin_datax %>% filter(",  subcohort, "|", cch_case, ")")
  dfin_datax <- eval(parse (text=cch_cstmnt))
  dfdim_txt  <- paste(dim(dfin_datax), collapse =", ")
  message("---> Step1a: Dim (nrows, ncols)", dfdim_txt) 
} else {
 message("---> Step 1a is skipped,because `CCH_data` is FALSE")
}



#---- Step 2: Truncate time for all time variables included in `tvars_all` matrix

message("---> Step 2: Time horizon :=", time_horizon[1], ", tm_cut =", tm_cut) 

# Before truncating time in df
rngs_beforeS1 <-  sapply(tvars_mtx[,1], FUN =  function(tmx){
         dtx_temp <- dfin_datax
         assign("timex", dtx_temp[, tmx])
         range(timex)
 })
max_time <- max(rngs_beforeS1[2.])
message("---  Max time _before_ truncation :=", round(max_time, digits=3)) 

tbls_beforeS1 <-  sapply(tvars_mtx[,2], FUN =  function(evnt){
         dtx_temp <- dfin_datax
         assign("event", dtx_temp[, evnt])
         table(event)
 })
#tbls_beforeS1


for (i in 1:ntvarsx) dfin_datax <- SurvSplit_truncate(dfin_datax, tvars_mtx[i,], tm_cut)

# After truncating time

rngs_S1 <-  sapply(tvars_mtx[,1], FUN =  function(tmx){
         dtx_temp <- dfin_datax
         assign("timex", dtx_temp[, tmx])
         range(timex)
 })
max_time <- max(rngs_S1[2.])
message("---  Max time _after_ truncation :=", round(max_time, digits =3)) 
 
#rngs_S1

tbls_S1 <-  sapply(tvars_mtx[,2], FUN =  function(evnt){
         dtx_temp <- dfin_datax
         assign("event", dtx_temp[, evnt])
         table(event)
 })
#tbls_S1
rm(max_time, rngs_S1, tbls_S1, rngs_beforeS1, tbls_beforeS1) 

if (!CCH_data){
   keep_Allvars <- c(keep_Allvars, as.vector(tvars_mtx))
   } else {
   keep_Allvars <- c(keep_Allvars, subcohort, cch_case, as.vector(tvars_mtx))
   }
# print(keep_Allvars)


#--- Step 2: Create weight variables for data from C-CH study in dfin_name : Self, SelfPrentice, BorganI

message("---> Step 2: Create weight variables for data from C-CH study ---")
if (CCH_data){ # CCH data only
  message("--- CCH_data is ", CCH_data, " => CCH weight variables created")
 # assign(dfin_name, 
 #     create_cch_weights(as.name(dfin_name), as.name(subcohort), as.name(cch_case), total_cohort_size),
 #     envir=.GlobalEnv)
 dfin_datax <-  create_cch_weights(dfin_datax, as.name(subcohort), as.name(cch_case), total_cohort_size)
 keep_Allvars <- c(keep_Allvars,"w_Self","w_SelfPrentice", "w_BorganI") 
} else  message("--- CCH_data is ", CCH_data, " => CCH weight variables _not_ created")
 

# Step 3: Create `initSplit` and `foldid` variables.
message("---> Step 3: Create `initSplit` and `foldid` variables ---")

if (length(initSplit) !=0 && df_no ==1){

if (CCH_data){
  message("--- CCH data `create_cch_folds()` functiom used (not yet). Vars `initSplit`, `foldid` created")
  dfin_datax <- create_cch_folds(dfin_datax, as.name(subcohort), as.name(cch_case), initSplit, nfolds)
 } else {
  message("--- SRS data `create_srs_folds()` functiom used. Vars `initSplit`, `foldid` created")
  # assign(dfin_name, create_srs_folds(dfin_datax, initSplit, nfolds))
 dfin_datax <- create_srs_folds(dfin_datax, initSplit, nfolds)
}
  keep_Allvars <- c(keep_Allvars, "initSplit", "foldid") 

}

#--- Step 4a: Create competing risk variables (cr_*) in `dfin_datax` dataframe

if (ntvarsx > 1){
 
 message("--- STEP4a.`cr` variables for CR analysis created (ntvarsx :=", ntvarsx, ")")

 ncrvarsx <- ntvarsx-1  # Last row is competing risk

 for (i in 1:ncrvarsx) dfin_datax  <- create_cr_vars2(dfin_datax, tvars_mtx[i,], tvars_mtx[ncrvarsx+1 ,] )
 cr_vars0 <- tvars_mtx[ -ntvarsx,] # Last row omitted

 cr_mtx0 <- paste("cr_", cr_vars0, sep="")
 cr_mtx <- matrix(cr_mtx0, ncol =2)
 colnames(cr_mtx) <- c("cr_time", "cr_event")
 nms <- rownames(tvars_mtx)
 rownames(cr_mtx) <- nms[-ntvarsx]
 # cr_mtx

 cr_rngs_S2 <-  sapply(cr_mtx[,1], FUN =  function(tmx){
         dtx_temp <- dfin_datax
         assign("timex", dtx_temp[, tmx])
         range(timex)
 })
 # cr_rngs_S2

 tbls_S2 <-  sapply(cr_mtx[,2], FUN =  function(evnt){
         dtx_temp <- dfin_datax
         assign("event", dtx_temp[, evnt])
         table(event)
 })
 rownames(tbls_S2)[2] <- "event"
 # tbls_S2
 } else {
 message("---> `cr` variables for competing risk analysis _not_ created (because ntvarsx :=", ntvarsx, ")")
 keep_Allvars <- c(keep_Allvars, as.vector(cr_mtx))
 # print(keep_Allvars)

} # if (ntvarsx > 1)

#--- Step 4b: Create FG data using competing risk variables (cr_*) in `dfin_datax` dataframe


if (ntvarsx > 1){
 
 message("--- STEP4b. Create FG data using `cr` variables (ntvarsx :=", ntvarsx, ")")
 csurv <- paste0("Surv(", cr_mtx[1, "cr_time"], ",", cr_mtx[1, "cr_event"], ")") # Surv object created
 cform <- paste0(csurv, "~ .")
 
 argsx <- list(formula =as.formula(cform), data = dfin_datax,
    etype = tvars_mtx[1,2], id =as.name(id))
 FG_df <- do.call("finegray", argsx)
 assign(FG_dfname, FG_df, envir =.GlobalEnv)  
 
} # (ntvarsx > 1)



# ----- FINISH

# print(keep_Allvars)

keep_Allvars <- unique(c(keep_Allvars, common_vars))

if (length(dfnew_name) !=0){
 tmp_df <- dfin_datax %>% select(all_of(keep_Allvars)) 
  assign(dfnew_name, tmp_df, envir =.GlobalEnv)
  #rm(dfin_datax)
  message("----> New df `", dfnew_name, "` created  -----")

}
 





