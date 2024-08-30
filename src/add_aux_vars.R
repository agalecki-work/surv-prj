#======  Update `dfin_name` data frame with auxilary variables
source("./R/zzz_Rfuns.R")    # R functions loaded
if (is.null(df_Info$CCH_data)) df_Info$CCH_data <- FALSE
if (df_Info$CCH_data & !exists("dfCCH_Info"))  dfCCH_Info <- dfCCH_initInfo


Added_vars <- df_Info$id 

#---- Step 0: Check input info

# Make sure no new components created in `df_Info` list compared to `df_initInfo`
  nms0 <- names(df_initInfo)
  nms1 <- names(df_Info)
  added_df_Info_components <- setdiff(nms1, nms0)
  if (length(added_df_Info_components) ==0){
    txt <- "---  df_initInfo vs  df_Info. No added components  ... OK"
    } else {
    txt <- "---  df_initInfo vs  df_Info. Added components  ...  Error"
    }
 message(txt)

 
# Unpack df_Info
nms0 <-  names(df_Info)
for (nm in nms0) assign(nm, df_Info[[nm]], envir = .GlobalEnv)

# if (is.numeric(initSplit)) initSplit_fraction <- initSplit


# unpack dfCCH_Info
if (CCH_data){
  # Make sure no new components created in `dfCCH_Info` list
  nms0 <- names(dfCCH_initInfo)
  nms1 <- names(dfCCH_Info)
  added_dfCCH_Info_components <- setdiff(nms1, nms0)
  for (nm in nms0) assign(nm, dfCCH_Info[[nm]], envir = .GlobalEnv)
  ### tvars <- dfCCH_Info$CCH_tvars
} # if (CCH_data)
 

# Define tm_cut
tm_cut <- if (length(time_horizon) == 2) time_horizon[2] else time_horizon[1]


## Process dfin_name original df

 # dfnew_name <-  if (length(dfin_name) == 2)  dfin_name[2] else character(0)
 # dfin_name  <- dfin_name[1]

 # message("===>>==>>=== # data frame `", dfin_name, "`-> `", dfnew_name,  "` modified -----")
 initSplit_label <- if (initSplit_select ==1) "Training" else "Validation"
 
#--- source("./src/add_aux_vars_main.R")

dfin_datax <- work_data %>% filter(initSplit == initSplit_select) # select rows for training/validation data
## message("=====>  ",  initSplit_label, "(", initSplit_select, ")")
tvars_vec <- tvar_select$tvars

Added_vars <- c(Added_vars, tvars_vec)
tvars_slevels <- tvar_select$slevels
tvars_slabels <- tvar_select$slabels

# keep_dfinvars <- c(id, tvars_vec, keep_xvars) 
## dfin_datax <- dfin_datax %>% select(all_of(unique(keep_dfinvars)))  # Select xvars
dfdim_txt  <- paste(dim(dfin_datax), collapse =", ")
message("====>===>  ", initSplit_label, "(",initSplit_select , "): ", nrow(dfin_datax)," x ", ncol(dfin_datax))
# print(colnames(dfin_datax))

#----- prep steps

# message(paste0("===> Surv vars for the event of interest: ", tvars_vec[1], ", ",  tvars_vec[2]))
message(paste0("--- CCH_data: ", CCH_data))

ntvarsx <- length(tvar_select$slevels) # 2 or 3

txt0  <- if (ntvarsx ==2) "FALSE" else paste0("TRUE")
message(paste0("--- Competing risk: ", txt0))


# --- Checking whether variables are present
 varNms <- colnames(dfin_datax)
 vars_not_found <- if (CCH_data) {
          setdiff(c(tvars_vec, id, subcohort),varNms) 
       } else {
          setdiff(c(tvars_vec, id),varNms) 
       }
 if (length( vars_not_found) ==0){
    message("---  Vars not found (blank expected):  ... OK")
    } else {
    message("---  Vars not found (blank expected):", vars_not_found, "  ...???")
    }
 
#---- Step 1:  cfilter applied

message(paste0("===> Step1: Filter is defined as: ", cfilter)) 
if (length(cfilter) > 0){
  dfdim <- paste(dim(dfin_datax), collapse =", ")
  message("-- Dim (_before_ filter): ", nrow(dfin_datax), " x ", ncol(dfin_datax)) 
  cfilter_stmnt <- paste0("dfin_datax %>% filter(", cfilter,")")
  assign("dfin_datax", eval(parse(text= cfilter_stmnt)))
  dfdim <- paste(dim(dfin_datax), collapse =",")
  message("-- Dim (_after_ filter): ",  nrow(dfin_datax), " x ", ncol(dfin_datax))
  rm(cfilter_stmnt)
} else {
  dfdim <- paste(dim(dfin_datax), collapse =",")
  message("---  Dim filter is NULL:", dfdim) 
} # if cfilter

  which_idx <- which(dfin_datax$initSplit == 1)
  foldidx <- sample(1:nfolds, length(which_idx),replace=TRUE)
  dfin_datax[which_idx, "foldid"] <- foldidx
  message("--- foldid variable created")

#---- Step 2: Truncate time variable included in `tvars_vec` 

message("===> Step 2: Time horizon :=", time_horizon[1], ", tm_cut =", tm_cut)
tvars_adj <- NULL

if (is.finite(time_horizon[1])){
 # Before truncating time in df
 rngs_beforeS1 <-  sapply(tvars_vec[1], FUN =  function(tmx){
         dtx_temp <- dfin_datax
         assign("timex", dtx_temp[, tmx])
         range(timex)
  })
 max_time <- max(rngs_beforeS1[2.])
 message("---  Max time _before_ truncation :=", round(max_time, digits=3)) 

 postfix <- paste0("_th", time_horizon[1])
 
 dfin_datax <- SurvSplit2_truncate(dfin_datax, tvars_vec, tm_horizon = tm_cut, postfix = postfix )
 
 tvars_adj <- paste(tvars_vec, postfix, sep="") #names in tvars_vec adjusted
 
 Added_vars <- c(Added_vars, tvars_adj)

 # After truncating time

 rngs_S1 <-  sapply(tvars_adj[1], FUN =  function(tmx){
         dtx_temp <- dfin_datax
         assign("timex", dtx_temp[, tmx])
         range(timex)
 })
 max_time <- max(rngs_S1[2.])
 message("---  Max time _after_ truncation :=", round(max_time, digits =3))
 message("---!!! Adjusted tvar names:", tvars_adj[1], ",", tvars_adj[2])  
 rm(max_time, rngs_S1, rngs_beforeS1) 
} else{

 message("--- Time_horizon is Infinity (Time truncation NOT done)   ... OK ")  
} # if (is.finite(time_horizon[1])


#--- Step 3: Create FG data for competing risk variables

if (ntvarsx >=2){ # Step 3a: `*_factor` variable created, if needed 
  # Create factor with multiple (typically 0,1,2) levels for the event variable
  is_crFactor <- is.factor(dfin_datax[, tvars_vec[2]])
  if (!is_crFactor) {
   crEventName <- if (is.null(tvars_adj)) tvars_vec[2]  else tvars_adj[2]
   crEventFactorName <- paste0(crEventName, "_factor")
   fac_vec <- pull(dfin_datax[, crEventName])
   crEventFactor <- factor(fac_vec, levels = tvars_slevels, labels= tvars_slabels)
   assign(crEventFactorName, crEventFactor, envir = .GlobalEnv)
   dfin_datax[,crEventFactorName] <- crEventFactor 
   Added_vars <- c(Added_vars, crEventFactorName)
   message("===> Step 3a: Factor `",  crEventFactorName, "` added")
  } else {  # (!is_crFactor)
   message("===> Step 3a: Variable '", tvars_vec[2], "` is already a factor")
  }
} # (ntvarsx >=2)


if (ntvarsx >= 2){ # Step 3b: Create FG data using `finegray` function for the competing risk
 
 tvars_cr <- if (is.null(tvars_adj)) tvars_vec else tvars_adj
 message("===> STEP3b. Create FG data for competing risk (ntvarsx :=", ntvarsx, ")")
 csurv <- paste0("Surv(", tvars_cr[1], ",", crEventFactorName, ")") # Surv object created
 cform <- paste0(csurv, "~ .")
 
 argsx <- list(formula =as.formula(cform), data = dfin_datax,
    etype = tvars_slabels[2], id =as.name(id))
 if (length(wght_var) != 0) argsx$weights <- as.name(wght_var)
 
 dfin_dataxFG <- do.call("finegray", argsx)
 message("--- Dataset: `dfin_dataxFG` created nobs=", nrow(dfin_dataxFG), " x ", ncol(dfin_dataxFG)) 

} # (ntvarsx > 1)

#---- Step 4: Expand/Split FG data into small time intervals

# Define the cut points for time intervals

cut_points <- seq(0, max( dfin_dataxFG$fgstop), by = tt_split_length)  # Define 'some_interval' length appropriately

# Split the FG  data ID???
dfin_datax_FGtt <- survSplit(Surv(fgstart, fgstop, fgstatus) ~ ., 
                        data =  dfin_dataxFG,                     
                        cut = cut_points,
                        episode = "interval")
 message("--- Split FG Dataset: ", " created nobs=", nrow(dfin_datax_FGtt) ) 

# Adjust the time-varying covariate
#split_FGdata$SEX_time <- (split_FGdata$SEX-1)*sqrt(split_FGdata$fgstop)
dfin_datax_FGtt$BM1_time <- (dfin_datax_FGtt$BM1)*sqrt(dfin_datax_FGtt$fgstop)




