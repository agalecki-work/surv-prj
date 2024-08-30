
#---- Script requires `work_data`, `data_Info`, `data_initInfo` objects
# Functions `SurvSplit2_truncate`, `create_cch_weights` called

source("./R/zzz_Rfuns.R")    # R functions loaded

#---- Step 0: Make sure no new components created in `df_Info` list compared to `df_initInfo` 
  
  nms0 <- names(data_initInfo)
  nms1 <- names(data_Info)
  added_data_Info_components <- setdiff(nms1, nms0)
  if (length(added_data_Info_components) == 0){
    message("---  data_initInfo vs  data_Info. No added components  ... OK")
    rm(data_initInfo)
    } else {
    message("---  data_initInfo vs data_Info. Added components  ...  Error")
    print(added_data_Info_components)
    }

message("=== Processing `work_data` ==== ")

 
#---- Unpack df_Info and 
#  path_Info, tvar_Info dfin_Info CCH_Info split_Info mod_Info 
dtInfo <- data_Info

dtInfo_nms <-  names(dtInfo)
for (nm in dtInfo_nms) assign(nm, dtInfo[[nm]], envir = .GlobalEnv)
# print(dtInfo_nms)

#---- extract auxiliary objects

dfin_cfilter <- dfin_Info$cfilter
dfin_time_horizon <- dfin_Info$time_horizon
tm_cut <- if (length(dfin_time_horizon) == 2) dfin_time_horizon[2] else dfin_time_horizon[1]

if(exists("CCH_Info")){
  CCH_subcohort <- CCH_Info$subcohort
  CCH_n_total   <- CCH_Info$n_total
}

tv_tnms    <- tvar_Info$tnms   # Time and status variables
tv_slevels <- tvar_Info$slevels
tv_slabels <- tvar_Info$slabels
message("-- tvar selected: ", tv_tnms[1], ", status var: ", tv_tnms[2])
ntvarsx <- length(tv_slevels) # 2 or 3

txt0  <- if (ntvarsx ==2) "Competing risk NOT defined" else paste0("Competing risk: ",  tv_slabels[2])
message(paste0("---> ", txt0))

#  [dfs_] seed initSplit nfolds"
dfs_initSplit <- split_Info$initSplit
dfs_seed      <- split_Info$seed
dfs_nfolds    <- split_Info$nfolds

#---- Step 1:  cfilter applied

message(paste0("===> Step1: Filter is defined as: ", dfin_cfilter)) 
if (length(dfin_cfilter) > 0){
  message("---  Filter ", dfin_cfilter, " applied")
  message("-- `work_data` (_before_ cfilter): ", nrow(work_data), " x ",  ncol(work_data))
  cfilter_stmnt <- paste0("work_data %>% filter(", dfin_cfilter,")")
  assign("work_data", eval(parse(text= cfilter_stmnt)))
  message("-- `work_data` (_after_ cfilter): ", nrow(work_data), " x ",  ncol(work_data))
  rm(cfilter_stmnt)
} else {
  message("---  cfilter is blank `work_data`: ",  nrow(work_data), " x ",  ncol(work_data)) 
} # if cfilter


#--- Step 2: Filter out non-cases outside subcohort

message ("===> Step 2: Filter out non-cases outside subcohort ( if CCH_subcohort exists)")

if(exists("CCH_subcohort")){
  message("--- Subcohort varname: ", CCH_subcohort)
  # print(table(subcohort= work_data[, CCH_subcohort]))
  cstmnt <- paste0("ifelse(", tv_tnms[2],"==1, 1,0)")
  work_data <- within(work_data, CCH_ucase <- eval(parse(text = cstmnt)))
  #print(table(CCH_ucase = work_data$CCH_ucase))
  print(table(CCH_ucase = work_data$CCH_ucase, subcohort = work_data[, CCH_subcohort] ))
  subcht_sym <- rlang::sym(CCH_subcohort)  
  work_data <- work_data  %>% filter(!!subcht_sym == 1 | CCH_ucase == 1) %>% select(-c(CCH_ucase)) 
  message("--- `work_data` (CCH_data non-cases outside subcohort filtered out): ", nrow(work_data), " x ", ncol(work_data))
} else {
  message ("--- skipped (CCH_subcohort does not exist)")
}

#---- Step 3 
# Create `initSplit` auxiliary variable stratified by (SUBCO15) CCH subgroup variable

message("===> Step 3: initSplit_fraction = ", dfs_initSplit) 
if (length(dfs_seed) !=0) set.seed(dfs_seed) else set.seed(12435)
# message("- seed = ", dfs_seed)

# Check, initSplit already included
 idx <- which(colnames(work_data) == "initSplit") 

 if (exists("CCH_subcohort") && length(idx) == 0){
  unx <- paste(sort(unique(work_data[, CCH_subcohort])), collapse="")
  subcht_sym <- rlang::sym(CCH_subcohort) 
  if (unx == "01"){ # stratified
     work_data <- work_data %>% group_by(!!subcht_sym) %>%
       mutate(initSplit = ifelse(runif(n()) <  dfs_initSplit, 1, 0)) %>% ungroup()
       message("-- `initSplit` stratified by ", CCH_subcohort, " created: ", nrow(work_data), " x ",  ncol(work_data))
 } else {     # SRS
     work_data <- work_data %>% 
       mutate(initSplit = ifelse(runif(nrow(work_data)) <  dfs_initSplit, 1, 0)) %>% ungroup()
       message("-- `initSplit` _NOT_ stratified by ", CCH_subcohort, " created: ")
 }
} else message("-- `initSplit` already defined it was NOT created: ", nrow(work_data), " x ",  ncol(work_data))

#---- Step 4 Create `foldid`
   message("===> Step 4: Create `foldid` variable: nfolds = ", dfs_nfolds) 
   idx1 <- which(work_data$initSplit ==1)
   tmp  <- sample(1:dfs_nfolds, length(idx1), replace=TRUE)
   work_data$foldid <- 0
   work_data$foldid[idx1] <- tmp
   message("-- `work_data` (variable `foldid` added): ", nrow(work_data), " x ",  ncol(work_data))

   print(table(work_data$foldid))

#--- Step 5: Truncate time 

message("===> Step 5: Truncate time. Time horizon :=", dfin_time_horizon[1], ", tm_cut =", tm_cut)

max_time <- max(work_data[, tv_tnms[1]])
# tvars_adj <- NULL

if (dfin_time_horizon[1] < max_time){
 message("---  Max time _before_ truncation :=", round(max_time, digits=3)) 
 work_data <- SurvSplit2_truncate(work_data, tv_tnms, tm_horizon = tm_cut, postfix =character(0))
 
 # tvars_adj <- paste(tvars_vec, postfix, sep="") #names in tvars_vec adjusted
 max_time <-max(work_data[, tv_tnms[1]])
 message("---  Max time _after_ truncation :=", round(max_time, digits =3))
 message("--- Original variables: ", tv_tnms[1], ", ", " over-written  ... OK")
} else{

 message("--- Time_horizon >= max_time (Time truncation NOT done)   ... OK ")  
} # if time _horizon < max_time
rm(max_time) 

#--- Step 6: CCH weights

message ("====> Step 6: Vars with CCH weights created ")
message("-- `work_data` (_before_ adding CCH weights): ", nrow(work_data), " x ",  ncol(work_data))
# print(colnames(work_data))
if (exists("CCH_n_total")){ 
 message("--- CCH_n_total ", CCH_n_total)
 work_data <- create_cch_weights(work_data, CCH_subcohort, tv_tnms[2], CCH_n_total)
} else {
 message("--- CCH_n_total not defined. CCH_weights are equal to 1")
 work_data <- create_cch_weights(work_data, CCH_subcohort, tv_tnms[2], 999)
} 
message("-- `work_data` (_after_ adding CCH weights): ", nrow(work_data), " x ",  ncol(work_data))
message("--- Max BorganI weight: ", max(work_data$CCH_BorganI))
message("=====> Six steps of `work_data` processing using `./src/process_work_data.R` completed ")






