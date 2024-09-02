# Creates `ntvarsx` variable created
#---- Unpack data_Info and 
#  path_Info, tvar_Info dfin_Info CCH_Info split_Info mod_Info 
dtInfo <- data_Info

dtInfo_nms <-  names(dtInfo)
for (nm in dtInfo_nms) assign(nm, dtInfo[[nm]], envir = .GlobalEnv)
# print(dtInfo_nms)

#---- extract auxiliary objects

tvar_Info <- prj_Info$tvar_Info
project_name <- prj_Info$nick
DV_name <- tvar_Info$dv_nick
dfin_cfilter <- dfin_Info$cfilter
dfin_time_horizon <- dfin_Info$time_horizon
tm_cut <- if (length(dfin_time_horizon) == 2) dfin_time_horizon[2] else dfin_time_horizon[1]

if(exists("CCH_Info")){
  CCH_subcohort <- CCH_Info$subcohort
  CCH_n_total   <- CCH_Info$n_total
}

#--tvar_info
tv_tnms    <- tvar_Info$tnms      # Time and status variables
tv_slevels <- tvar_Info$slevels
tv_slabels <- tvar_Info$slabels
message("-- tvar selected: ", tv_tnms[1], ", status var: ", tv_tnms[2])
ntvarsx  <- length(tv_slevels) # 2 or 3
txt0  <- if (ntvarsx ==2) "Competing risk NOT defined" else paste0("Competing risk: ",  tv_slabels[2])
message(paste0("---> ", txt0))

#  [dfs_] seed initSplit nfolds"
dfs_initSplit <- split_Info$initSplit
dfs_seed      <- split_Info$seed
dfs_nfolds    <- split_Info$nfolds

