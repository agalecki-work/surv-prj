# Creates `ntvarsx` variable created
#---- Unpack data_Info and 
#  path_Info, tvar_Info dfin_Info CCH_Info split_Info mod_Info 
dtInfo <- data_Info

dtInfo_nms <-  names(dtInfo)
for (nm in dtInfo_nms) assign(nm, dtInfo[[nm]], envir = .GlobalEnv)
# print(dtInfo_nms)

#---- extract auxiliary objects

#--tvar_info
tv_tnms    <- tvar_Info$tnms      # Time and status variables
tv_slevels <- tvar_Info$slevels
tv_slabels <- tvar_Info$slabels
message("-- tvar selected: ", tv_tnms[1], ", status var: ", tv_tnms[2])
ntvarsx  <- length(tv_slevels) # 2 or 3

