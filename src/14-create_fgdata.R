#====== Create `train_fgdata` and `val_fgdata` using finegray function
if (ntvarsx  >= 3){ # finegray()
  message("====> Expanded data for competing risks for: ", tv_tnms[1], " finegray() created" )
  message("--- `train_data` ", nrow(train_data), "x", ncol(train_data)) 
  # Expand Data Using finegray for event type 1
  train_fgdata <- expand_data_finegray(train_data, tvar_Info = tvar_Info, cwght = mod_wght, cid = mod_id) #
  message("--- `train_fgdata` ", nrow(train_fgdata), "x", ncol(train_fgdata)) 
   if (!is.null(val_data)){
    message("---  `val_data` ", nrow(val_data), "x", ncol(val_data)) 
    val_fgdata <- expand_data_finegray(val_data, tvar_Info = tvar_Info, cwght = mod_wght, cid = mod_id ) #
    message("--- `val_fgdata` ", nrow(val_fgdata), "x", ncol(val_fgdata)) 

  } # !is.null(val_data))
} else { # if ntvarsx >= 3
  message ("====> Datasets `train_fgdata` and ` val_fgdata _NOT_ created ... OK")
}

