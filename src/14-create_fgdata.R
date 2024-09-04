#====== Create `train_fgdata` and `val_fgdata` using finegray function
# Input: `train_data`, `val_data` obtained from `work_data`
# Output: `train_data`, `val_data` (expanded if model with tt vars)
#         `train_fgdata`

##----  Use finegray()
##- train_data -> train_fgdata
##  val_data   -> val_fgdata
  if (ntvarsx  >= 3){ # finegray() followed by tt split
    message("====> Expanded data for competing risks for: ", tv_tnms[1], " finegray() created (No tt terms)" )
    message("--- `train_data` ", nrow(train_data), "x", ncol(train_data)) 

    # Expand data using finegray for event type 1
    train_fgdata <- expand_data_finegray(train_data, tvar_Info = tvar_Info, cwght = mod_wght, cid = mod_id) #
    message("--- `train_fgdata` obtained using finegray() ", nrow(train_fgdata), "x", ncol(train_fgdata))     
      if (!is.null(val_data)){
        message("---  `val_data` ", nrow(val_data), "x", ncol(val_data)) 
        val_fgdata <- expand_data_finegray(val_data, tvar_Info = tvar_Info, cwght = mod_wght, cid = mod_id ) #
        message("--- `val_fgdata` ", nrow(val_fgdata), "x", ncol(val_fgdata)) 
      } # !is.null(val_data))
 
 ## ntvarsx  >= 3 `train_fgdata` and `val_fgdata` will be split (over-written)
    if (mod_tt_data ) {  ## tt split in the data
      cut_points <- seq(0, max(train_fgdata$fgstop), by = mod_tt_split_length)  # Define 'some_interval' appropriately
      train_fgdata <- survSplit(Surv(fgstart, fgstop, fgstatus) ~ ., 
                              data = train_fgdata,                     
                              cut = cut_points,
                              episode = "interval")
      message("--- `train_fgdata` expanded using SurvSplit ", nrow(train_fgdata), "x", ncol(train_fgdata))     
   
        if (!is.null(val_fgdata)){
          cut_points <- seq(0, max(val_fgdata$fgstop), by = mod_tt_split_length)  # Define 'some_interval' appropriately
          val_fgdata <- survSplit(Surv(fgstart, fgstop, fgstatus) ~ ., 
                              data = val_fgdata,                     
                              cut = cut_points,
                              episode = "interval")
           message("--- `val_fgdata` expanded using SurvSplit ", nrow(val_fgdata), "x", ncol(val_fgdata))     
       
        } # !is.null(val_data)) 
    } # mod_tt_data 
 
 } else { 
    message ("====> Datasets `train_fgdata` and ` val_fgdata _NOT_ created: ntvarsx<3  ... OK")
  }  # ifelse ntvarsx >= 3

# (Expanded) Data for models with tt terms
# Input: `train_data`, `val_data` obtained from `work_data`
#         train_fgdata
# Output: Expanded Datasets with the same name

if (mod_tt_data  && ntvarsx == 2) {          # Split train_data for models with tt vars
  message("(Expanded) Data for models with tt terms")
  message("Split data for models with tt vars")  
  csrvx      <- paste0("Surv(", tv_tnms[1], ',', tv_tnms[2],") ~.")    # 'Surv(time, status)'
  esrvx      <- expression(parse( text= csrvx))
  train_data <- survSplit(as.formula(esrvx), 
                            data = train_data,                     
                            cut = cut_points)
}  # mod_tt_data ...   Data with tt vars


