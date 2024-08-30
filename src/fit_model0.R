
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

#--mod_Info
mod_mtx_spec <- mod_Info$mtx_spec
mod_id   <- mod_Info$id
mod_wght <- mod_Info$wght
mod_cxterms_mtx   <- mod_Info$cxterms_mtx        # Matrix spec, multiple models
mod_cxterms_mtx_tt<- mod_Info$cxterms_mtx_tt  # select columns in `cxterms_mtx`. Possibly NULL
mod_cxterms       <- unique(mod_Info$cxterms) # Vector spec, one model 
mod_cxterms_tt    <- mod_Info$cxterms_tt      # Select element(s) 

# Vector with tt cxterms 
cxterms_tt <- if (mod_mtx_spec) unique(mod_cxterms_mtx[, mod_cxterms_mtx_tt]) else mod_cxterms[mod_cxterms_tt]

#-- glmnet_Info
gnet_alpha <- glmnet_Info$alpha
gnet_penalty.factor <- glmnet_Info$penalty.factor
  
if (mod_mtx_spec){  # Create `cpatt_nms`
  matches <- grep("^cpatt",  colnames(mod_cxterms_mtx), value = TRUE)
  tmp_mtx <- mod_cxterms_mtx[ , matches ]
  cpatt_nms <-  if (is.null(dim(tmp_mtx))) tmp_mtx else apply(tmp_mtx, 1, paste, collapse=":") 
} 

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


#==== Run coxph models _without_ tt() interaction terms for each row of `cterms_mtx` matrix
message("===> Step Model _without_ tt() interaction terms: mtx_spec=", mod_mtx_spec )

if (mod_mtx_spec){ # multiple coxph using `train_data` or `train_fgdata wout tt vars
 cox0_FITs <- lapply(1:nrow(mod_cxterms_mtx), FUN = function(i) {
    cxterms        <- mod_cxterms_mtx[i,] # `cxterms` for  model wout tt vars
    cxterms_plus   <- paste(cxterms, collapse ="+" )
    cxform         <- paste("~ 0 +", cxterms_plus)       # '~0 +x1+x2'
      if (ntvarsx == 2){
           
            csrv      <-     paste0("Surv(", tv_tnms[1], ',', tv_tnms[2],")")    # 'Surv(time, status)'
            train_df  <-     train_data
            csrvexpr  <-     parse(text=csrv)
            wght      <-     if (length(mod_wght)> 0) train_data[[mod_wght]] else NULL
            idx       <-     if (length(mod_id)> 0) train_data[[mod_id]] else NULL
         } else {                                
            csrv      <-     paste0("Surv(fgstart, fgstop, fgstatus)")  # 'Surv(fgstart, fgstop, fgstatus)'
            train_df  <-     train_fgdata
            csrvexpr  <-     expression(Surv(fgstart, fgstop, fgstatus))
            wght      <-     if (length(mod_wght)> 0) train_fgdata[[mod_wght]] else NULL
            idx       <-     if (length(mod_id)> 0)   train_fgdata[[mod_id]] else NULL
       }  # csrv defined  
     cxform_srv <- paste0(csrv, "~0 + ", cxterms_plus)     
     cox0_args <- list(data = train_df, 
                      formula = as.formula(cxform_srv),
                      weights = wght,
                      id      =idx
                      ) 
    cox0_fit <-  do.call(coxph, cox0_args) 
    return(cox0_fit) 
 })
  names(cox0_FITs) <- cpatt_nms
  cox0_xfits <- cox0_FITs  
 } # mod_mtx_spec = TRUE
  
 #-- Run glmnet model
 
if (!mod_mtx_spec) {            # glmnet -- using `train_data` or  `train_fgdata` _WITHOUT_ ttvars

      cxterms_plus   <- paste(mod_cxterms, collapse ="+" ) # 'x1+x2'
      cxform         <- paste("~ 0 +", cxterms_plus)       # '~0 +x1+x2'
       if (ntvarsx ==2){                         
                csrv      <-     paste0("Surv(", tv_tnms[1], ',', tv_tnms[2],")")    # 'Surv(time, status)'
                train_df  <-     train_data
                csrvexpr  <- parse(text=csrv)
             } else {                                
                csrv      <-     paste0("Surv(fgstart, fgstop, fgstatus)")  # 'Surv(fgstart, fgstop, fgstatus)'
                train_df  <-     train_fgdata
                csrvexpr  <-     expression(Surv(fgstart, fgstop, fgstatus))
             }  # csrv defined 
                   
      cform_srv      <- paste0(csrv, "~ 0 +", cxterms_plus) # 'Surv(time, status) ~ 0 +x1+x2'
  
      X_train        <- model.matrix(as.formula(cxform), data = train_df)      
      Y_train        <- with(train_df, eval(csrvexpr))
        
      # Specify args for glmnet call      
      coxnet0_args <- list(x = X_train,
                           y = Y_train,          
                           family = "cox",
                           alpha = gnet_alpha   # From glmnet_Info 
                        )
     
      if (length(mod_wght)> 0 && ntvarsx ==2) coxnet0_args$weights <- train_data[[mod_wght]]
      if (ntvarsx == 3)        coxnet0_args$weights  <- train_fgdata$fgweight 
      if (length(mod_id)> 0)   coxnet0_args$id <- train_data[[mod_id]] 
      if (length(gnet_penalty.factor) > 0) coxnet0_args$penalty.factor = gnet_penalty.factor
      coxnet0_fit <-  do.call(glmnet, coxnet0_args)
      # call_list <- list(glmnet, coxnet0_args)
      # callx <- as.call(c(glmnet, coxnet0_args))
      # names(callx)
      # callx$alpha
      # call("glmnet", coxnet0_args)
      ## cat(colnames(coxnet0_args$x)) # Display without escape characters
  } # if mod_mtx_spec = FALSE

