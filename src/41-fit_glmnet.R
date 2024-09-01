
source("./src/11-unpack-data-Info.R")
source("./src/12-unpack-mod_Info.R")
source("./src/13-unpack-glmnet_Info.R")
source("./src/14-create_fgdata.R")
source("./src/15-create-coxph0_fit.R")



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

