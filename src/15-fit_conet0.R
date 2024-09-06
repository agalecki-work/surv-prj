#==== Run coxph models _without_ tt() interaction terms for each row of `cterms_mtx` matrix
## message("===> Step Model _without_ tt() interaction terms ")

# multiple coxph using `train_data` or `train_fgdata wout tt vars

 coxnet0_FITs <- lapply(cxterms_plus , FUN = function(cxi_plus) {
      if (ntvarsx == 2 ){     # `coxph()` applied to `train_data`: Standard Cox without tt variables
            csrv      <-     str_glue("Surv({tv_tnms[1]} ,{tv_tnms[2]})")    # 'Surv(time, status)'
            csrvexpr  <-     parse(text=csrv)
            cxform0   <-     paste0("~0 + ", cxi_plus)
            cxform_srv <- paste0(csrv, cxform0) 
            data_name <- "train_data"
            Ymtx <- with(train_data, csrvexpr)
            Xmtx <- model.matrix(as.formula(cxform0), data = train_data)
            
               
         } else {     #  ntvarsx > 2  # coxph applied to `train_fgdata`: Emulates competing risks without tt vars                          
            csrv      <-     paste0("Surv(fgstart, fgstop, fgstatus)")
            csrvexpr  <-     expression(Surv(fgstart, fgstop, fgstatus))
            cxform0   <-     str_glue("~0 +  {cxi_plus}")
            cxform_srv <-    str_glue({csrv} ~0 +  {cxi_plus}) 
            data_name  <-    "train_fgdata"
            Ymtx <- with(train_fgdata, csrvexpr)
	    Xmtx <- model.matrix(as.formula(cxform0), data = train_fgdata)
       }  #  ifelse ntvarsx
          
              coxnet0_args <-  list(
                                  y = substitute(Ymtx),
       	                          x = substitute(Xmtx),
       	                          family="cox",
       	                          alpha = mod_alpha 
                            )
  
        # `coxnet0_args$id` created 
           if (length(mod_id)> 0) coxnet0_args$id <- 
               substitute(data[[mod_id]], list  (mod_id   = mod_id,   data =as.name(data_name)))
           if (length(mod_wght)> 0) coxnt0_args$weights <- 
	       substitute(data[[mod_wght]], list(mod_wght = mod_wght, data =as.name(data_name)))
    #message("i=", i)
    #print(cox0_args)
    coxnet0_fit <-  do.call(glmnet, coxnet0_args)
    modified_call <- coxnet0_fit$call
    modified_call <- modified_call[-1] 
    #  modified_call <- as.call(modified_call) 
    coxnet0_fit$call <- modified_call
    return(coxnet0_fit) 
 })
  names(coxnet0_FITs) <- rownames(mod_cxterms_mtx)
 #  cox0_xfits <- cox0_FITs
  
  # Create list of glance tibbles
  
  list_of_glances <- lapply(1:length(cox0_FITs), 
                       FUN = function(i){
                          fit <-coxnet0_FITs[[i]]
                          broom::glance(fit)
                       })
  cox0_FITs_glance <- plyr::rbind.fill(list_of_glances)
  rownames(cox0_FITs_glance) <- rownames(mod_cxterms_mtx)                       
                            
  #-- Illustration how to extract call
 # modx <- cox0_FITs[[1]]
 # clx_1 <- getCall(modx)[-1]
 # print(clx_1)
  

  