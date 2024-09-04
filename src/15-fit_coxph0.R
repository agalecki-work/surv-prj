#==== Run coxph models _without_ tt() interaction terms for each row of `cterms_mtx` matrix
## message("===> Step Model _without_ tt() interaction terms ")

# multiple coxph using `train_data` or `train_fgdata wout tt vars

 cox0_FITs <- lapply(1:nrow(mod_cxterms_mtx), FUN = function(i) {
    cxterms0       <- mod_cxterms_mtx[i,] # `cxterms`
    cxterms        <- cxterms0[which(cxterms0 != "")]    # Skip blank strings
    cxterms_plus   <- paste(cxterms, collapse ="+" )
    cxform         <- paste("~ 0 +", cxterms_plus)       # '~0 +x1+x2'
      if (ntvarsx == 2 ){     # `coxph()` applied to `train_data`: Standard Cox without tt variables
            csrv      <-     paste0("Surv(", tv_tnms[1], ',', tv_tnms[2],")")    # 'Surv(time, status)'
            csrvexpr  <-     parse(text=csrv)
            cxform_srv <- paste0(csrv, "~0 + ", cxterms_plus)     
            cox0_args <- list(data   = substitute(train_data), 
	                     formula = as.formula(cxform_srv)
                     )
            
         } else {     #  ntvarsx > 2  # coxph applied to `train_fgdata`: Emulates competing risks without tt vars                          
            csrv      <-     paste0("Surv(fgstart, fgstop, fgstatus)")  # 'Surv(fgstart, fgstop, fgstatus)'
            csrvexpr  <-     expression(Surv(fgstart, fgstop, fgstatus))
            cxform_srv <-    paste0(csrv, "~0 + ", cxterms_plus)     
            cox0_args  <-    list(data   = substitute(train_fgdata), 
                               formula = as.formula(cxform_srv)
                        ) 
       }  #  ifelse ntvarsx
   
       
       
       
        # `cox0_args$id` created 
           if (length(mod_id)> 0){
             # https://stackoverflow.com/questions/26638746/how-can-i-do-partial-substitution-in-r
              e <- quote(get(mod_id))
              e1 <- do.call("substitute", list(e, list(mod_id=mod_id)))
             cox0_args$id    <- substitute(e1)  
           } # if mod_id 
       
       # `cox0_args$weights` created 
           if (length(mod_wght)> 0){
             # https://stackoverflow.com/questions/26638746/how-can-i-do-partial-substitution-in-r
              e <- quote(get(mod_wght))
              e1 <- do.call("substitute", list(e, list(mod_wght=mod_wght)))
              cox0_args$weights    <- substitute(e1)  
           } # if mod_wght 

    cox0_fit <-  do.call(coxph, cox0_args)
    modified_call <- cox0_fit$call
    modified_call <- modified_call[-1] 
    #  modified_call <- as.call(modified_call) 
    cox0_fit$call <- modified_call
    return(cox0_fit) 
 })
  names(cox0_FITs) <- rownames(mod_cxterms_mtx)
 #  cox0_xfits <- cox0_FITs
  
  # Create list of glance tibbles
  
  list_of_glances <- lapply(1:length(cox0_FITs), 
                       FUN = function(i){
                          fit <-cox0_FITs[[i]]
                          broom::glance(fit)
                       })
  cox0_FITs_glance <- plyr::rbind.fill(list_of_glances)
  rownames(cox0_FITs_glance) <- rownames(mod_cxterms_mtx)                       
                            
  #-- Illustration how to extract call
 # modx <- cox0_FITs[[1]]
 # clx_1 <- getCall(modx)[-1]
 # print(clx_1)
  

  