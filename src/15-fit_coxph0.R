#==== Run coxph models _without_ tt() interaction terms for each row of `cterms_mtx` matrix
## message("===> Step Model _without_ tt() interaction terms ")

# multiple coxph using `train_data` or `train_fgdata wout tt vars

 
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
  

  