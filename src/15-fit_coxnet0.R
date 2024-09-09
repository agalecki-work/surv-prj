#==== Run coxph models _without_ tt() interaction terms for each row of `cterms_mtx` matrix
## message("===> Step Model _without_ tt() interaction terms ")

# multiple coxph using `train_data` or `train_fgdata wout tt vars


coxnet0_cvaFITs <- lapply(mod_alphas , FUN = function(a) {
  
      if (ntvarsx == 2 ){     # `coxph()` applied to `train_data`: Standard Cox without tt variables
            csrv      <-     str_glue("Surv({tv_tnms[1]} ,{tv_tnms[2]})")    # 'Surv(time, status)'
            csrv      <-     as.character(csrv)
            csrvexpr  <-     parse(text=csrv)
            cxform0   <-     paste0("~0 + ", cxterms_plus)  # cxi_plus)
            cxform_srv <- paste0(csrv, cxform0) 
            data_name <- "train_data"
            Ysrv <- substitute(with(train_data, eval(csrvexpr)))
            Xmtx <- substitute(model.matrix(as.formula(cxform0), data = train_data))
            
               
         } else {     #  ntvarsx > 2  # coxph applied to `train_fgdata`: Emulates competing risks without tt vars                          
            csrv      <-     paste0("Surv(fgstart, fgstop, fgstatus)")
            csrvexpr  <-     expression(Surv(fgstart, fgstop, fgstatus))
            cxform0   <-     str_glue("~0 +  {cxterms_plus}") #  cxi_plus
            cxform_srv <-    str_glue({csrv} ~0 +  {cxterms_plus})  # cxi_plus
            data_name  <-    "train_fgdata"
            Ysrv       <- substitute(with(train_data, eval(csrvexpr)))
  	    Xmtx      <-  substitute(model.matrix(as.formula(cxform0), data = train_fgdata))
       }  #  ifelse ntvarsx
          
              coxnet0_args <-  list(
                   	          x = substitute(Xmtx),
                                  y = substitute(Ysrv),
       	                          family="cox",
       	                          alpha = a 
                            )
  
        # `coxnet0_args$id` created 
           if (length(mod_id)> 0) coxnet0_args$id <- 
               substitute(data[[mod_id]], list  (mod_id   = mod_id,   data =as.name(data_name)))
           if (length(mod_wght)> 0) coxnet0_args$weights <- 
	       substitute(data[[mod_wght]], list(mod_wght = mod_wght, data =as.name(data_name)))
    #message("i=", i)
    #print(cox0_args)
    coxnet0_fit <-  do.call(cv.glmnet, coxnet0_args) # do.call cv.glmnet
    return(coxnet0_fit) 
 }) #  coxnet0_aFITs
 names(coxnet0_cvaFITs) <- paste(DV_name, "_a", mod_alphas, sep="")
 
 plotcva_data_list <- prepare_plotcva_data(coxnet0_cvaFITs, alphas=  mod_alphas)
 plot_data <- plotcva_data_list$plot_data
 highlight_points <- plotcva_data_list$highlight_points

 # Plot Loss vs. Lambda and Highlight Points
 ggplot() +
     geom_line(data = plot_data, aes(x = log(lambda), y = cvm, color = alpha)) +
     geom_point(data = highlight_points, aes(x = log(lambda), y = cvm, color = alpha, shape = type), 
                size = 3) +
     labs(title = "Loss Function vs. Lambda for Selected Values of Alpha",
          x = "Log(Lambda)",
          y = "Cross-Validation Error") +
     theme_minimal() +
     scale_color_discrete(name = "Alpha") +
    scale_shape_manual(name = "Lambda Type", values = c("min" = 17, "1se" = 19))
  # Create list of glance tibbles
  
  list_of_glances <- lapply(1:length(coxnet0_cvaFITs), 
                       FUN = function(i){
                          cv.fit <-coxnet0_cvaFITs[[i]]
                          broom::glance(cv.fit)
                       })
  cva.coxnet0_FITs_glance <- plyr::rbind.fill(list_of_glances)
  cva.coxnet0_FITs_glance <- cva.coxnet0_FITs_glance  %>% mutate(DV_name = DV_name, alphax = mod_alphas) %>% relocate(DV_name, nobs=, alphax )
 
                            
  

  