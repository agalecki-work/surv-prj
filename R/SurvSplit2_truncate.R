SurvSplit2_truncate <- function(df0, tvars, tm_horizon, postfix = "_"){
 
 #csurv <- paste0("Surv(", tvars[1], ",",  tvars[2], ")") # Surv(time, status)
 
 # Apply time horizon limit to the data frame
 df <- tibble(df0)
 nm1 <- paste0(tvars[1], postfix)
 nm2 <- paste0(tvars[2], postfix)
  #message("1")
  df[, nm1]  <- pmin(pull(df[, tvars[1]]), tm_horizon)
  #message("2")
  df[, nm2]  <- ifelse(pull(df[, tvars[1]]) > tm_horizon, 0, pull(df[, tvars[2]]))
  #message("3")
return(df)
}

# SurvSplit2_truncate (df =dfin_datax, tvars =tvars_vec, tm_horizon= tm_cut)

