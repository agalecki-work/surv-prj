
#=====> Time transformation
message("===Time transformatiom for:")
print(cxterms_tt)

if (ntvarsx  >= 3 & length(mod_cxterms_tt) > 0){ # Split time in data created by finegray()
   message(" ----> FG  Data with split time to accommodate time-dependent coefficients")
   tmp_tt   <- mod_cxterms_[mod_cxterms_tt]

# Define the cut points for time intervals
cut_points <- seq(0, max(train_fgdata$fgstop), by =  tt_split_length)  # Define 'some_interval' appropriately

# Split the data
 train_split_fgdata <- survSplit(Surv(fgstart, fgstop, fgstatus) ~ ., 
                        data = train_fgdata,                     
                        cut = cut_points,
                        episode = "interval")
 # Adjust the time-varying covariate
 # split_data$SEX_time <- (split_data$SEX-1)*sqrt(split_data$fgstop)
 
 
 if (!is.null(val_data)){
  val_split_fgdata <- survSplit(Surv(fgstart, fgstop, fgstatus) ~ ., 
                        data = val_fgdata,                     
                        cut = cut_points,
                        episode = "interval")
  }   # if !is.null(val_data)
 
# Adjust the time-varying covariate
 # split_data$SEX_time <- (split_data$SEX-1)*sqrt(split_data$fgstop)

} #  if ntvarsx  >= 3, finegray()


# fgfit1tm1_split <- coxph(Surv(fgstart, fgstop, fgstatus) ~ SEX + SEX_time, 
#          data=split_data, weight= fgwt, id =PID)
# print(summary(fgfit1tm1_split))

if (length(mod_cxterms_tt) > 0){  # time transformation variables
   message(" ----> Data expansion to accommodate time-dependent coefficients")
   tmp_tt   <- mod_cxterms_[mod_cxterms_tt]
   cxterms0 <- paste( tmp_tt, ":", "YRS_PRIMARY", sep="")
   #cxterms <-  c(mod_cxterms0, mod_cxterms_, cxterms0) 
  } else { # No tt transformation
   ## cxterms <- c( mod_cxterms0, mod_cxterms_)
  }
 
# print(cxterms)

ff <- function(x){ 
 fcxterms <- paste(cxterms,  collapse ="+")
 print(fcxterms)
 
 formcx  <- paste0("~0 +", fcxterms)
 csurv_form <-  paste0("Surv(YRS_PRIMARY, STATUS_PRI) ~ 0 +", fcxterms)
 
 train_Xmtx <- model.matrix(as.formula(formcx), data= train_data)
 cox_fit1 <- coxph(as.formula(csurv_form), data = train_data)
 summary(cox_fit1)
 cox.zph(cox_fit1)
}

