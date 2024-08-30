
# Based on: 
# https://missingdatasolutions.rbind.io/2021/02/cox-external-validation/
# https://bmcmedresmethodol.biomedcentral.com/articles/10.1186/1471-2288-13-33
# https://missingdatasolutions.rbind.io/2022/12/cox-baseline-hazard/

#--  Royston, P. and Sauerbrei, W., 2008. Multivariable model-building: a pragmatic approach 
# to regression anaylsis based on fractional polynomials for modelling continuous variables.
# John Wiley & Sons.

# Objects needed: 

csurv    <- paste0("Surv(",tvars[1],",", tvars[2], ")")
cxform <- paste(x_cterms, sep="", collapse="+")

cform   <- paste0(csurv, "~", cxform) # Formula as a string

form <- as.formula(cform)

#-- Create Info lists for development and validation datasets

# clInfox is a prototype Info list
# Contents of this list will be used to feed `call()` function
clInfox <- list(
  data  = "Placeholder",
  formula = form
) 


clInfo1 <- clInfox
clInfo1$data <- as.name(df_names[1]) # Development data name : Ex: `df_dev`

clInfo2 <- clInfox
clInfo2$data <- as.name(df_names[2]) # Validation data name : Ex:  `df_val`

fit_dev <- do.call("coxph", clInfo1) # Model fit using developmental data
fit_val <- do.call("coxph", clInfo2) # Model fit using validation data


rm(clInfox, x_cterms, df_names)

    
assign("df_devx", eval(clInfo1$data), envir = .GlobalEnv)  #  `df_devx`: Working version of validation data frame 

assign("df_valx", eval(clInfo2$data), envir = .GlobalEnv)  #  `df_valx`: Working version of validation data frame 

graphics.off()

#----------  METHOD 1: Regression on PI in validation data

######### PI in train/development data

Xb_dev <- model.matrix(fit_dev) %*% coef(fit_dev)
Xbavg_dev <- sum(coef(fit_dev)*fit_dev$means) 
PI_dev <- Xb_dev - Xbavg_dev # centered PI in development dataset (for discrimination later)
df_devx$PI_dev <- PI_dev


#### PI in test/validation data
fit_val <- update(fit_dev, data = df_valx)

Xb_val <- model.matrix(fit_val) %*% coef(fit_dev) # determine Xb in validation data 
                                                  # by using coefficients of development data 
PI_val <- Xb_val - Xbavg_dev # center PI by using mean of PI of development data 

df_valx$PI_val <- PI_val

fit_PI_val <- update(fit_val, . ~ PI_val, data = df_valx)
fhtml <-paste0(prefix_cum, "_tabM01a-fit_PI_val.html") 
print(tab_model(fit_PI_val, transform=NULL, show.r2 = FALSE, file=fhtml))

fit_val_test <- update(fit_PI_val, . ~ PI_val + offset(PI_val))
fhtml <-paste0(prefix_cum, "_tabM01b-fit_val_test.html") 
print(tab_model(fit_val_test, transform=NULL, show.r2 = FALSE, file=fhtml))


# Note: The test if the slope deviates from one ( see for method Steyerberg, 2nd ed 15.3.3).

#--------------- METHOD 2: Check model misspecification/fit
#
# Apply a Jointed test to evaluate if the regression coefficients from the derivation dataset
# differ when estimated in the validation dataset (page 7 under Check model misspecification/fit).

fit_PIo_val <- update(fit_dev,  . ~ . + offset(PI_val), data = df_valx)
round(2*(diff(fit_PIo_val$loglik)), 2) # Chi-squared value

round(1-pchisq(2*(diff(fit_PIo_val$loglik)), 8), 3) # p-value

#----------- METHOD 3: Measures of discrimination

# Derivation
library(rms)
library(survAUC)


#--rcorr.cens(-1*PI_val, Surv(df_devx$time, df_devx$status))[1]
rcorr.cens(-1*PI_dev, Surv(df_devx[, tvars[1]], df_devx[, tvars[2] ]))[1] # Harrell's c-index

GHCI(PI_dev) # Gonen and Heller

# Validation
rcorr.cens(-1*PI_val, Surv(df_valx[, tvars[1]], df_valx[, tvars[2]]))[1] # Harrell's c-index in val_data

#--------- METHOD 4: Kaplan-Meier curves for risk groups
# Determine risk groups by categorizing the PI in four groups at the 16th, 50th and 84th centiles (page 5, under Example).
# Use the quantile function for that in the development and validation dataset. Create figure 2 on page 10.

# KM curve development data
quant_PI_dev <- quantile(PI_dev, probs=c(0, 0.16, 0.5, 0.84, 1)) # Determine risk groups
df_devx$Risk_group_dev <- cut(PI_dev, breaks = quant_PI_dev, include.lowest = TRUE,
                                 labels = c(1,2,3,4))
fit_dev_survfit <- survfit(Surv(df_devx[, tvars[1]], df_devx[, tvars[2]]) ~ Risk_group_dev, data = df_devx)
library(survminer)


fpdf <- paste0(prefix_cum, "_M4-KM.pdf")
pdf(fpdf)
par(mfrow = c(3, 1))

p1 <- ggsurvplot(fit_dev_survfit, title = "Survival curve (development)") # survival curve in development dataset
print(p1)

quant_PI_val <- quantile(PI_val, probs=c(0, 0.16, 0.5, 0.84, 1))
df_valx$Risk_group_val <- cut(PI_val, breaks = quant_PI_val,
                                include.lowest = TRUE,  labels = c(1,2,3,4))

# KM curve validation data
fit_val_survfit <- survfit(Surv(df_valx[, tvars[1]], df_valx[, tvars[2]]) ~ Risk_group_val, data = df_valx)
p2 <- ggsurvplot(fit_val_survfit, title = "Survival curve (validation)") # survival curve in validation dataset
print(p2)



library(dplyr)

df_val_KM <- df_valx %>%
  group_split(Risk_group_val)
surv_val <- lapply(df_val_KM, function(x){
  #--obj_surv <- survfit(Surv(time, status) ~ 1, data = x)
  cform1 <- paste0(csurv, "~1") 
  obj_surv <- survfit(as.formula(cform1), data = x)
  data.frame(time=obj_surv$time, surv=obj_surv$surv)
})

p3 <- p1$plot +
  geom_step(data=surv_val[[1]], aes(x = time, y = surv), linetype=2) +
  geom_step(data=surv_val[[2]], aes(x = time, y = surv), linetype=2) +
  geom_step(data=surv_val[[3]], aes(x = time, y = surv), linetype=2) +
  geom_step(data=surv_val[[4]], aes(x = time, y = surv), linetype=2) +
  labs(title= "Overlaid")

  
print(p3)
par(mfrow = c(1, 1)) #reset this parameter
dev.off()

#--- METHOD 5: Logrank or Cox P-values (deprecated)


#--- METHOD 6: Hazard ratios between risk groups

#-- (fit_dev_hr <-  coxph(Surv(time, status) ~ factor(Risk_group_dev), data=df_devx))
cform1 <- paste0(csurv, "~factor(Risk_group_dev)")
(fit_dev_hr <-  coxph(as.formula(cform1), data=df_devx))

fhtml <-paste0(prefix_cum, "_tabM06a-fit_dev_hr.html") 
print(tab_model(fit_dev_hr, show.r2 = FALSE, file=fhtml))

#-- (fit_val_hr <-  coxph(Surv(time, status) ~ factor(Risk_group_val), data=df_valx))
cform1 <- paste0(csurv, "~factor(Risk_group_val)")
(fit_val_hr <-  coxph(as.formula(cform1), data=df_valx))

fhtml <-paste0(prefix_cum, "_tabM06b-fit_val_hr.html") 
print(tab_model(fit_val_hr, show.r2 = FALSE, file=fhtml))


#--- METHOD 7: Calibration and the baseline hazard function
# Start by modeling the baseline hazard function in the derivation data 
bh <- basehaz(fit_dev) # Determine baseline hazard

#--baseh.all <- bh[match(df_devx$time, bh[, 2]), 1] # match baseline hazard to all survival times
baseh.all <- bh[match(df_devx[ , tvars[1]], bh[, 2]), 1]

df_devx <- data.frame(df_devx, baseh.all)
# take log of baseline hazard and use as outcome for next step
df_devx$ln_bh <- log(df_devx$baseh.all) 

# Model the log cumulative baseline hazard function (ln H0(t)) (Page 10, under Method 7)
#   df_devx$ln_bh  <- ifelse(df_devx$ln_bh == -Inf,min( , df_devx$ln_bh)
  df_devx <- within(df_devx,{
     ln_bh <- ifelse(ln_bh == -Inf, min(ln_bh[is.finite(ln_bh)]), ln_bh)
     ln_bh <- ifelse(ln_bh == +Inf, max(ln_bh[is.finite(ln_bh)]), ln_bh)
  })
  
  fit_bh <- glm(ln_bh_formula, data=df_devx)
  fhtml <- paste0(prefix_cum, "_tabM07-fit_bh.html")                 
  print(tab_model(fit_bh, show.r2 = FALSE, file=fhtml)) # result of model fit                 
  log_H0_dev <- predict(fit_bh)
  bh_pred <- exp(log_H0_dev)
  gr <- factor(rep(1:2, each=length(bh_pred)), labels = c("Observed", "Predicted"))

  #--df_plot <- data.frame(bh=c(df_devx$baseh.all, bh_pred), time=c(df_devx$time, df_devx$time), gr=gr)
  df_plot <- data.frame(bh=c(df_devx$baseh.all, bh_pred), time=c(df_devx[,tvars[1]], df_devx[,tvars[1]]), gr=gr)

  fpdf <- paste0(prefix_cum, "_M7.pdf")
  pdf(fpdf)
  p1x <- ggplot(df_plot, aes(x = time, y = bh, group=gr)) +
  geom_line(aes(color=gr)) + geom_rug(data = df_plot, aes(x = time), sides = "b")
  print(p1x)

  # S1.Calculate S0(t) in the validation dataset
 log_H0_val <- predict(fit_bh, newdata = df_valx)
 s0_val <- exp(-exp(log_H0_val)) # derive baseline survival function

# S2. For a given PI value compute the predicted survival function for each individual

pred_surv_val <- matrix(NA, nrow(df_valx), nrow(df_valx))
for(i in 1:nrow(df_valx)){
  pred_surv_val[i, ] <- s0_val^exp(df_valx$PI_val[i])
}
df_surv_val <- data.frame(LP_val=df_valx$Risk_group_val, pred_surv_val)

# S3. Average the curves over all members of each risk group (add them to development dataset)

library(dplyr)
df_surv_mean_val <- df_surv_val %>%
  group_by(LP_val) %>%
  summarise_all("mean")

df_surv_mean_val <- t(df_surv_mean_val[, -1])
df_valx <- data.frame(df_valx, df_surv_mean_val)

# S4. Plot curves in validation data

library(survminer)
tvar_name <- as.name(tvars[1])
cform1 <- paste0(csurv," ~ Risk_group_val")
fit<- survfit(as.formula(cform1), data = df_valx)
p1 <- ggsurvplot(fit)
p2x <- p1$plot +
  geom_line(data=df_valx, aes(x = !!tvar_name, y = X1)) +
  geom_line(data=df_valx, aes(x = !!tvar_name, y = X2)) +
  geom_line(data=df_valx, aes(x = !!tvar_name, y = X3)) +
  geom_line(data=df_valx, aes(x = !!tvar_name, y = X4)) 

print(p2x)
dev.off()
graphics.off()