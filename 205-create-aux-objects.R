# Auxiliary objects created for later use 
# Requires `work_data`

train_data <- work_data %>% filter(initSplit == 1)
message("--- `train_data` : ", nrow(train_data), "x" , ncol(train_data))

val_data <- work_data %>% filter(initSplit == 0)
message("--- `val_data` : ", nrow(val_data), "x" ,ncol(val_data))

BM_vars <- paste("BM", 1:21, sep = "")
xvars    <- c("AGE")
nsdf3_vars <- c(BM_vars, xvars) # Splines withh df=3 will be generated for these variables

ns_df3 <- sapply(nsdf3_vars, FUN= function(cx){
  splinex <- ns(train_data[, cx],df=3) 
  attr(splinex, "knots")
})  # matrix with spline knots for selected vars,  df=3. Colnames correspond to var names
