# Create Time-Dependent Covariates
create_time_dependent_covariates <- function(data, var_name, time_start, time_stop) {
  # Calculate the midpoint of the interval
  data$time_mid <- (data[[time_start]] + data[[time_stop]]) / 2
  
  # Create time-dependent interactions for each spline basis function
  for (i in 1:4) {
    spline_col_name <- paste0(var_name, "_spline_", i)
    time_int_col_name <- paste0(var_name, "_spline_", i, "_time")
    data[[time_int_col_name]] <- data[[spline_col_name]] * data$time_mid
  }
  
  return(data)
}

# Create time-dependent covariate interactions
#  split_data <- split_data %>%
#    mutate(time_mid = (fgstart + fgstop) / 2) %>%
#    mutate(across(starts_with("age_spline_"), ~ . * time_mid, .names = "{col}_time"))
  