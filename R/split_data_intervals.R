split_data_intervals <- function(data, time_start_var, time_stop_var, status_var, interval_length = 1) {

# Function to Divide Time Using survSplit() (Generic for Time Variables)
  split_data <- survSplit(as.formula(paste0("Surv(", time_start_var, ", ", time_stop_var, ", ", status_var, ") ~ .")), 
                          data = data, cut = seq(0, max(data[[time_stop_var]]), by = interval_length), 
                          start = time_start_var, end = time_stop_var, event = status_var)
  return(split_data)
}