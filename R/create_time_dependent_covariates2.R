create_time_dependent_covariates2 <- function(data, var_names, time_var) {
  # Iterate over the variable names and create the time-dependent covariates
  # Ex: mutate(time_mid = (fgstart + fgstop) / 2)
  data %>%
    mutate(across(all_of(var_names), ~ . * .data[[time_var]], .names = "{col}_time"))
}
