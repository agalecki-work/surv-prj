create_spline_basis <- function(data, var_name, df = 4) {
  spline_basis <- bs(data[[var_name]], df = df)
  spline_cols <- as.data.frame(spline_basis)
  colnames(spline_cols) <- paste0(var_name, "_spline_", 1:ncol(spline_basis))
  data <- cbind(data, spline_cols)
  return(data)
}

