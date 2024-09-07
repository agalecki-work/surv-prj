count_columns_per_term <- function(formula, data) {
  # Generate the design matrix
  design_matrix <- model.matrix(formula, data = data)
  
  # Extract terms from the formula
  terms_obj <- terms(formula)
  term_labels <- attr(terms_obj, "term.labels")
  
  # Initialize a named list to store counts
  term_column_counts <- setNames(integer(length(term_labels)), term_labels)
  
  # Extract the 'assign' attribute from the design matrix
  assign_vec <- attr(design_matrix, "assign")
  
  # Iterate over each term
  for (i in seq_along(term_labels)) {
    # Count the columns assigned to the current term
    term_column_counts[i] <- sum(assign_vec == i)
  }
  
  return(term_column_counts)
}
