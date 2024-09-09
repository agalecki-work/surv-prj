prepare_plotcva_data <- function(cv_results, alphas){
  plot_data <- data.frame()
  highlight_points <- data.frame()
  for (i in 1:length(cv_results)) {
    name <- names(cv_results)[i]
    cv_fit <- cv_results[[name]]
    lambda_values <- cv_fit$lambda
    cvm_values <- cv_fit$cvm
 
    tmp <- data.frame(project_name=project_name, DV_name = DV_name, lambda = lambda_values, cvm = cvm_values, alpha = factor(alphas[i]))
    
    plot_data <- rbind(plot_data, tmp )
    # Get lambda.min and lambda.1se
            lambda_min <- cv_fit$lambda.min
            lambda_1se <- cv_fit$lambda.1se
    
            min_index <- which(lambda_values == lambda_min)
            one_se_index <- which(lambda_values == lambda_1se)
            highlight_points <- rbind(highlight_points, data.frame(project_name=project_name,DV_name = DV_name, lambda = lambda_min, cvm = cvm_values[min_index], alpha = factor(alphas[i]), type = "min"),
                                  data.frame(project_name=project_name,DV_name = DV_name, lambda = lambda_1se, cvm = cvm_values[one_se_index], alpha = factor(alphas[i]), type = "1se"))

  }
  
  list(plot_data = plot_data, highlight_points = highlight_points)
}
