KrigOpt <- function(data_csv, test_data, input_cols, output_cols, samplesize_percentage = 0.85, sim_count = 10, cross_validate_count = 100, theta_upper = c(5, 5, 5, 5), theta_lower = c(0.01, 0.01, 0.01, 0.01)){
  data <- csv_data_setup(data_csv, input_cols, output_cols, samplesize_percentage)

  optimal_theta <- optimal_theta_monte_carlo(data$input, data$output, sample_size, cross_validate_count, sim_count)

  optimum_parameter_simulation <- krig_monte_carlo_table(isTraining=FALSE, test_data, data$input, data$output, data$sample_size, optimal_theta)

  final_table <- sum_mean(optimum_parameter_simulation, input_cols)
  final_table
}

optimal_theta_monte_carlo <- function(table, sample_size, cross_validate_count, sim_count){
  best_theta <- NULL
  best_rmse <- NULL
  for (i in as.range(sim_count)){
    theta_values <- krig_simulation_thetas(table, sample_size)
    simulation_table <- krig_monte_carlo_table(isTraining=TRUE, test_data = NULL, table, sample_size, cross_validate_count, theta_values)
    rmse <- root_mean_sq(simulation_table)
    thetas <- theta_comparison(theta_values, rmse, best_theta, best_rmse)
    best_theta <- thetas$theta
    best_rmse <- thetas$rmse
  }
  best_thetas
}

theta_comparison <- function(current_theta, current_rmse, best_theta = NULL, best_rmse = NULL){
  new_list <- list()
  if (is.null(best_theta)){
    new_list["theta"] = data.frame(current_theta)
    new_list["rmse"] = data.frame(current_rmse)
  } else {
    if (best_rmse > current_rmse){
      new_list["theta"] <- current_theta
      new_list["rmse"] <- current_rmse
    }
  }
  new_list
}

krig_simulation_thetas <- function(table, sample_size){
  data <- data_sampling_setup(table, sample_size)
  generated_model <- krig_model(data$sample_input, data$sample_output)
  krig_thetas(generated_model)
}

krig_monte_carlo_table <- function(isTraining=TRUE, test_data, table, sample_size, validation_count, thetas){
  full_table <- NULL
  for (i in as.range(validation_count)){
    data <- data_sampling_setup(table, sample_size)
    simulation_model <- krig_model(data$sample_input, data$sample_output, theta_upper = thetas, theta_lower = thetas)
    if (isTraining == TRUE){
      test_data <- data$test_input
    }
    predictions <- krig_predict(simulation_model, test_data)
    sub_table <- data.frame(data$test_output, predicted)
    full_table <- rbindFast(full_table, sub_tables)
  }
  full_table
}

krig_model <- function(table, theta_upper = c(.01, .01, .01, .01), theta_lower = c(.05, .05, .05, .05)){
  krig_model <- km(formula =~ 1, design = sample_rows, response = sample_ouputs, optim.method='BFGS', upper=c(5, 5, 5, 5), parinit=theta_lower, lower=theta_lower)
  krig_model
}

krig_predict <- function(krig_model, test_input){
  predictions <- predict(object = krig_model, newdata = test_input, type = 'SK')
  data.frame(predictions$mean)
}

krig_thetas <- function(krig_model){
  extract_1 <- unclass(krig_modl)
  extract_2 <- attr(extract_1, "covariance")
  extract_3 <- unclass(extract_2)
  theta_data <- data.frame(attr(extract_3, "range.val"))
  thetas <- c()
  for (i in 1:4){
    thetas <- c(thetas, theta_data[i, 1])
  }
  thetas
}
