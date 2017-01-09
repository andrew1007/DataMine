KrigOpt <- function(training_data, test_data, samplesize_percentage = 0.85, sim_count = 5, cross_validate_count = 5){
  data <- data_setup(training_data, samplesize_percentage)
  print(data)
  optimal_theta <- optimal_theta_monte_carlo(data$full_table, data$sample_size, cross_validate_count, sim_count)
  print(data.frame(1:100))
  # print(data$sample_size)
  # optimum_parameter_simulation <- krig_monte_carlo_scheme(isTraining=FALSE, test_data, data$input, data$output, data$sample_size, optimal_theta)
  # optimum_parameter_simulation
  # final_table <- sum_mean(optimum_parameter_simulation, input_cols)
  # final_table
}

# KrigOpt(input_dat, test_dat)

optimal_theta_monte_carlo <- function(table, sample_size, cross_validate_count, sim_count){
  best_theta <- NULL
  best_rmse <- NULL
  data.frame(1:200)
  for (i in as.range(sim_count)){
    theta_values <- krig_simulation_thetas(table, sample_size)
    simulation_table <- krig_monte_carlo_scheme(table, sample_size, cross_validate_count, theta_values)
    data.frame(1:100)
    rmse <- root_mean_sq(simulation_table)
    thetas <- theta_comparison(theta_values, rmse, best_theta, best_rmse)
    best_theta <- thetas$theta
    best_rmse <- thetas$rmse
  }
  best_theta
}

krig_simulation_thetas <- function(table, sample_size){
  data <- data_sampling_setup(table, sample_size)
  generated_model <- krig_model(data$sample_input, data$sample_output)
  krig_thetas(generated_model)
}

krig_monte_carlo_scheme <- function(table, sample_size, validation_count, thetas){
  full_table <- NULL
  for (i in as.range(validation_count)){
    data <- data_sampling_setup(table, sample_size)
    simulation_model <- krig_model(data$sample_input, data$sample_output, theta_upper = thetas, theta_lower = thetas)
    predictions <- krig_predict(simulation_model, data$test_input)
    sub_table <- data.frame(data$test_output, predictions)
    full_table <- rbindFast(full_table, sub_table)
  }
  full_table
}

krig_model <- function(input_table, output_table, theta_upper = c(.01, .01, .01, .01), theta_lower = c(.05, .05, .05, .05)){
  krig_model <- km(formula =~ 1, design = input_table, response = output_table, optim.method='BFGS', upper=c(5, 5, 5, 5), parinit=theta_lower, lower=theta_lower)
  krig_model
}

krig_predict <- function(krig_model, test_input){
  predictions <- predict(object = krig_model, newdata = test_input, type = 'SK')
  data.frame(predictions$mean)
}

theta_comparison <- function(current_theta, current_rmse, best_theta = NULL, best_rmse = NULL){
  new_list <- list()
  if (is.null(best_theta)){
    new_list["theta"] = data.frame(current_theta)
    new_list["rmse"] = data.frame(current_rmse)
  } else {
    if (best_rmse > current_rmse){
      new_list["theta"] <- data.frame(current_theta)
      new_list["rmse"] <- data.frame(current_rmse)
    } else {
      new_list["theta"] <- data.frame(best_theta)
      new_list["rmse"] <- data.frame(best_rmse)
    }
  }
  new_list
}

krig_thetas <- function(krig_model){
  extract_1 <- unclass(krig_model)
  extract_2 <- attr(extract_1, "covariance")
  extract_3 <- unclass(extract_2)
  theta_data <- data.frame(attr(extract_3, "range.val"))
  thetas <- c()
  for (i in 1:4){
    thetas <- c(thetas, theta_data[i, 1])
  }
  thetas
}
