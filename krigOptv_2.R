KrigOpt <- function(data_csv, test_data, input_cols, output_cols, samplesize_percentage = 0.85, sim_count = 10, cross_validate_count = 100, theta_upper = c(5, 5, 5, 5), theta_lower = c(0.01, 0.01, 0.01, 0.01)){
  data <- csv_data_setup(data_csv, input_cols, output_cols, samplesize_percentage)

  optimal_theta <- optimal_theta_monte_carlo(data$input, data$output, sample_size, cross_validate_count, sim_count)

  optimum_parameter_simulation <- krig_test_data_monte_carlo(test_data, data$input, data$output, data$sample_size, optimal_theta)

  final_table <- sum_mean(optimum_parameter_simulation, input_cols)
  final_table
}

optimal_theta_monte_carlo <- function(input, output, sample_size, cross_validate_count, sim_count){
  for (i in as.range(sim_count)){
    theta_values <- krig_simulation_thetas(input, output, sample_size)
    simulation_table <- krig_monte_carlo_table(input, output, sample_size, cross_validate_count, theta_values)
    rmse <- root_mean_sq(simulation_table)
    if (i == 1){
      best_thetas <- theta_values
      best_rmse <- rmse
    } else {
      if (best_rmse > rmse){
        best_thetas <- theta_values
        best_rmse <- rmse
      }
    }
  }
  best_thetas
}

krig_simulation_thetas <- function(input, ouputs, sample_size){
  data <- data_sampling_setup(input, output, sample_size)
  generated_model <- krig_model(data$sample_input, data$sample_output)
  krig_thetas(generated_model)
}

#these models probably don't accept vectors from data_sampling_setup
krig_test_data_monte_carlo <- function(test_data, input, output, sample_size, validation_count, thetas){
  full_table <- NULL
  for (i in as.range(validation_count)){
    sub_table <- NULL
    data <- data_sampling_setup(input, output, sample_size)
    simulation_model <- krig_model(data$sample_input, data$sample_output, theta_upper = thetas, theta_lower = thetas)
    predictions <- krig_predict(simulation_model, test_data)
    input_and_predicted_datum <- data.frame(test_data, predicted)
    sub_table <- rbind(sub_table, input_and_predicted_datum)
    full_table <- rbind(full_table, sub_table)
    full_table <- sum_mean(full_table, 1)
  }
  full_table
}

krig_monte_carlo_table <- function(input, output, sample_size, validation_count, thetas){ <- nrow(input) - sample_size
  full_table <- NULL
  for (i in as.range(validation_count)){
    sub_table <- preallocated_matrix(ncol = 2, nrow )
    data <- data_sampling_setup(input, output, sample_size)
    simulation_model <- krig_model(data$sample_input, data$sample_output, theta_upper = thetas, theta_lower = thetas)
    predictions <- krig_predict(simulation_model, data$test_input)
    experimental_v_predicted <- data.frame(data$test_output, predicted)
    #performance critical step with sum_mean!!!!
    sub_table <- insert_to_preallocated_matrix(sub_table, experimental_v_predicted, i, test_size)
    full_table <- rbind(full_table, sub_tables)
    full_table <- sum_mean(full_table, 1)
  }
  full_table
}

krig_model <- function(input, output, theta_upper = c(.01, .01, .01, .01), theta_lower = c(.05, .05, .05, .05)){
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
