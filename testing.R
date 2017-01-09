setwd('/home/andrew/Dropbox/R_files/RawData')


data <- read.csv('Freezing.csv')
dataset <- data_sampling_setup(data[,2:6], 28)
sample_input <- dataset$sample_input
sample_output <- dataset$sample_output
test_input <- dataset$test_input
test_output <- dataset$test_output

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
  best_theta
}

model <- krig_model(sample_input, sample_output)
thetas <- krig_thetas(model)

predictions <- krig_predict(model, test_input)

krig_monte_carlo_table(isTraining=TRUE, test_input, dataset, 28, 10, thetas)

data <- read.csv('Freezing.csv')[,2:6]
dataset <- data_sampling_setup(data[,2:6], 28)
sample_input <- dataset$sample_input
sample_output <- dataset$sample_output
test_input <- dataset$test_input
test_output <- dataset$test_output

#krig_monte_carlo_table
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
krig_simulation_thetas(data, 28)
krig_monte_carlo_scheme(data, 28, 5, data.frame(0.3,0.35534, 0.2321, 0.456))
# krig_model(data)


root_mean_sq(table)

optimal_theta_monte_carlo <- function(table, sample_size, cross_validate_count, sim_count){
  best_theta <- NULL
  best_rmse <- NULL
  for (i in as.range(sim_count)){
    theta_values <- krig_simulation_thetas(table, sample_size)
    simulation_table <- krig_monte_carlo_scheme(table, sample_size, cross_validate_count, theta_values)
    rmse <- root_mean_sq(simulation_table)
    thetas <- theta_comparison(theta_values, rmse, best_theta, best_rmse)
    best_theta <- thetas$theta
    best_rmse <- thetas$rmse
  }
  best_theta
}


data <- read.csv('Freezing.csv')[,2:6]
input_dat <- data[1:28,1:4]
test_dat <- data[29:34,5]


setup <- data_setup(data, 0.85)

optimal_theta_monte_carlo(setup$full_table, setup$sample_size, 5, 5)

KrigOpt(input_dat, test_dat, 1:4, sim_count = 1, cross_validate_count = 1)
