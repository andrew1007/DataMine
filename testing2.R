setwd('/home/andrew/Dropbox/R_files/RawData')


data <- read.csv('Freezing.csv')
dataset <- data_sampling_setup(data[,2:6], 28)
sample_input <- dataset$sample_input
sample_output <- dataset$sample_output
test_input <- dataset$test_input
test_output <- dataset$test_output

krig_monte_carlo_table <- function(table, sample_size, validation_count, thetas){
  full_table <- NULL
  for (i in as.range(validation_count)){
    test_data <-
    data <- list([sample_input]=table[,-ncol(table)], [sample_output] = table[,ncol(table)])
    if (isTraining == TRUE){
      data <- data_sampling_setup(table, sample_size)
      test_data <- data$test_input
    }
    simulation_model <- krig_model(data$sample_input, data$sample_output, theta_upper = thetas, theta_lower = thetas)
    predictions <- krig_predict(simulation_model, test_data)
    sub_table <- data.frame(data$test_output, predictions)
    full_table <- rbindFast(full_table, sub_table)
  }
  full_table
}

krig_monte_carlo_table(isTraining=TRUE, )
