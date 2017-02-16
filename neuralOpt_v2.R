NeuralNetOpt <- function(data_csv, test_data, samplesize_percentage = 0.85, input_cols, output_cols, sim_count = 10, cross_validate_count = 100, nodes = 1, hidden = 1){
  raw_data <- read.csv(data_csv)
  inputs <- input.data(raw_data, input_cols)
  outputs <- output.data(raw_data, output_cols)
  sample_size <- round(nrow(inputs) * samplesize_percentage)
  test_size <- nrow(raw_data) - sample_size

  optimal_weights <- optimal_weights_monte_carlo(inputs, outputs, sample_size, cross_validate_count, sim_count)
  neuralnet_calc(inputs, outputs, test_data, cross_validation_count, optimal_weights, hidden, nodes)
}

neuralnet_calc <- function(input, output, test_data, validation_count, weights, hidden, nodes){
  sample_table <- NULL
  for (i in as.range(validation_count)){
    neuralnet_model <- neuralnet_model(input, output, hidden, weights, nodes)
    neuralnet_predict <- neuralnet_compute(neuralnet_model, test_data, output)
    sample_table <- rbind(sample_table, data.frame(test_data, neuralnet_predict))
  }
  sum_mean(sample_table, as.range(ncol(input)))
}

optimal_weights_monte_carlo <- function(input, output, sample_size, cross_validate_count, sim_count){
  for (i in as.range(sim_count)){
    sim_weights = neuralnet_weights(formula = 1, input, output, hidden, startweights)
    randomized_weight <- randomized_weights(sim_weights)
    simulation_table <- neuralnet_monte_carlo_table(input, output, sample_size, cross_validate_count, randomized_weight)
    rmse <- root_mean_sq(simulation_table)
    if (i == 1){
      best_weights <- randomized_weight
      best_rmse <- rmse
    } else {
      if (best_rmse > rmse){
        best_weights <- randomized_weight
        best_rmse <- rmse
      }
    }
  }
  best_weights
}


neuralnet_monte_carlo_table <- function(input, output, sample_size, validation_count, weights){
  test_size <- nrow(inputs) - sample_size
  full_table <- NULL
  for(i in as.range(cross_validate_count)){
    sub_table <- preallocated_matrix(ncol = 2, nrow = test_size)
    data <- data_sampling_setup(input, output, sample_size)
    cross_validate_weights = randomize_weights(sim_weights)
    neuralnet_model <- neuralnet_model(formula = 1, data$sample_inputs, data$sample_outputs, hidden, startweights, sample_size)
    neuralnet_predict <- neuralnet_compute(neuralnet_model, data$testing_outputs, data$sample_outputs)
    result_table <- data.frame(neuralnet_predict, data$testing_outputs)

    start_range <- (i - 1) * test_size + 1
    end_range <- i * test_size
    sub_table[(start_range:end_range), 1] <- result_table[ ,1]
    sub_table[(start_range:end_range), 2] <- result_table[ ,2]
    full_table <- rbind(full_table, sub_table)
    full_table <- sum_mean(full_table)
  }
  full_table
}

neuralnet_weights <- function(formula = 1, input, output, hidden){
  normalized_output = scale(output)
  dataset = data.frame(input, normalized_output)
  output_colname<- colnames(output)
  input_colname <- colnames(input)
  formula <- as.formula(c(paste(output_colname, '~', paste(input_colname, collapse = "+"))))
  ann_model = neuralnet(formula, dataset, hidden= hidden, threshold=c(0.1), rep=1, algorithm='rprop+', stepmax= 2e+05)
  data.frame(unlist(ann_model$weight))
}

randomize_weights <- function(weights){
  random_weight_sequence <- seq(-0.2, 0.2, by = 2e-04)
  weight_scale_samples <- random_weights <- sample(random_weight_sequence, length(weights))
  weights + weight_scale_samples
}


neuralnet_model_with_startweights <- function(input, output, hidden, startweights, nodes= 3){
  normalized_output = scale(output)
  dataset = data.frame(input, normalized_output)
  output_colname<- colnames(output)
  input_colname <- colnames(input)
  formula <- as.formula(c(paste(output_colname, '~', paste(input_colname, collapse = "+"))))
  neuralnet(formula = formula, dataset, hidden=nodes, threshold=c(0.1), rep=1, algorithm='rprop+', startweights=weights_0, stepmax= 2e+05)
}


neuralnet_compute <- function(neuralnet_model, testing_inputs, sample_outputs){
  neuralnet_predict <- compute(neuralnet_model, testing_inputs)
  predicted_output <- data.frame(denormalize(neuralnet_predict$net.result, sample_outputs))
  colnames(predicted_output) <- "neuralnet prediction"
  predicted_output
}

neuralnet_model <- function(formula=NULL, input, output, nodes= 3){
  if (is.null(formula)){
    output_colname<- colnames(output)
    input_colname <- colnames(input)
    formula <- as.formula(c(paste(output_colname, '~', paste(input_colname, collapse = "+"))))
  }
  normalized_output = scale(output)
  dataset = data.frame(input, normalized_output)
  neuralnet(formula, dataset, hidden=nodes, threshold=c(0.1), rep=1, algorithm='rprop+', stepmax= 2e+05)
}
