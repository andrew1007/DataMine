
SvmOpt <- function(data_csv, test_data, dimensions, input_cols, output_cols, sample_percentage = 0.85, kernel ='polynomial', sim_count = 10, cross_validate_count = 100){
  raw_data <- read.csv(data_csv)
  inputs <- input.data(raw_data, input_cols)
  outputs <- output.data(raw_data, output_cols)
  sample_size <- round(nrow(inputs) * sample_percentage)
  test_size <- nrow(raw_data) - sample_size


}

svm_monte_carlo <- function(){
  
}
