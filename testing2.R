
# model <- neuralnet_model(NULL, input_dat, output_dat)
# neuralnet_compute <- function(model, testing_outputs){
#   neuralnet_predict <- compute(model, input_dat)
#   # print(neuralnet_predict)
#   data.frame(denormalize(neuralnet_predict), input_dat)
#   neuralnet_predict
# }
# x <- neuralnet_compute(model, input_dat, output_dat)
# randomize_weights(data.frame(1:10))


setwd('/home/andrew/Dropbox/RCode/RawData')
data <- read.csv('Freezing.csv')[,2:6]

setup <- data_setup(data[1:28,], 0.85)
input_dat <- setup$input
output_dat <- setup$output
colnames(output_dat) <- "ouput"
test_dat <- data[29:34,1:5]
weights <- neuralnet_weights(1, input=input_dat, output=output_dat, 4)

neuralnet_model_with_startweights <- function(input, output, hidden, startweights, nodes= 3){
  normalized_output = scale(output)
  dataset = data.frame(input, normalized_output)
  output_colname<- colnames(output)
  input_colname <- colnames(input)
  formula <- as.formula(c(paste(output_colname, '~', paste(input_colname, collapse = "+"))))
  print('SAdf')
  neuralnet(formula = formula, dataset, hidden=nodes, threshold=c(0.1), rep=1, algorithm='rprop+', stepmax= 2e+05)
}

h = neuralnet_model_with_startweights(input_dat, output_dat, 4, weights, nodes = 3)
