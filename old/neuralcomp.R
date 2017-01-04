#' Artificial Neural Network Compute
#'
#'A function that takes your optimized model and predicts values based on user-specified input values.
#'@param model Model (from neuralSim)
#'@param data Input data
#'
#'@return A data.frame of predicted outputs.
#'@export

neuralcomp <- function(model, data){
	library(hydroGOF)
		x <- model
		parameters <- data.frame(NNinputs)
		objectives <- data.frame(NNoutputs)
		norm_object <- data.frame(scale(objectives))
		weights <- data.frame(x$weights)
		Training <- data.frame(parameters, norm_object)
		input_colname <- colnames(parameters)
		output_colname <- colnames(norm_object)
		NNformula <- as.formula(c(paste(output_colname, '~', paste(input_colname, collapse = "+"))))

		ANN_0 <- neuralnet(NNformula, Training, hidden=x$nodes,
		threshold=c(0.1), rep=1, algorithm='rprop+', startweights=weights, stepmax= 2e+05)
		comp <- compute(ANN_0, data)
		std <- sd(data.matrix(objectives))
		mn <- mean(data.matrix(objectives))
		predicton <- std*comp$net.result+mn
}
