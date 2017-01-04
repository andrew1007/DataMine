#' Support Vector Machine Compute
#'
#'A function that takes your optimized model and predicts values based on user-specified input values.
#'@param model Model from svmSim.
#'@param data Input data.
#'
#'@return A data.frame of predicted outputs.
#'@export

svmcomp <- function(model, data){
	x <- model
	z <- svm(x=data.frame(SVinputs), y=data.frame(SVoutputs), scale=TRUE, type='eps-regression', kernel=x$kernel,
		degree=x$dimensions ,coef0=2, fitted=TRUE, epsilon=0.000001, shrinking=TRUE, cross=4,
		probability=TRUE)
	predictions <- predict(z, data)
	data.frame(predictions)
}





