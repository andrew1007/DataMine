#' Kriging Interpolation Compute
#'
#'A function that takes the optimized model and predicts values based on user-specified input values.
#'@param model Model from krigSim.
#'@param data Input data.
#'
#'@note Two global variables are used, which are generated from krigSim: Kinputs and Koutputs
#
#'@return A data.frame of predicted outputs.
#'@export


krigcomp <- function(model, data){
	x <- model
    y <- km(formula=~1, design=Kinputs, response=Koutputs,
         optim.method='BFGS', upper=x$thetas, parinit=c(.05, .05, .05, .05),
        lower=x$thetas)
    predictions <- predict(object=y, newdata=data, type='SK')
    z <- data.frame(predictions$mean)
    	names(z) <- 'predictions'
    z
}
