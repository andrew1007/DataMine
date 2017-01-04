#' Error Plotting
#'
#'A plotting function. Generates an error table of the simulation function's optimal model.
#'
#'@param table The output of krigSim, neuralSim, or svmSim.
#'@param maxerror The maximum amount of percent error to be plotted.
#'@param datapoints Number data points generated, between 0 and the maxerror.
#'@param plot A TRUE/FALSE argument for plotting.
#'
#'@details This function is generic and applies to krigSim, neuralSim, and svmSim.
#'@return A data.frame containing percent error with respect to bulk of data with the error.
#'@export

errorplot <- function(table, maxerror=0.5, datapoints=100, plot=FALSE){
	data <- table
	file <- data.frame(data$experimental, data$predicted) #data.frame of experimental vs. predicted values, extracted from krigSim, svmSim, neuralSim
	Exp <- file[,1] #experimental values column
	Pred <- file[,2] #prediction values column
	Error <- abs(data.frame((Exp-Pred)/Exp)) #error equation

	#counting the number of values, used in the loop, below
	rowcount <- nrow(Error)
	intervals <- seq(0, maxerror, 1/(datapoints*2)) #the number of data points you want in your error plot

	id <- 1:length(intervals) #number of iteration, dictated by the argument dataspacing
	errorpoints <- NULL
	for (i in id){
		intervalpoint <- intervals[i]

		#creates a criteria: what rows from our Error data.frame are have and error below
		#one of our sequence values, intervals[i]
		errorcount <- Error[Error<intervalpoint,]
		points <- length(errorcount)
		standardized <- points/rowcount
		errorpoints <- rbind(errorpoints, standardized)
	}
	x <- intervals
	y <- errorpoints
	if(plot==TRUE){
		plot(x, y)
	}


	Data <- suppressWarnings(data.frame(intervals, errorpoints)) #dataframe of results
		names(Data) <- c('experimental', 'predicted')
	Data1 <- Data
}
