#' Scatter Plotting
#'
#'A plotting function. Generates a scatterplot table of the average predicted vs. experimental values from a simulation function's optimal model.
#'
#'@param table Define the variable of krigSim, neuralSim, or svmSim
#'@param plot A TRUE/FALSE argument for plotting.
#'
#'@details scatterplot is an all-in-one function that handles krigSim, neuralSim, and svmSim outputs. For error analysis/visualization.
#'@return A data.frame containing experimental vs. predicted values (averaged out).
#'@export

scatterplot <- function(table, plot=FALSE){
	data <- table
	Property <- data.frame(data$experimental, data$predicted)
	#finds all unique experimental points, and finding the length (to use in defining id)
	experimental <- unique(Property[, 1])
	expCount <- length(experimental)

	#loop. Each iteration determines the rows with the experimental value (column 1)
	#Then it takes the values of the predicted values (column 2)
	predicted <- NULL
	id <- 1:expCount
	for (i in id){
		value <- Property[Property[,1] == experimental[i], ]
		Predicted <- mean(value[,2])
		predicted <- rbind(predicted, Predicted)
	}

	#plotting
	x <- experimental
	y <- predicted
		names(x) <- 'experimental'
		names(y) <- 'predicted'
 	if(plot==TRUE){
		plot(x, y)
	}
	Data <- suppressWarnings(data.frame(x,y)) #dataframe of results
		names(Data) <- c('experimental', 'predicted')
 	Data
}
