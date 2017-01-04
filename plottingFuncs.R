
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
