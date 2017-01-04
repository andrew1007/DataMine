#' Support Vector Machine (SVM) Optimization
#'
#'A Monte Carlo Optimization scheme which utilizes e1071 to create simulations and find the optimal predictive model.
#'
#'@param inputCOLs Input values
#'@param outputCOL Objective values
#'@param simCOUNT Number of simulations to perform
#'@param crossREF Number of times to cross reference each model
#'@param samplesize The number of data points used to train the Kriging model. Unused points go to testing
#'@param kernel Kernel type to use
#'@param dimensions Number of dimensions to use
#'@param ignorewarns Ignore initial checking of user inputs for potential fatal errors during simulation.
#'
#'@return A list containing $predicted $experimental $RMSE $weights
#'@note Two global variables are generated: SVinputs and SVoutputs.
#'@note svmSim has preliminary error checks, which are as follows:
#'@note You set your samplesize= higher than the amount of data you have.
#'@note Your inputs and outputs must be in the form of data.frame().
#'@note The functions in DataMine can not handle more than one output at a time.
#'@return A list containing $predicted $experimental $rootmean $SV $kernel $dimensions)
#'@export


svmSim <- function(inputCOLs=2:5, outputCOL=6, kernel='polynomial', simCOUNT=1000, dimensions=3, samplesize=4, ignorewarns=FALSE){
library(e1071)

  if (ignorewarns==FALSE){
    if (samplesize >= nrow(inputCOLs)){
        print('You set your samplesize= higher than the amount of data you have.')
        stop('samplesize too high', call.=FALSE)
    }

    if (is.data.frame(inputCOLs==FALSE)){
        print('Your inputs and outputs must be in the form of data.frame(')
        stop('inputCOLs not a data.frame', call.=FALSE)
    }

    if (length(outputCOL) > 1){
      print('The functions in DataMine can not handle more more than one output at a time!')
      stop('More than one column in outputCOL', call.=FALSE)
    }

	}


	SVinputs <<- local(inputCOLs) #columns with input data
	inputs <- SVinputs
	SVoutputs <<- data.frame(outputCOL) #column with output data
	outputs <- SVoutputs
	rows <- nrow(inputs) #counting the amount of training data (aka rows in data table)

	id <- 1:simCOUNT #defining the loop with number of simulations to run
	for (i in id){ #loop
		random <- sample(1:rows, samplesize) #randomly selecting data from the data set for sampling/training
			#note that 'random' is simply an RNG, which is then used as to which rows of data are used as training data
		parameters <- inputs[random, ] #input data from the training set
		objectives <- outputs[random, ] #output data from the training set

		percenttesting <- (rows-samplesize)/rows #this is a measure of what % of your data is testing data
		repeatsize <- 5.2/percenttesting #my equation to determine how many times you need to repeat your model until you capture all of your experimental vs. predicted at least once for every dataset
		table <- NULL
		fd <- 1:repeatsize #values produced from the svm( algorithm are completely static and determined by the parameters used, aka kernel, degree, etc.
		for (f in fd){ #cross reference is only used to capture all of the data when cross referencing, because the values will stay the same. Usually 200 or 300 crossREF is plenty
			random <- sample(1:rows, samplesize) #RNG randomized row numbers
			test_parameters <- inputs[-random, ] #testing input data
			test_objectives <- outputs[-random, ] #testing output

			#svm model function
			svm <- svm(x=parameters, y=objectives, scale=TRUE, type='eps-regression', kernel=kernel,
				degree=dimensions ,coef0=2, fitted=TRUE, epsilon=0.000001, shrinking=TRUE, cross=4,
				probability=TRUE)
			SV <- data.frame(svm$SV)
			predictions <- predict(svm, test_parameters) #predicting values

			PvO <- data.frame(test_objectives, predictions) #data table of the established (experimental) values and the model prediction
			table <- rbind(table, PvO) #rbinding all cross referenced values into a single table from loop fd
			}
		data <- table

		#RMSE CALCULATION================================================================================================
			rmse <- table #simple renaming for keeping track of stuff later on
			rootmean <- rmse(data[,1], data[,2]) #RMSE

		#SETTING UP CSV FILE===============================================================================================
		stats <- data.frame(0, rootmean) #this writes the thetas/weights/weights for Kriging/ANN/SVM, respectively as a 2 column table, with the values on [,1] and all zeros on [,2]
print(stats)
			#stats[1,2] <- rootmean #inserting the calculated RMSE from the RMSE CALCULATION from above
			colnames(stats) <- c('experimental', 'predicted') #column naming
			colnames(data) <- c('experimental', 'predicted') #column naming
			print('okay')
		overall_file <- rbind(stats, data) #combining the weights, statistical data, and the predicted vs. experimental values all into 2 columns

		if (i==1){ #so this is a little complicated, I guess. what happens is the first run, I will relabel overall_file to overall
			overall <- overall_file #the data table "overall" is the eventual one that I will have when finished
		}
		if (overall_file[1,2] < overall[1,2] & i != 1){ #every loop after that, I compare what I wrote as "overall" to the new simulation, "overall_file"
			overall <- overall_file #UNDER THE CONDITION that the RMSE of the new file is lower
		}			#So what happens is the file is written, then keeps getting rewritten when a lower RMSE simulation occurs
	}
	data <- overall[-1,]
		names(data) <- c('experimental', 'predicted')
	rootmean <- overall[1,2]
	names(rootmean) <- 'RMSE'
	names(kernel) <- 'kernel'
	names(dimensions) <- 'dimensions'
	c(data, rootmean, SV, kernel, dimensions)
}
