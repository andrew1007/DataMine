#' Artificial Neural Network Optimization
#'
#'A Monte Carlo Optimization scheme which utilizes neuralnet to create simulations and find the optimal predictive model.
#'
#'@param inputCOLs Input values
#'@param outputCOL Objective values
#'@param simCOUNT Number of simulations to perform
#'@param crossREF Number of times to cross reference each model
#'@param samplesize The number of data points used to train the Kriging model. Unused points go to testing
#'@param nodes Number of hidden nodes in the model
#'
#'@return A list containing $predicted $experimental $RMSE $nodes
#'@note Two global variables are generated: Kinputs and Koutputs.
#'@note neuralSim has preliminary error checks, which are as follows:
#'@note You set your samplesize= higher than the amount of data you have.
#'@note Your inputs and outputs must be in the form of data.frame().
#'@note The functions in DataMine can not handle more than one output at a time.
#'@export



neuralSim <- function(inputCOLs=2:5, outputCOL=6, simCOUNT=1000, crossREF=1000, samplesize= 32, nodes=7, ignorewarns=FALSE){
library(hydroGOF)
library(neuralnet)
library(reshape)

  if (ignorewarns==FALSE){
    if (samplesize >= nrow(inputCOLs)){
        print('You set your samplesize= higher than the amount of data you have.')
        stop('samplesize too high', call.=FALSE)
    }

    if (is.data.frame(inputCOLs==FALSE)){
        print('Your inputs and outputs must be in the form of data.frame(')
        stop('inputCOLs not a data.frame', call.=FALSE)
    }

    if (length(outputCOL) != 1){
      print('The functions in DataMine can not handle more more than one output at a time!')
      stop('More than one column in outputCOL', call.=FALSE)
    }

  }

	options(warn=-1)
	NNinputs <<- inputCOLs #columns with input data
		inputs <- NNinputs
	NNoutputs <<- data.frame(outputCOL) #columns with output data
		outputs <- NNoutputs
	rows <- nrow(inputs) #counting the amount of training data (aka rows in data table)

	id <- 1:simCOUNT #defining the loop with number of simulations to run
	for (i in id){ #loop
		random <- sample(1:rows, samplesize) #randomly selecting data from the data set for sampling/training
			#note that 'random' is simply an RNG, which is then used as to which rows of data are used as training data
		parameters <- inputs[random, ] #input data from the training set
		objectives <- outputs[random, ] #output data from the training set

    norm_object <- data.frame(scale(objectives)) #normalized value of objective values, which is needed to give ANNs the best results
  	Training <- data.frame(parameters, norm_object) #binding the normalized objective values and the training data's inputs
  	input_colname <- colnames(parameters)
  	output_colname <- colnames(norm_object)
  	NNformula <- as.formula(c(paste(output_colname, '~', paste(input_colname, collapse = "+"))))
  	#ANN function
  	ANN <- neuralnet(NNformula, Training, hidden=nodes,
  		threshold=c(0.1), rep=1, algorithm='rprop+', stepmax= 2e+05)

  	weight <- unlist(ANN$weight) #decompose the S3 class output from R to extract weights from ANN, aka modeling function
  	weight.dataframe <- data.frame(weight)
  	#this subloop, fd, takes the theta values you found from the i (above), with the weights constant
  	#resimCOUNT your ANN model repeatedly (cross referenced)
  	fd <- 1:crossREF
  	table <- NULL
  	for (f in fd){
  		weight_nrow <- length(weight) #for the sake of variability in your outputs, I just do +/- .2. If you have your initial weights completely static
  		randomized_weight_range <- seq(-0.2, 0.2, by=2e-04) #explanation above
  		samples <- sample(-randomized_weight_range, weight_nrow) #explanation above
  		weights_0 <- weight + samples #explanation above

  		##Remember the inputs we didn't use up on top (in loop i)? Well, now that's for our output predictions
  		random_0 <- sample(1:rows, samplesize) #RNG randomized row numbers
  		parameters_0 <- inputs[-random_0, ] #training data inputs
  		objectives_0 <- outputs[-random_0, ] #training outputs

  		norm_object_0 <- data.frame(scale(objectives_0)) #normalizing the output data
  		Training_0 <- data.frame(parameters_0, norm_object_0) #data table of inputs and normalized outputs
  		input_colname_0 <- colnames(parameters_0)
  		output_colname_0 <- colnames(norm_object_0)
  		NNformula_0 <- as.formula(c(paste(output_colname_0, '~', paste(input_colname_0, collapse = "+"))))

  		#cross referencing function
  		ANN_0 <- neuralnet(NNformula_0, Training_0, hidden=nodes,
  			threshold=c(0.1), rep=1, algorithm='rprop+', startweights=weights_0, stepmax= 2e+05)
  			net.results <- compute(ANN_0, parameters_0)

  		predicted <- sd(objectives_0)*net.results$net.result+mean(objectives_0) #equation to denormalize outputs
  		collective_weights <- weight #weights
  		PvO <- data.frame(objectives_0, predicted) #data table of the established (experimental) values and the model prediction
  		table <- rbind(table, PvO) #rbinding all cross referenced values into a single table from loop fd

  	}
  	data <- table #renaming the data table
  		colnames(data) <- c('experimental', 'predicted') #column naming


	#RMSE CALCULATION================================================================================================
	rmse <- data #simple renaming for keeping track of stuff later on
	rootmean <- rmse(data[,1], data[,2]) #RMSE
	stats <- cbind(weight.dataframe, 0)
	stats[1,2] <- rootmean
		colnames(stats) <- c('experimental', 'predicted') #column naming
	weightlength <- nrow(stats)
	overall_file <- suppressWarnings(rbind(stats, data))

	#[LAST STEP OF KRIGING AND ANN] COMPARING RMSE OF SEQUENTIAL FILES GENERATED AND PICKING THE BEST ONE============================================

		if (i==1){ #so this is a little complicated, I guess. what happens is the first run, I will relabel overall_file to overall
			overall <- overall_file #the data table "overall" is the eventual one that I will have when finished
		}
		if (overall_file[1,2] < overall[1,2]){ #every loop after that, I compare what I wrote as "overall" to the new simulation, "overall_file"
			overall <- overall_file #UNDER THE CONDITION that the RMSE of the new file is lower
		}							#So what happens is the file is written, then keeps getting rewritten when a lower RMSE simulation occurs

	}
	data1 <- overall[-(1:weightlength),]
		names(data) <- c('experimental', 'predicted')
	rootmean <- overall[1,2]
		names(rootmean) <- 'RMSE'
	weights <- data.frame(overall[1:weightlength,1])
		names(weights) <- 'weights'
		names(nodes) <- 'nodes'
	c(data1, rootmean, weights, nodes)
}
