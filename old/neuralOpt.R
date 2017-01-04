#' Artificial Neural Network Optimization: Predictions of Physical Properties of Theoretical Hydrocarbon Blends
#'
#'A Monte Carlo Optimization scheme which utilizes neuralnet to create simulations and find the optimal predictive model.
#'
#'@param data Character list of data files to be read
#'@param fuelcomp data.frame() of hydrocarbon fuel blends to predict
#'@param inputCOLs Numeric values of which columns contain input data
#'@param outputCOL Numeric values of which column contains the output data
#'@param simCOUNT Number of simulations to perform
#'@param crossREF Number of times to cross reference each model
#'@param nodes A numerical list of nodes to be used for each respective data file
#'@param ignorewarns TRUE/FALSE argument. Turn on/off initial error checks before functions are run.d
#'@details First, neuralOpt handles sample size optimization and then a subsequent Monte Carlo optimization scheme to find the optimal parameters. This is then applied to the user-input data.frame to generate optimal predicted values
#'@return A data.frame containing the predicted values, set by the user in the argument fuelcomp
#'@note inputCOLs and outputCOL are defined as the column number. They are not defined as a data.frame column. All of the files must be formatted the same, in this regard. The following preliminary error checks are performed:
#'@note The data files you put in to the function must exist as a list and as characters. Ex: data=c(\'density.csv\', \'freezing.csv\').
#'@note For neuralOpt, you are only supposed to define the columns in which all of the values lie. NOT a data.frame like neuralSim.
#'@note The functions in DataMine cannot handle more than one output at a time! Just define them as separate datasets under data=
#'@note The number of variables in fuelcomp= do not match the number of variables in inputCOL=. You probably left the fuel blend percentages in the data.frame (from the function fuelComp). Take it out of your data.frame!
#'@export



neuralOpt <- function(data= c('density.csv'), fuelcomp= x, inputCOLs=2:5, outputCOL=6, simCOUNT=1000, crossREF=1000, nodes=1, ignorewarns=FALSE, neuralnetFormula=1){
	library(hydroGOF)
	library(neuralnet)
	library(reshape)

	if(ignorewarns==FALSE){

		    if (is.data.frame(inputCOLs==TRUE)){
		        print('For neuralOpt, you are only supposed to define the columns in which all of the values lie. NOT a data.frame like neuralSim.')
		    	stop('inputCOLs is a data.frame', call.=FALSE)
		    	}

			if (length(outputCOL) != 1){
				print('The functions in DataMine can not handle more more than one output at a time! Just define them as separate datasets under data=')
				stop('outputCOL is more than 1 column', call.=FALSE)
				}

			if (ncol(fuelcomp) != length(inputCOLs)){
				print('The number of variables in fuelcomp= do not match the number of variables in inputCOL=. You probably left the fuel blend percentages in the data.frame (from the function fuelComp). Take it out of your data.frame!')
				stop('fuelcomp= is not the same number of variables as inputCOLs=', call.=FALSE)
				}

			if (neuralnetFormula != 1){
				print('Be careful if you set your own formula. If you are running multiple files in neuralOpt, the inputCOLs and outputCOL column names, your variables, must the same.')
				}
				}


	result <- matrix(data=NA, ncol=length(data), nrow=nrow(fuelcomp))
	datafiles <- length(data)

	#i start
	id <- 1:datafiles
	for (i in id){


		options(warn=-1)
		Data1 <- read.csv(data[i])


		inputs <- Data1[,inputCOLs]
		in_colname <- names(inputs)
		outputs <- data.frame(Data1[,outputCOL]) #columns with output data
		out_colname <- names(Data1[outputCOL])
		rows <- nrow(inputs)
		formula <- as.formula(c(paste(out_colname, '~', paste(in_colname, collapse = "+"))))

		if (neuralnetFormula!=1){
			formula <- neuralnetFormula
			}

		lowestsamplesize <- round(rows*0.5)
		stepsize <- round(rows*0.05)
		highestsamplesize <- round(rows*0.95)
		incriments <- c(seq(lowestsamplesize, highestsamplesize, stepsize), rows-2)
		sample_table <- NULL
		yd <- 1:length(incriments)
		print('Calculating sample sizes...')

		#y start
		for (y in yd){

			ud <- 1:crossREF*1.5
			for (u in ud){
				dataset <- c(1:crossREF)-1
				q <- dataset[u]
				samplesize_0 <- incriments[y]
				testingsize <- rows-samplesize_0
				random_0 <- sample(1:rows, samplesize_0)
				parameters_0 <- inputs[random_0, ] #input data from the training set
				objectives_0 <- outputs[random_0, ] #output data from the training set
				test_parameters_0 <- inputs[-random_0,]
				test_objectives_0 <- outputs[-random_0,]
				norm_object_0 <- data.frame(scale(objectives_0)) #normalized value of objective values, which is needed to give ANNs the best results
				names(parameters_0) <- in_colname
				names(norm_object_0) <- out_colname
				Training_0 <- data.frame(parameters_0, norm_object_0) #binding the normalized objective values and the training data's inputs


				ANN_node <- neuralnet(formula, Training_0, hidden=5, threshold=c(0.1), rep=1, algorithm='rprop+', stepmax= 2e+05)
				net.results <- compute(ANN_node, test_parameters_0)
				predicted_0 <- sd(objectives_0)*net.results$net.result+mean(objectives_0)
				sample_resulttable <- data.frame(test_objectives_0, predicted_0)
				sample_table <- rbind(sample_table, sample_resulttable)
				}
			# u end

			#y continued
			rmse_PvO <- rmse(sample_table[,1], sample_table[,2])
			sample_rmse <- data.frame(samplesize_0, rmse_PvO)
			names(sample_rmse) <- c('samplesize', paste(data[i], 'RMSE'))
			print(sample_rmse)

			if (y==1){
				best_sample <- sample_rmse
				}

			if (sample_rmse[1,2] < best_sample[1,2]){
				best_sample <- sample_rmse
				}
			}
		#y end

		#i continued
		opt_samplesize <- best_sample[1,1]
		testingsize <- rows-opt_samplesize
		print(paste(data[i], 'Optimal samplesize found:', opt_samplesize, 'with an RMSE of:', print(best_sample[1,2])))
		print('Starting node calculations...')

		#t start
		td <- 1:nodes
		for (t in td){

			#g start
			gd <- 1:crossREF

			node_table <- matrix(data=NA, ncol=2, nrow=length(gd)*testingsize)
			for (g in gd){
				q <- c(1:crossREF)-1
				random_1 <- sample(1:rows, opt_samplesize) #randomly selecting data from the data set for sampling/training
				parameters_1 <- inputs[random_1, ] #input data from the training set
				objectives_1 <- outputs[random_1, ] #output data from the training set
				test_parameters_1 <- inputs[-random_1,]
				test_objectives_1 <- outputs[-random_1,]
				norm_object_1 <- data.frame(scale(objectives_1)) #normalized value of objective values, which is needed to give ANNs the best results
				names(parameters_1) <- in_colname
				names(norm_object_1) <- out_colname
				Training_1 <- data.frame(parameters_1, norm_object_1) #binding the normalized objective values and the training data's inputs

				ANN_node <- neuralnet(formula, Training_1, hidden=g, threshold=c(0.1), rep=1, algorithm='rprop+', stepmax= 2e+05)

				net.results <- compute(ANN_node, test_parameters_1)
				predicted_1 <- sd(objectives_1)*net.results$net.result+mean(objectives_1)
				node_resulttable <- data.frame(test_objectives_1, predicted_1)

				node_table[(q*testingsize+1):(g*testingsize), 1] <- node_resulttable[,1]
				node_table[(q*testingsize+1):(g*testingsize), 2] <- node_resulttable[,2]
				}
			#g end

			#t continued
			rmse_PvO <- rmse(node_table[,1], node_table[,2])
			node_rmse <- data.frame(t, rmse_PvO)
			names(node_rmse) <- c('hidden nodes', paste(data[i], 'RMSE'))
			print(node_rmse)

			if (t==1){
				best_nodes <- node_rmse
				}

			if (node_rmse[1,2] < best_nodes[1,2]){
				best_nodes <- node_rmse
				}
				opt_node <- best_nodes[1,1]
			}
		#t end

		#i continued
		print(paste(data[i], 'optimal node count found:', opt_node, 'with an RMSE of:', print(best_nodes[1,2])))
		print('Starting Monte Carlo scheme with optimized parameters...')
		#i cutoff

		#x start
		xd <- 1:simCOUNT #defining the loop with number of simulations to run

		for (x in xd){
			random <- sample(1:rows, opt_samplesize) #randomly selecting data from the data set for sampling/training
			parameters <- inputs[random, ] #input data from the training set
			objectives <- outputs[random, ] #output data from the training set
			norm_object <- data.frame(scale(objectives)) #normalized value of objective values, which is needed to give ANNs the best results
			names(parameters) <- in_colname
			names(norm_object) <- out_colname
			Training <- data.frame(parameters, norm_object) #binding the normalized objective values and the training data's inputs

			ANN <- neuralnet(formula, Training, hidden=opt_node, threshold=c(0.1), rep=1, algorithm='rprop+', stepmax= 2e+05)

			weight <- unlist(ANN$weight) #decompose the S3 class output from R to extract weights from ANN, aka modeling function
			weight.dataframe <- data.frame(weight)
			#x cuttoff

				#f start
				fd <- 1:crossREF
				table <- matrix(data=NA, ncol=2, nrow=length(fd)*testingsize)
				dataplacement <- c(1:length(fd))-1
				for (f in fd){
					dataplacement <- c(1:length(fd))-1
					q <- dataplacement[f]
					weight_nrow <- length(weight) #for the sake of variability in your outputs, I just do +/- .2. If you have your initial weights completely static
					randomized_weight_range <- seq(-0.2, 0.2, by=2e-04) #explanation above
					samples <- sample(-randomized_weight_range, weight_nrow) #explanation above
					weights_0 <- weight + samples #explanation above

					#Remember the inputs we didn't use up on top (in loop i)? Well, now that's for our output predictions
					random_0 <- sample(1:rows, opt_samplesize) #RNG randomized row numbers
					parameters_0 <- inputs[-random_0, ] #testing data inputs
					objectives_0 <- outputs[-random_0, ] #testing outputs

					norm_object_0 <- data.frame(scale(objectives_0)) #normalizing the output data
					names(parameters_0) <- in_colname
					names(norm_object_0) <- out_colname
					Training_0 <- data.frame(parameters_0, norm_object_0) #data table of inputs and normalized outputs

					ANN_0 <- neuralnet(formula, Training_0, hidden=opt_node, threshold=c(0.1), rep=1, algorithm='rprop+', startweights=weights_0, stepmax= 2e+05)
					net.results <- compute(ANN_0, parameters_0)
					predicted <- sd(objectives_0)*net.results$net.result+mean(objectives_0) #equation to denormalize outputs
					collective_weights <- unlist(ANN_0$weight) #weights
					PvO <- data.frame(objectives_0, predicted) #data table of the established (experimental) values and the model prediction
					table[(q*testingsize+1):(f*testingsize), 1] <- PvO[,1]
					table[(q*testingsize+1):(f*testingsize), 2] <- PvO[,2]
					}
				#f end

			#x continued
			data_0 <- table #renaming the data table
			colnames(data_0) <- c('experimental', 'predicted') #column naming

			rmse <- data_0 #simple renaming for keeping track of stuff later on
			rootmean <- rmse(data_0[,1], data_0[,2]) #RMSE
			stats <- cbind(weight.dataframe, 0)
			stats[1,2] <- rootmean
			names(stats) <- c('experimental', 'predicted') #column naming
			weightlength <- nrow(stats)
			overall_file <- suppressWarnings(rbind(stats, data_0))

			}
		#x end

		if (i==1){ #so this is a little complicated, I guess. what happens is the first run, I will relabel overall_file to overall
			overall <- overall_file #the data table "overall" is the eventual one that I will have when finished
			}

		if (overall_file[1,2] < overall[1,2]){ #every loop after that, I compare what I wrote as "overall" to the new simulation, "overall_file"
				overall <- overall_file #UNDER THE CONDITION that the RMSE of the new file is lower
			}						#So what happens is the file is written, then keeps getting rewritten when a lower RMSE simulation occurs

		weights <- data.frame(overall[1:weightlength,1])

		print(paste('Predicting user-supplied input data for', data[i]))

		#s start
		sd <- 1:50

		mean_pred <- matrix(data=NA, ncol=length(sd), nrow=(nrow(fuelcomp)))
		for (s in sd){
			random_2 <- sample(1:rows, opt_samplesize)
			parameters_2 <- inputs[random_2, ] #training data inputs
			objectives_2 <- outputs[random_2, ] #training outputs
			norm_object_2 <- data.frame(scale(objectives_2)) #normalizing the output data
			names(parameters_2) <- in_colname
			names(norm_object_2) <- out_colname
			Training_2 <- data.frame(parameters_2, norm_object_2) #data table of inputs and normalized outputs

			ANN_2 <- neuralnet(formula, Training_2, hidden=opt_node,
			threshold=c(0.1), rep=1, algorithm='rprop+', startweights=weights, stepmax= 2e+05)

			net.results <- compute(ANN_2, fuelcomp)
			predicted <- sd(objectives_1)*net.results$net.result+mean(objectives_1) #equation to denormalize outputs
			mean_pred[,s] <- predicted[,1]
			}
		#s end

		#i continued
		k <- data.frame(rowSums(mean_pred))/ncol(mean_pred)
		result[,i] <- k[,1]

		if(i!=length(data) & length(data)!=1){
			print(paste(data[i], 'complete. Cycling to', data[i+1], '...'))
			}

		}
		#i end

		print(paste('neuralOpt complete'))
		result <- data.frame(result)
		colnames(result) <- data
		data.frame(result)
		}
