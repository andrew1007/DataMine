#'Support Vector Machine: Predictions of Physical Properties of Theoretical Hydrocarbon Blends
#'
#'A function that utilizes SVM for simulation-based optimization. Training data is incorporated into a Monte Carlo simulation optimization scheme. Which is then used to predict hydrocarbon blends, as defined by the user.
#'
#'@param data A list of data files to be read.
#'@param fuelcomp A data.frame of hydrocarbon blends of interest. This data is extracted from the function 'fuelComp'.
#'@param inputCOLs The columns in each .csv file with the inputs of each respective dataset.
#'@param outputCOL The columns in each .csv file with the output of each respective dataset.
#'@param simCOUNT The number of simulations to perform in the Monte Carlo optimization scheme.
#'@param crossREF The number of cross referencing to be done in each respective simulation.
#'@param samplesize The amount of data used for training with the other points used for testing. Can be a list for each data file or a percentage of the total data in each file. Default is .85 for training.
#'@param thetaUpper The maximum value of the theta parameter that may be used.
#'@param thetaLower The minimum value of the theta parameter that may be used.
#'@param ignorewarns Ignore initial checking of user inputs for potential fatal errors during simulation.
#'@param debug Performs a quick cycle through the entire simulation scheme, to test for errors prior to full simulations.
#'@details First, svmOpt handles sample size optimization and then a subsequent Monte Carlo optimization scheme to find the optimal parameters. This is then applied to the user-input data.frame to generate optimal predicted values
#'@return A data.frame containing the predicted values, set by the user in the argument fuelcomp
#'@note inputCOLs and outputCOL are defined as the column number. They are not defined as a data.frame column. All of the files must be formatted the same, in this regard. The following preliminary error checks are performed:
#'@note The data files you put in to the function must exist as a list and as characters. Ex: data=c(\'density.csv\', \'freezing.csv\').
#'@note For svmOpt, you are only supposed to define the columns in which all of the values lie. NOT a data.frame like svmSim.
#'@note The functions in DataMine cannot handle more than one output at a time! Just define them as separate datasets under data=
#'@note The number of variables in fuelcomp= do not match the number of variables in inputCOL=. You probably left the fuel blend percentages in the data.frame (from the function fuelComp). Take it out of your data.frame!
#'@export

svmOpt <- function(data=c('FlashPoint.csv'), fuelcomp= fuelcomp, inputCOLs=2:5, outputCOL=6, kernels=c('radial', 'polynomial', 'linear', 'sigmoid'), simCOUNT=1000, crossREF=1000, dimensionSim=1:2, ignorewarns=FALSE, debug=TRUE){
	library(Metrics)
	library(e1071)

	#i-loop of the list of files under the argument "data"
	#h-kernel types
	#j-number of simulations for each kernel
	#h-kernel cross validation
	#y-various sample sizes to test
	#u-sample size cross validation
	#g-loop of varius dimensions to test
	#x-dimensions cross validation
	#s-simulation with optimized parameters+predicting

	if (ignorewarns==FALSE){
		if (is.character(data)==FALSE){
			print('The data files you put in to the function must exist as a list and as characters. Ex: data=c(\'density.csv\', \'freezing.csv\').')
			stop('data= is not as.character', call.=FALSE)
		}


		if (is.data.frame(inputCOLs==TRUE)){
			print('For svmlOpt, you can only define the columns (as numbers) in which all of the values lie. NOT a data.frame like svmSim.')
			stop('inputCOLs is a data.frame', call.=FALSE)
		}

		if (length(outputCOL) != 1){
			print('The functions in DataMine can not handle more more than one output at a time! Just define them as separate datasets under data=')
			stop('outputCOL is more than 1 column', call.=FALSE)
		}

		if (ncol(fuelcomp) != length(inputCOLs)){
			print('The number of variables in fuelcomp= do not match the number of variables in inputCOL=. You probably left the fuel blend percentages in the data.frame (from the function fuelComp). Take it out of your data.frame!')
			stop('fuelcomp= is not the same number of inputs as inputCOLs=', call.=FALSE)
		}
	}

	if (debug==TRUE){
		print('Debug forces 5 simulations and 5 cross references for each data file.')
		print('Starting...')
		}

  	datafiles <- length(data)
	result <- matrix(data=NA, ncol=datafiles, nrow(fuelcomp))

	#i start
	id <- 1:length(data) #defining the loop with number of simulations to run
	for (i in id){ #loop

		if (debug==TRUE){
			print(paste('Initializing optimization of', data[i], '...'))
			print(paste('Setting up data.frames for input and outputs for', data[i]))
			}

		Data_1 <- read.csv(data[i])
		inputs <- data.frame(Data_1[,inputCOLs]) #columns with input data
		outputs <- data.frame(Data_1[,outputCOL]) #column with output data
		rows <- nrow(inputs) #counting the amount of training data (aka rows in data table)

		if (debug==TRUE){
			print(paste('Simulation start for svm model of', data[i], '...'))
			simCOUNT <- 5
			crossREF <- 5
			}

		#h start
		hd <- 1:length(kernels)
		for (h in hd){

			#j start
			kernelsimcount <- round(simCOUNT*0.10)
			jd <- 1:kernelsimcount
			for (j in jd){
				sampling <- round(rows*0.85)
				testingsize <- rows-sampling

				#f start
				table <- matrix(data=NA, ncol=2, nrow=crossREF*testingsize)
				fd <- 1:crossREF #values produced from the svm( algorithm are completely static and determined by the parameters used, aka kernel, degree, etc.
				for (f in fd){
					random <- sample(1:rows, sampling) #randomly selecting data from the data set for sampling/training
					#note that 'random' is simply an RNG, which is then used as to which rows of data are used as training data
					parameters <- inputs[random, ] #input data from the training set
					objectives <- outputs[random, ] #output data from the training set
					dataplacement <- c(1:crossREF)-1
					q <- dataplacement[f]
					degree <- 3
					test_parameters <- inputs[-random, ] #testing input data
					test_objectives <- outputs[-random, ] #testing output
					#svm model function

					if (debug==TRUE){
						print(paste('[ Simulation Cross Reference', kernels[h], ']','Running function svm of', data[i], '...', collapse=''))
					}

					svm0 <- svm(x=parameters, y=objectives, scale=TRUE, type='eps-regression', kernel=kernels[h],
					degree=degree ,coef0=2, fitted=TRUE, epsilon=0.000001, shrinking=TRUE, cross=4,
					probability=TRUE)

					if (debug==TRUE){
						print(paste('[ Simulation Cross Reference', kernels[h], ']','Model generated, predicting values from', data[i], '...', collapse=''))
						}

					predictions <- predict(svm0, test_parameters) #predicting values
					PvO <- data.frame(test_objectives, predictions) #data table of the established (experimental) values and the model prediction
					table[(q*testingsize+1):(f*testingsize),1] <- PvO[,1] #rbinding all cross referenced values into a single table from loop fd
					table[(q*testingsize+1):(f*testingsize),2] <- PvO[,2]
					}
				}
				#f end
			#j end

			#h continued
			if (debug==TRUE){
				print(paste('[ Simulation Cross Reference', kernels[h], ']', 'SVM kernel calculation complete for', data[i], collapse=''))
				print(paste('[ Simulation Data', kernels[h], ']', 'Kernel RMSE calculating for', data[i], '...', collapse=''))
				}

			rmse <- table #simple renaming for keeping track of stuff later on
			rootmean <- rmse(table[,1], table[,2]) #RMSE
			stats <- data.frame(degree, rootmean) #this writes the thetas/weights/weights for Kriging/ANN/SVM, respectively as a 2 column table, with the values on [,1] and all zeros on [,2]
			colnames(stats) <- c('dimensions', kernels[h]) #column naming

			if (h==1){ #so this is a little complicated, I guess. what happens is the first run, I will relabel overall_file to overall
				best_stats <- stats #the data table "overall" is the eventual one that I will have when finished
				}

			print(paste(kernels[h],'RMSE:'))
			print(stats[,2])

			if (debug==TRUE){
				print(paste('[ Simulation Data', kernels[h], ']', 'Comparing kernels against each other', data[i], '...', collapse=''))
				}

			if (stats[1,2] < best_stats[1,2]){ #every loop after that, I compare what I wrote as "overall" to the new simulation, "overall_file"
				best_stats <- stats #UNDER THE CONDITION that the RMSE of the new file is lower
				}			#So what happens is the file is written, then keeps getting rewritten when a lower RMSE simulation occurs
			} #h end

		#i continued
		opt_kernel <- colnames(best_stats)[2]
		print(paste(data[i], 'Optimal kernel found:', opt_kernel, 'with an RMSE of:', best_stats[1,2]))
		#i cutoff

		if (debug==TRUE){
			print(paste('Best kernel established for', data[i], collapse=''))
			print(opt_kernel)
			}

		#y start
		lowestsamplesize <- round(rows*0.5)
		stepsize <- round(rows*0.05)
		highestsamplesize <- round(rows*0.95)
		incriments <- c(seq(lowestsamplesize, highestsamplesize, stepsize), rows-1)
		sample_table <- NULL
		yd <- 1:length(incriments)
		for (y in yd){

			#u start
			ud <- 1:crossREF*1.5
			for (u in ud){
				dataset <- c(1:crossREF)-1
				q <- dataset[u]
				samplesize_0 <- incriments[y]
				testingsize <- rows-samplesize_0
				random_5 <- sample(1:rows, samplesize_0)
				parameters_5 <- inputs[random_5, ] #input data from the training set
				objectives_5 <- outputs[random_5, ] #output data from the training set
				test_parameters_5 <- inputs[-random_5,]
				test_objectives_5 <- outputs[-random_5,]

				svm5 <- svm(x=parameters_5, y=objectives_5, scale=TRUE, type='eps-regression', kernel=opt_kernel,
				degree=degree ,coef0=2, fitted=TRUE, epsilon=0.000001, shrinking=TRUE, cross=4,
				probability=TRUE)

				predictions_5 <- predict(svm5, test_parameters_5)
				sample_results <- data.frame(test_objectives_5, predictions_5)
				sample_table <- rbind(sample_table, sample_results)
				}
			#u end

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
		samplesize <- best_sample[1,1]
		testingsize <- rows-samplesize
		print(paste(data[i], 'Optimal samplesize found:', samplesize, 'with an RMSE of:', print(best_sample[1,2])))

		if (debug==TRUE){
			print(paste('[', opt_kernel, 'Dimension Optimization ]', 'Starting dimension loops for', data[i], '...', collapse=''))
			gd <- 1:5
			dimensionSim <- 1:5
			}

		#g start
		gd <- 1:length(dimensionSim) #values produced from the svm( algorithm are completely static and determined by the parameters used, aka kernel, degree, etc.
		for (g in gd){
			degree_1 <- g
			xd <- 1:crossREF
			dataplacement <- c(xd)-1
			dim_bind <- matrix(data=NA, ncol=2, nrow=length(xd)*testingsize)
			#g cut

			if (debug==TRUE){
				print(paste('[ Dimension', g, 'Cross Reference ]', 'Starting dimension cross referencing for', data[i], '...', collapse=''))
				}

			#x start
			for (x in xd){
				q <- dataplacement[x]
				random_1 <- sample(1:rows, samplesize) #RNG randomized row numbers
				parameters_1 <- inputs[random_1, ] #testing input data
				objectives_1 <- outputs[random_1, ] #testing output
				test_parameters_1 <- inputs[-random_1, ] #testing input data
				test_objectives_1 <- outputs[-random_1, ] #testing output

				if (debug==TRUE){
					print(paste('[ Dimension', g, 'Cross Reference ]', 'Running svm function for', data[i], '...', collapse=''))
					}

				svm_1 <- svm(x=parameters_1, y=objectives_1, scale=TRUE, type='eps-regression', kernel=opt_kernel,
				degree=degree_1 ,coef0=2, fitted=TRUE, epsilon=0.000001, shrinking=TRUE, cross=4,
				probability=TRUE)

				if (debug==TRUE){
					print(paste('[ Dimension', g, 'Cross Reference ]', 'Predicting values for', data[i], '...', collapse=''))
					}

				predictions_1 <- predict(svm_1, test_parameters_1) #predicting values
				table_1 <- data.frame(test_objectives_1, predictions_1)
				dim_bind[(testingsize*q+1):(x*testingsize),1] <- table_1[,1]
				dim_bind[(testingsize*q+1):(x*testingsize),2] <- table_1[,2]
				}
			#x end

			if (debug==TRUE){
				print(paste('[ Dimension', g, 'Cross Reference ]', 'Calculating RMSE for', data[i], '...', collapse=''))
				}

			#g continued
			rootmean_1 <- rmse(dim_bind[,1], dim_bind[,2])
			stats_1 <- data.frame(degree_1, rootmean_1)
				names(stats_1) <- c('dimensions', 'RMSE')

			if (g==1){ #so this is a little complicated, I guess. what happens is the first run, I will relabel overall_file to overall
				best_dim <- stats_1 #the data table "overall" is the eventual one that I will have when finished
				}

			if (debug==TRUE){
				print(paste('[Dimension', g, ']', 'Comparing dimensions against each other for', data[i], '...', collapse=''))
				}

			if ((stats_1[1,2] < best_dim[1,2])){ #every loop after that, I compare what I wrote as "overall" to the new simulation, "overall_file"
				best_dim <- stats_1 #UNDER THE CONDITION that the RMSE of the new file is lower
				}
			print(stats_1)

			#So what happens is the file is written, then keeps getting rewritten when a lower RMSE simulation occurs
			opt_dim <- best_dim[1,1]
			}
		#g end

		#i continued
		print(paste(data[i], 'Optimal dimension found:', opt_dim, 'with an RMSE of:', best_dim[1,2]))

		if (debug==TRUE){
			print(paste('[', data[i], 'Optimization, kernel',opt_kernel, 'Dimension', opt_dim, ']', 'Starting optimized svm calculations', collapse=''))
			sd <- 1:5
			}

		#s start
		sd <- 1:crossREF
		opt_svm <- matrix(data=NA, ncol=length(sd), nrow=nrow(fuelcomp))
		for (s in sd){
			random_2 <- sample(1:rows, samplesize) #RNG randomized row numbers
			parameters_2 <- inputs[random_2, ] #testing input data
			objectives_2 <- outputs[random_2, ] #testing output
			#svm model function

			if (debug==TRUE){
				print(paste('[', data[i], 'Optimal Model Simuation Cross Reference', s, ']', 'Running SVM...', collapse=''))
				}

			svm_2 <- svm(x=parameters_2, y=objectives_2, scale=TRUE, type='eps-regression', kernel=opt_kernel,
			degree=opt_dim ,coef0=2, fitted=TRUE, epsilon=0.000001, shrinking=TRUE, cross=4,
			probability=TRUE)

			if (debug==TRUE){
				print(paste('[', data[i], 'Optimal Model Simuation Cross Reference', s, ']', 'Predicting input values...', collapse=''))
				}

			predictions <- predict(svm_2, fuelcomp) #predicting values
			predicted <- data.frame(predictions)
			opt_svm[,s] <- predicted[,1]
			}
		#s end

		#i continued
		if (debug==TRUE){
			print(paste('[', data[i], 'modeling complete', ']', 'Binding and averaging (from cross referencing) predicted values for user input data.frame...', collapse=''))
			}

		averaged <- data.frame(rowSums(opt_svm))/ncol(opt_svm)
		result[,i] <- averaged[,1]

		if (debug==TRUE){
				print(paste('[', data[i], 'results complete', '] Starting next simulation loop...'))
				}

		}
	#i end

	if (debug==TRUE){
		print(paste('svmOpt complete'))
		}

	result <- data.frame(result)
		names(result) <- data
	result
	}
