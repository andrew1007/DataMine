#'Kriging Interoplation Optimization: Predictions of Physical Properties of Theoretical Hydrocarbon Blends
#'
#'A function that utilizes Kriging interpolation for simulation-based optimization. Which is then used to predict hydrocarbon blends, as defined by the user.
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
#'@details First, krigOpt handles sample size optimization and then a subsequent Monte Carlo optimization scheme to find the optimal parameters. This is then applied to the user-input data.frame to generate optimal predicted values
#'@return A data.frame containing the predicted values, set by the user in the argument fuelcomp
#'@note inputCOLs and outputCOL are defined as the column number. They are not defined as a data.frame column. All of the files must be formatted the same, in this regard. The following preliminary error checks are performed:
#'@note The data files you put in to the function must exist as a list and as characters. Ex: data=c(\'density.csv\', \'freezing.csv\').
#'@note For krigOpt, you are only supposed to define the columns in which all of the values lie. NOT a data.frame like krigSim.
#'@note The functions in DataMine cannot handle more than one output at a time! Just define them as separate datasets under data=
#'@note The number of variables in fuelcomp= do not match the number of variables in inputCOL=. You probably left the fuel blend percentages in the data.frame (from the function fuelComp). Take it out of your data.frame!
#'@export


krigOpt <- function(data= c('density.csv'), fuelcomp=x, inputCOLs=2:5, outputCOL=6, simCOUNT=10, crossREF=10, thetaUpper= c(5, 5, 5, 5), thetaLower= c(0.01, 0.01, 0.01, 0.01), ignorewarns=FALSE){

  #errorchecking
  if (ignorewarns==FALSE){
    if (is.character(data)==FALSE){
      print('The data files you put in to the function must exist as a list and as characters. Ex: data=c(\'density.csv\', \'freezing.csv\').')
      stop('data= is not as.character', call.=FALSE)
    }

      if (is.data.frame(inputCOLs==TRUE)){
          print('For krigOpt, you are only supposed to define the columns in which all of the values lie. NOT a data.frame like neuralSim.')
        stop('inputCOLs is a data.frame', call.=FALSE)
      }

    if (length(outputCOL) != 1){
      print('The functions in DataMine can not handle more than one output at a time! Just define them as separate datasets under data=')
      stop('outputCOL is more than 1 column', call.=FALSE)
    }

    if (ncol(fuelcomp) != length(inputCOLs)){
      print('The number of variables in fuelcomp= do not match the number of variables in inputCOL=. You probably left the fuel blend percentages in the data.frame (from the function fuelComp). Take it out of your data.frame!')
      stop('fuelcomp= is not the same number of inputs as inputCOLs=', call.=FALSE)
    }
  }

  #COMPLETE LOOP FOR EACH FILE TO BE READ AND OPTIMIZED
  result <- matrix(data=NA, ncol=datafiles, nrow=nrow(fuelcomp)) #preallocated matrix
  datafiles <- length(data) #automated criteria for the number of loops to run, based on number of .csv files
  id <- 1:datafiles #this is the outer loop for each .csv file
  for (i in id){
    options(warn=-1)
    Data1 <- read.csv(data[i])
    inputs <- Data1[,inputCOLs]
    outputs <- data.frame(Data1[,outputCOL]) #columns with output data
    rows <- nrow(inputs) #number of datasets in the file

    sample_table <- NULL
    yd <- 1:length(incriments)#this is the sample size testing loop
    for (y in yd){

      incriments <- seq(8, (rows-4), 4) #the number and amount of sample sizes to test
      ud <- 1:crossREF
      for (u in ud){
        samplesize_0 <- incriments[y] #which sample size to use
        testingsize <- rows-samplesize_0
        random_5 <- sample(1:rows, samplesize_0) #finding random values
        parameters_5 <- inputs[random_5, ] #input data from the training set
        objectives_5 <- outputs[random_5, ] #output data from the training set
        test_parameters_5 <- inputs[-random_5,] #the parameters used to predict values after km( is run
        test_objectives_5 <- outputs[-random_5,] #the output values used to compare predicted and experimental values

        kriging_0 <- km(formula=~1, design=data.frame(parameters_5), response=data.frame(objectives_5),
                        optim.method='BFGS', upper=c(5, 5, 5, 5), parinit=c(.05, .05, .05, .05),
                        lower=c(.01, .01, .01, .01))
        predictions <- predict(object=kriging_0, newdata=test_parameters_5, type='SK') #function that generates model predictions, using the testing data defined above

        experimental <- data.frame(test_objectives_5) #data.frame out of experimental data
        predicted <- data.frame(predictions$mean) #extracting the mean values from the function predict(
        PvO <- data.frame(experimental, predicted) #experimental vs predicted values
        sample_table <- rbind(sample_table, PvO)
        }

      rmse_PvO <- rmse(sample_table[,1], sample_table[,2]) #rmse calculation
      sample_rmse <- data.frame(samplesize_0, rmse_PvO)
      names(sample_rmse) <- c('samplesize', paste(data[i], 'RMSE'))

      #the comparisons of each sample size as they are generated, based on their rmse (lower rmse is kept)
      if (y==1){
        best_sample <- sample_rmse
      }
      if (sample_rmse[1,2] < best_sample[1,2]){
        best_sample <- sample_rmse
      }
    }
      #best samplesize, along with testingsize
      opt_samplesize <- best_sample[1,1]
      testingsize <- rows-opt_samplesize
      print(paste(data[i], 'Optimal samplesize found:', opt_samplesize, 'with an RMSE of:', print(best_sample[1,2])))
      #OPTIMAL SAMPLE SIZE COMPLETE

      #MONTE CARLO SIMULATIONS
      xd <- 1:simCOUNT #defining the loop with number of simulations to run
      for (x in xd){ #loop
        random <- sample(1:rows, opt_samplesize) #randomly selecting data from the data set for sampling/training
        parameters <- inputs[random,] #input data from the training set
        objectives <- outputs[random,] #output data from the training set

        #Kriging function
        kriging <- km(formula= ~1, design=data.frame(parameters), response=data.frame(objectives),
                      optim.method='BFGS', parinit=c(.05, .05, .05, .05),lower=thetaLower, upper=thetaUpper)

        unclass <- unclass(kriging) #extracting theta values out of function 'km'
        cov.attr <- attr(unclass, "covariance") #extracting theta values out of function 'km'
        unclass_theta <- unclass(cov.attr) #extracting theta values out of function 'km'
        thetas <- data.frame(attr(unclass_theta, 'range.val')) #extracting theta values out of function 'km'... yes it's that difficult
        names(thetas) <- 'thetas'
        c1 <- thetas[1,1] #theta 1
        c2 <- thetas[2,1] #theta 2
        c3 <- thetas[3,1] #theta 3
        c4 <- thetas[4,1] #theta 4

        table <- matrix(data=NA, ncol=2, nrow=(testingsize*crossREF))
        #INTERNAL CROSS VALIDATION FOR EACH SET OF THETA PARAMETERS
        fd <- 1:crossREF
        for(f in fd){
            dataplacement <- c(1:crossREF)-1
            q <- dataplacement
            random_0 <- sample(1:rows, opt_samplesize) #RNG random numbers to be used as training data
            parameters_0 <- inputs[random_0,] #training data inputs
            objectives_0 <- outputs[random_0, ] #training data outputs

            test_parameters_0 <- inputs[-random_0, ] #testing data inputs. note that this is defined as all of the values not used for sampling
            test_objectives_0 <- outputs[-random_0, ] #outputs of testing data. this is used when comparing model-predicted values and actual

            #kriging function inner cross referencing
            kriging_0 <- km(formula=~1, design=data.frame(parameters_0), response=data.frame(objectives_0),
                            optim.method='BFGS', upper=c(c1,c2,c3,c4), parinit=c(.05, .05, .05, .05),
                            lower=c(c1,c2,c3,c4))
            predictions <- predict(object=kriging_0, newdata=test_parameters_0, type='SK') #function that generates model predictions, using the testing data defined above

            experimental <- data.frame(test_objectives_0) #data.frame out of experimental data
            predicted <- data.frame(predictions$mean) #extracting the mean values from the function predict(
            collective_weights <- thetas #defining thetas again
            PvO <- data.frame(experimental, predicted) #experimental vs predicted values

            table[(q*testingsize+1):(f*testingsize),1] <- PvO[,1]
            table[(q*testingsize+1):(f*testingsize),2] <- PvO[,2]
        }
    data1 <- table #renaming the data table


      #RMSE CALCULATION AND OPTIMAL THETA ELUCIDATION
      rmse <- data1 #simple renaming for keeping track of stuff later on
      rootmean <- rmse(data1[,1], data1[,2]) #RMSE

      stats <- cbind(collective_weights, 0) #this writes the thetas/weights/weights for Kriging/ANN/SVM, respectively as a 2 column table, with the values on [,1] and all zeros on [,2]
      stats[1,2] <- rootmean #inserting the calculated RMSE from the RMSE CALCULATION from above
      colnames(stats) <- c('experimental', 'predicted') #column naming
      colnames(data1) <- c('experimental', 'predicted') #column naming
      overall_file <- rbind(stats, data1) #combining the weights, statistical data, and the predicted vs. experimental values all into 2 columns

      if (i==1){ #so this is a little complicated, I guess. what happens is the first run, I will relabel overall_file to overall
        overall <- overall_file #the data table "overall" is the eventual one that I will have when finished
      }
      if (overall_file[1,2] < overall[1,2]){ #every loop after that, I compare what I wrote as "overall" to the new simulation, "overall_file"
        overall <- overall_file #UNDER THE CONDITION that the RMSE of the new file is lower
      }							#So what happens is the file is written, then keeps getting rewritten when a lower RMSE simulation occurs
    }

    thetas <- c(overall[1:4,1])
    #BEST THETA PARAMETERS FOUND

    #USAGE OF OPTIMAL SAMPLE SIZE AND THETA PARAMETERS FOR PREDICTIONS
    sd <- 1:50
    mean_pred <- matrix(data=NA, ncol=length(sd), nrow=nrow(fuelcomp))
    for(s in sd){
      random_0 <- sample(1:rows, opt_samplesize) #RNG random numbers to be used as training data
      parameters_0 <- inputs[random_0,] #training data inputs
      objectives_0 <- outputs[random_0, ] #training data outputs

      test_parameters_0 <- inputs[-random_0, ] #testing data inputs. note that this is defined as all of the values not used for sampling
      test_objectives_0 <- outputs[-random_0, ] #outputs of testing data. this is used when comparing model-predicted values and actual

      #kriging function inner cross referencing
      kriging_0 <- km(formula=~1, design=data.frame(parameters_0), response=data.frame(objectives_0),
                      optim.method='BFGS', upper=c(c1,c2,c3,c4), parinit=c(.05, .05, .05, .05),
                      lower=c(c1,c2,c3,c4))
      predictions <- predict(object=kriging_0, newdata=fuelcomp, type='SK') #function that generates model predictions, using the testing data defined above
      predicted <- data.frame(predictions$mean) #extracting the mean values from the function predict(
      mean_pred[,s] <- predicted[,1]
    }

    x <- data.frame(rowSums(mean_pred))/ncol(mean_pred)
      names(x) <- data[i]
    result[,i] <- x[,1]
}

result <- data.frame(result)
  names(result) <- data
  data.frame(result)
}
