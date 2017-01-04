#' Kriging Optimization
#'
#'A Monte Carlo optimization scheme which utilizes DiceOptim to create simulations and find the optimal predictive model.
#'
#'@param inputCOLs Input values.
#'@param outputCOL Objective values.
#'@param simCOUNT Number of simulations to perform.
#'@param crossREF Number of times to cross reference each model.
#'@param samplesize The number of data points used to train the Kriging model. Unused points go to testing.
#'@param thetaUpper Upper bound of thetas values allowed.
#'@param thetaLower Lower bound of thetas values allowed.
#'@return A list containing $predicted $experimental $RMSE $thetas
#'@note Two global variables are generated: Kinputs and Koutputs.
#'@note krigSim has preliminary error checks, which are as follows:
#'@note You set your samplesize= higher than the amount of data you have.
#'@note Your inputs and outputs must be in the form of data.frame().
#'@note The functions in DataMine can not handle more than one output at a time.
#'@export


krigSim <- function(inputCOLs=data[,2:5], outputCOL=data[,6], simCOUNT=1000, crossREF=1000, samplesize=4, thetaUpper= c(5, 5, 5, 5), thetaLower= c(0.01, 0.01, 0.01, 0.01, ignorewarns=FALSE)){
  library(hydroGOF)
  library(DiceOptim)

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
        print('The functions in DataMine can not handle more than one output at a time!')
        stop('More than one column in outputCOL', call.=FALSE)
      }

  }


  Kinputs <<- inputCOLs #columns with input data
    inputs <- Kinputs
  Koutputs <<- data.frame(outputCOL) #columns with output data
    outputs <- Koutputs

  rows <- nrow(inputs) #counting the amount of training data (aka rows in data table)
  id <- 1:simCOUNT #defining the loop with number of simulations to run
  for (i in id){ #loop
    random <- sample(1:rows, samplesize) #randomly selecting data from the data set for sampling/training
    #note that 'random' is simply an RNG, which is then used as to which rows of data are used as training data
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


    #this subloop, fd, takes the theta values you found from the i (above), with the theta held constant
    #resimCOUNT your Kriging model repeatedly (cross referenced)
    table <- NULL
    fd <- 1:crossREF #amount of cross referencing to be done. Note that the theta values are held static, and now an RNG is used again to sample random data
    for(f in fd){
      random_0 <- sample(1:rows, samplesize) #RNG random numbers to be used as training data
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
      table <- rbind(table,PvO) #rbinding all cross referenced values into a single table from loop fd
    }
    data <- table #renaming the data table


    rmse <- data #simple renaming for keeping track of stuff later on
    rootmean <- rmse(data[,1], data[,2]) #RMSE

    stats <- cbind(collective_weights, 0) #this writes the thetas/weights/weights for Kriging/ANN/SVM, respectively as a 2 column table, with the values on [,1] and all zeros on [,2]
    stats[1,2] <- rootmean #inserting the calculated RMSE from the RMSE CALCULATION from above
    colnames(stats) <- c('experimental', 'predicted') #column naming
    colnames(data) <- c('experimental', 'predicted') #column naming
    overall_file <- rbind(stats, data) #combining the weights, statistical data, and the predicted vs. experimental values all into 2 columns


    if (i==1){ #so this is a little complicated, I guess. what happens is the first run, I will relabel overall_file to overall
      overall <- overall_file #the data table "overall" is the eventual one that I will have when finished
    }
    if (overall_file[1,2] < overall[1,2]){ #every loop after that, I compare what I wrote as "overall" to the new simulation, "overall_file"
      overall <- overall_file #UNDER THE CONDITION that the RMSE of the new file is lower
    }							#So what happens is the file is written, then keeps getting rewritten when a lower RMSE simulation occurs
  }
  data1 <- overall[-(1:4),]
  rootmean <- overall[1,2]
  names(rootmean) <- 'RMSE'
  thetas <- data.frame(overall[1:4,1])
  names(thetas) <- 'thetas'
  c(data1, rootmean, thetas)
}
