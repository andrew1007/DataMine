#' Hydrocarbon Distribution from the Blending of Distinct Fuels
#'
#'A function that creates a data table with the permutations of possible fuel blends and their net molecular constituents.
#'
#'@param fuels data.frame where rows are molecular components of each fuel and the columns are the distinct fuels to be blended.
#'@param increments The step size between blend ratios. Default is .05 incrimental changes.
#'@param fuelratio A TRUE/FALSE argument. Whether or not to incorporate the percent blends in the final data table.
#'
#'@details fuelComp can handle, at most, 4 distinct fuels, but any number of hydrocarbons(rows). When utilizing the outputs from fuelComp in the prediction process, make sure you set fuelratio to FALSE or you will get an error.
#'@note When utilizing the outputs from fuelComp in the prediction process, make sure you set fuelratio to FALSE or you will get an error.
#'@return A data.frame containing fundamental molecular distributions of various blends
#'@export


fuelComp <- function(fuels ,increments=.05, fuelratio=FALSE){

	if (is.data.frame(fuels)==FALSE){
		print('User input must be in the form of a data.frame')
		stop('Incorrect data type. Not as.data.frame', call=FALSE)
	}

	if (ncol(fuels) > 4){
		print('fuelComp can only handle up to 4 distinct fuel blends!')
		stop('Too many columns/fuels in data.frame', call.=FALSE)
	}

	fuels <- fuels
	fuel_count <- ncol(fuels)
	fullgrid <- data.frame()
	inc <- increments*100

		if (fuel_count==4){
		fullgrid <- data.frame()
			id <- 0:100
			for (i in id){
			grid <- expand.grid(a=0:100, b=0:100, c=0:100, d=i)
			grid100 <- grid[rowSums(grid)==100,]
			grid_inc <- grid100[which(grid100[,1] %% inc==0 & grid100[,2] %% inc==0 & grid100[,3] %% inc==0 & grid100[,4] %% inc==0),]
				names(grid_inc) <- names(fuels)
			grid100_percent <- grid_inc/100
			fullgrid <- rbind(fullgrid, grid100_percent)
				rownames(fullgrid) <- c(1:nrow(fullgrid))
			}
		}

		if (fuel_count==3){
			grid <- expand.grid(a=0:100, b=0:100, c=0:100)
			grid <- data.frame(grid)
			grid100 <- grid[rowSums(grid)==100,]
			grid_inc <- grid100[which(grid100[,1] %% inc==0 & grid100[,2] %% inc==0 & grid100[,3] %% inc==0),]
				names(grid_inc) <- names(fuels)
			fullgrid <- grid_inc/100
				rownames(fullgrid) <- c(1:nrow(fullgrid))
		}

		if(fuel_count==2){
			grid <- expand.grid(a=0:100, b=0:100)
			grid <- data.frame(grid)
			grid100 <- grid[rowSums(grid)==100,]
			grid_inc <- grid100[which(grid100[,1] %% inc==0 & grid100[,2] %% inc==0),]
				names(grid_inc) <- names(fuels)
			fullgrid <- grid_inc/100
				rownames(fullgrid) <- c(1:nrow(fullgrid))
		}

		bind <- NULL
		id <- 1:nrow(fullgrid)
		for (i in id){
			a <- fuels[,1]*fullgrid[i,1]
			b <- fuels[,2]*fullgrid[i,2]
			x <- data.frame(a,b)

				if(fuel_count==3){
					c <- fuels[,2]*fullgrid[i,3]
					x <- data.frame(x, c)
				}

				if(fuel_count==4){
					c <- fuels[,2]*fullgrid[i,3]
					d <- fuels[,2]*fullgrid[i,4]
					x <- data.frame(x, c, d)
				}

			y <- t(rowSums(x))
			bind <- rbind(bind, y)
		}
	blend <- fullgrid
	blends <- data.frame(bind)
		names(blends) <- rownames(fuels)
	output <- blends

	assign("fuel", new.env(hash = TRUE), envir = .GlobalEnv)
	assign("fuelratio", fullgrid, envir = fuel)

	if(fuelratio==TRUE){
		output <- cbind(fullgrid, output)
	}
	data.frame(output)
}
