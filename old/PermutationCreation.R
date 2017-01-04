percentCOMP <- function(incriments=.01){

	inc <- incriments*100
	fullgrid <- data.frame()
	id <- 0:100
	for (i in id){
		grid <- expand.grid(a=0:100, b=0:100, c=0:100, d=i)
		grid <- data.frame(grid)
		grid100 <- grid[rowSums(grid)==100,]
		grid_inc <- grid100[which(grid100[,1] %% inc==0 & grid100[,2] %% inc==0 & grid100[,3] %% inc==0 & grid100[,4] %% inc==0),]
		grid100_percent <- grid_inc/100
		fullgrid <- rbind(fullgrid, grid100_percent)
	}
	fullgrid
}
