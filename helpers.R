library(DiceOptim)
# library(neuralnet)
# library(hydroGOF)
# library(reshape)
library(Metrics)
# library(e1071)
library(data.table)
# library(dplyr)

data_setup <- function(data, samplesize_percentage){
  new_list <- list()
  sample_size <- round(nrow(data) * samplesize_percentage)
  variable_count <- ncol(data)
  new_list[["full_table"]] <- data
  new_list[["input"]] <- input_data(data, as.range(variable_count - 1))
  new_list[["output"]] <- output_data(data, variable_count)
  new_list["sample_size"] <- sample_size
  new_list["test_size"] <- nrow(data) - sample_size
  new_list
}

input_data <- function(data, input_cols){
  data.frame(data[ ,input_cols])
}

output_data <- function(data, output_cols){
  data.frame(data[ ,output_cols])
}

data_sampling_setup <- function(table, sample_size){
  new_list <- list()
  input_range <- as.range(nrow(table))
  sample_rows <- sample.rows(input_range, sample_size)
  column_count <- ncol(table)
  testing_rows <- input_range[-sample_rows]
  input <- data.frame(table[ ,-column_count])
  output <- data.frame(table[ ,column_count])
  new_list[["sample_input"]] <- data.frame(input[sample_rows, ])
  new_list[["sample_output"]] <- data.frame(output[sample_rows, ])
  new_list[["test_input"]] <- data.frame(input[testing_rows, ])
  new_list[["test_output"]] <- data.frame(output[testing_rows, ])
  new_list
}

rbindFast <- function(current_table, new_table){
  rbindlist(list(current_table, new_table))
}

sample.rows <- function(rows, sample_size){
  sample(length(rows), sample_size)
}

root_mean_sq <- function(data){
  rmse(data[ ,1], data[ ,2])
}

as.range <- function(n){
  if (is.data.frame(n)){
    print(TRUE)
    return(1:nrow(n))
  }
  1:n
}

sum_mean <- function(data){
  colnames <- colnames(data)
  output_colnum <- ncol(data)
  input_colnames <- colnames[c(1:length(colnames)-1)]
  dots <- lapply(input_colnames, as.symbol)
  df2 <- data %>% group_by_(.dots=dots) %>% summarise_each(funs(mean), ncol(data))
  data.frame(df2)
}

normalize <- function(data){
  data.frame(scale(data))
}

denormalize <- function(normalized_data, reference_data){
  sd(reference_data) * normalized_data + mean(reference_data)
}

# percentCOMP <- functionme()
# 	id <- 0:100
# 	for (i in id){
# 		grid <- expand.grid(a=0:100, b=0:100, c=0:100, d=i)
# 		grid <- data.frame(grid)
# 		grid100 <- grid[rowSums(grid)==100,]
# 		grid_inc <- grid100[which(grid100[,1] %% inc==0 & grid100[,2] %% inc==0 & grid100[,3] %% inc==0 & grid100[,4] %% inc==0),]
# 		grid100_percent <- grid_inc/100
# 		fullgrid <- rbind(fullgrid, grid100_percent)
# 	}
# 	fullgrid
# }

#
# warnings <- function(ignorewarns == FALSE){
#   if (ignorewarns==FALSE){
#     if (is.character(data)==FALSE){
#       print('data file not characters.')
#       stop('data is not as.character', call.=FALSE)
#     }
#
#     if (length(outputCOL) != 1){
#       print('multiple output columns')
#       stop('too many output columns. expecting 1', call.=FALSE)
#     }
#
#     if (ncol(fuelcomp) != length(inputCOLs)){
#       print('misaligned number of columns')
#       stop('', call.=FALSE)
#     }
#   }
#   return()
# }
