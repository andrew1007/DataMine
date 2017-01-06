library(DiceOptim)
library(neuralnet)
library(hydroGOF)
library(reshape)
library(Metrics)
library(e1071)
setwd('/home/andrew/Dropbox/R_files/RawData')

csv_data_setup <- function(data_csv, input_cols, output_cols, samplesize_percentage){
  new_list <- list()
  raw_data <- read.csv(data_csv)
  sample_size <- round(nrow(raw_data) * samplesize_percentage)
  new_list[["input"]] <- input_data(raw_data, input_cols)
  new_list[["output"]] <- output_data(raw_data, output_cols)
  new_list["sample_size"] <- sample_size
  new_list["test_size"] <- nrow(raw_data) - sample_size
  new_list
}
x <- csv_data_setup('Freezing.csv', 2:5, 6, 0.85)

input_data <- function(raw_data, input_cols){
  data.frame(raw_data[ ,input_cols])
}

output_data <- function(raw_data, output_cols){
  data.frame(raw_data[ ,output_cols])
}

data_sampling_row_setup <- function(input, output, sample_size){
  new_list <- list()
  input_range <- as.range(input)
  sample_rows <- sample.rows(input_range, sample_size)
  testing_rows <- input_range[-sample_rows]
  input <- data.frame(input)
  output <- data.frame(output)
  new_list["sample_input"] <- data.frame(input[sample_rows, ])
  new_list["sample_output"] <- data.frame(output[sample_rows, ])
  new_list["test_input"] <- data.frame(input[testing_rows, ])
  new_list["test_output"] <- data.frame(output[testing_rows, ])
  new_list
}

insert_to_preallocated_matrix <- function(table, data, i, test_size){
  start_range <- (i - 1) * test_size + 1
  end_range <- i * test_size
  table[(start_range:end_range), 1] <- experimental_v_predicted[ ,1]
  table[(start_range:end_range), 2] <- experimental_v_predicted[ ,2]
  table
}

preallocated_matrix <- function(ncol, nrow){
  matrix(data = NA, ncol = ncol, nrow)
}

sample.rows <- function(rows, sample_size){
  sample(length(rows), sample_size)
}

root_mean_sq <- function(data){
  rmse(data[ ,1], data[ ,2])
}

as.range <- function(n){
  if (is.data.frame(n)){
    return(1:nrow(data.frame(n)))
  }
  1:n
}

sum_mean <- function(data, input_cols){
  uniq_inputs <- unique(data[ ,input_cols])
  final_table <- NULL
  for(i in row.range(uniq_inputs)){
    select_rows <- select_data_by(data, uniq_inputs[i, ])
    average <- mean(as.vector(select_rows[,ncol(select_rows)]))
    datum_set <- data.frame(uniq_inputs[i,], average)
    final_table <- rbind(final_table, datum_set)
  }
  final_table
}

select_data_by <- function(data, where_condition){
  new_table <- NULL
  for (j in 1:nrow(data)){
    new_row <- data[j,-ncol(data)]
    if (all(new_row == where_condition)){
      new_table <- rbind(new_table, data[j,])
    }
  }
  new_table
}

normalize <- function(data){
  data.frame(scale(data))
}

denormalize <- function(normalized_data, reference_data){
  sd(reference_data) * normalized_data + mean(reference_data)
}

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
