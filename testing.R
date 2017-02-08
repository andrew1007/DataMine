krig_monte_carlo_scheme(test_dat, 20, 5, m, isTraining=FALSE)
k <- optimal_theta_monte_carlo(setup$full_table, 20, 5, 10)

setwd('/home/andrew/Dropbox/RCode/RawData')
data <- read.csv('Freezing.csv')[,2:6]

setup <- data_setup(data[1:28,], 0.85)
input_dat <- setup$input
output_dat <- setup$output
test_dat <- data[29:34,1:5]




# optimal_theta_monte_carlo(setup$full_table, 20, 5, 10)
# c <- KrigOpt(input_dat, test_dat, samplesize_percentage=0.85, sim_count = 1, cross_validate_count = 1)
neuralnet_model(input_dat, output_dat, 3)
