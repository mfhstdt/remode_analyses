######  ASSESSING REMODE ACCURACY FOR DIFFERENT ALPHA CORRECTION METHODS #######

# What happens here? 
# Remode is applied to all 172 distributions in the benchmark, each time 
# using one of 6 potential alpha correction methods (see list below). 
# Accuracies, over- and underestimation rates are assessed

# Overview of alpha correction methods 
#    "1" = alpha / (le - 1), 
#    "2" = alpha / (floor((le + 1) / 2)),     # max modes
#    "3" = 2 * alpha / (0.5 * le * (le - 1)), 
#    "4" = alpha / 3,
#    "5" = alpha,                             # no correction
#    "6" = alpha / sqrt(le)

# Simulation setup ---------#
library(parSim)
source("check_alpha_correction/remode_alpha.R")
load("simulation_study/benchmark_distributions.RData")

# function to apply remode to each distribution with respective alpha & alpha correction
Remode_benchmark <- function(i, benchmark, alpha_correct, alpha){
  # get benchmark distribution i
  dist <- benchmark[[i]]
  # apply remode                   
  remode(table(dist), plot=F, alpha_correction = alpha_correct, alpha = alpha)$nr_of_modes[[1]] 
}



# run simulation ------------#
parSim(
  i = 1:168, # index of benchmark distribution
  alpha_correct = c(1,2,3,4,5,6), # alpha correction method
  alpha = c(0.01, 0.05, 0.1, 0.2, 0.3),
  reps = 50, 
  write = T, 
  export = c("Remode_benchmark","benchmark_distributions","remode","remode_find_maxima", 
             "perform_fisher_test", "perform_binomial_test"),
  name = "Remode_benchmark", # Name of the file
  nCores = 7, # Number of cores to use (check your computer!)
  expression = {
    Remode_benchmark(i, benchmark_distributions, alpha_correct = alpha_correct, alpha = alpha)
  }
)

data <- read.table("Remode_benchmark.txt", header = TRUE)


# check results --------------------------------------------------#
true_modes <- ifelse(grepl("Uni", names(benchmark_distributions)), 1, 
                     ifelse(grepl("Bimodal", names(benchmark_distributions)), 2, 
                            ifelse(grepl("Trimodal", names(benchmark_distributions)), 3, 5)))
overview_benchmark <- data.frame(  # store info on distributions
  name = names(benchmark_distributions),
  i = 1:length(true_modes), 
  n = c(rep(rep(c(50,500,5000,50000), each = 2), 21), 50,500,5000,50000), 
  c = c(rep(rep(c(5, 10), 4), 21), c(39, 28, 38, 42)),
  true_modes = true_modes
)
results <- left_join(data, overview_benchmark, by = "i")


# overall overestimation rate per alpha correction and alpha 
results$false_positive <- results$df > results$true_modes
false_positives <- results %>% 
  group_by(alpha, alpha_correct, n) %>%
  summarise(fpr = round(mean(false_positive), 3)) %>% 
  filter(alpha < .2) %>%
  tidyr::pivot_wider(names_from = alpha_correct,
                     values_from = fpr)


# overall underestimation rate per alpha correction and alpha 
results$false_negative <- results$df < results$true_modes
false_negative <- results %>% 
  group_by(alpha, alpha_correct, n) %>%
  summarise(fnr = round(mean(false_negative), 3)) %>% 
  filter(alpha < .2) %>%
  tidyr::pivot_wider(names_from = alpha_correct,
                     values_from = fnr)


# estimate bias per alpha correction and alpha
biases <- results %>%
  group_by(alpha, alpha_correct, i) %>%
  summarize(mean_mod = mean(df)) # mean estimate over 100 iterations
biases <- left_join(biases, overview_benchmark, by = "i") # get true modality
biases$bias <- biases$mean_mod - biases$true_modes 

biases <- biases %>%
  group_by(alpha, alpha_correct, n , c) %>%
  summarize(mean(bias)) %>%
  filter(alpha < .2) # over-/ underestimation bias 
