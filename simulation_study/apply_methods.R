# SIMULATION STUDY: APPLY MODALITY DETECTION METHODS TO BENCHMARK DISTRIBUTIONS # 
#################################################################################

# What happens here? ------------------------------------------------------------#
# Script 1: 
# Established and new methods are applied to detect the number of modes in 
# 172 simulated samples of ordinal data. 
# NOTE: some simulations are computationally expensive 
#--------------------------------------------------------------------------------#

# LOAD ALL FUNCTIONS AND SIMULATED DISTRIBUTIONS ---------------------------------#

library(parSim)
library(dplyr)
library(stringr)

# install remode package
# A) either run 
#    Sys.setenv(GITHUB_PAT = "YOUR_TOKEN") # insert PAT token 
#    devtools::install_github("hvdmaas/remode", auth_token = Sys.getenv("GITHUB_PAT"))
# B) or download R project here: https://github.com/hvdmaas/remode
#    open R project and run devtools::load_all()
library(remode)

# load functions for established modality detection methods 
source("simulation_study/src/source_alternative_functions.R")

# load benchmark distribution 
benchmark_distributions <- readRDS("simulation_study/benchmark_distributions.RData")


# APPLY FUNCTIONS TO BENCHMARK DISTRIBUTIONS -------------------------------------#

# keep track of distributions & true modality
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

# Apply silvermans method --------------------------------------#
test_silverman <- function(i, benchmark){
  silverman_it(as.numeric(benchmark[[i]]))
}

parSim(
  i = 1:length(benchmark_distributions), 
  reps = 1, 
  write = T, 
  export = c("silverman.test","silverman_it",
             "benchmark_distributions","test_silverman"),
  name = "benchmark_silverman", 
  nCores = 8, 
  expression = {
    test_silverman(i=i, benchmark=benchmark_distributions)
  }
)

silverman_df <- read.table("benchmark_silverman.txt", 
                           header = TRUE)
silverman_df <- left_join(silverman_df, 
                          overview_benchmark,
                          by="i") %>%
  select(i, name, n, c, true_modes, df) %>%
  rename(modality_est = df) %>%
  mutate(multimod = modality_est > 1, 
         method = "silverman")

save(silverman_df, file = "simulation_study/results/results_silverman.RData")


# Apply dip statistic -----------------------------------------# 
test_dip <- function(i, benchmark){
  x <- as.numeric(benchmark[[i]])
  res <- dip.test(x)
  res$p.value < 0.01
}

parSim(
  i = 1:length(benchmark_distributions), # index of benchmark distribution
  reps = 1,
  write = T, 
  export = c("dip.test","benchmark_distributions","test_dip"),
  name = "benchmark_dip", 
  nCores = 8,
  expression = {
    test_dip(i=i, benchmark=benchmark_distributions)
  }
)

dip_df <- read.table("benchmark_dip.txt", header = TRUE)
dip_df <- left_join(dip_df, overview_benchmark, by = "i") %>%
  select(i, name, n, c, true_modes, df) %>%
  rename(multimod = df) %>%
  mutate(modality_est = NA, 
         method = "dip")

save(dip_df, file = "simulation_study/results/results_dip.RData")


# Ameijeriras-Alonso method for uni- vs. multimodality ------------------------#
test_ACR <- function(i, benchmark){
  x <- as.numeric(benchmark[[i]])
  res <- modetest(as.numeric(x), B = 50, mod0 = 1)
  res$p.value <= 0.01 # return if multimodal
}

parSim(
  i = 1:length(benchmark_distributions), # index of benchmark distribution
  reps = 1,
  write = T, 
  export = c("test_ACR","benchmark_distributions","modetest"),
  name = "benchmark_ACR_unimodality", 
  nCores = 8,
  expression = {
    test_ACR(i=i, benchmark=benchmark_distributions)
  }
)
ACR_df <- read.table("benchmark_ACR_unimodality.txt", header = TRUE)
ACR_df <- left_join(ACR_df, overview_benchmark, by="i") %>%
  rename(multimod = df) %>%
  mutate(modality_est = NA, 
         method = "ACR multimodality") %>%
  select(i, name, n, c, true_modes, multimod, modality_est, method)

save(ACR_df, file = "simulation_study/results/results_ACR_multimod.RData")


# apply Gaussian Mixture Modeling ---------------------#
test_GMM <- function(i, benchmark){
  x <- as.numeric(benchmark[[i]])
  res <- summary(mclustBIC(x))[3]
  as.numeric(str_split(names(res), ",")[[1]][2])
}

parSim(
  i = 1:length(benchmark_distributions), # index of benchmark distribution
  reps = 1, 
  write = T, 
  export = c("mclustBIC","benchmark_distributions","test_GMM","str_split"),
  name = "benchmark_GMM", 
  nCores = 8,
  expression = {
    test_GMM(i=i, benchmark=benchmark_distributions)
  }
)

GMM_df <- read.table("benchmark_GMM.txt", header = TRUE)
GMM_df <- left_join(GMM_df, overview_benchmark, by="i") %>%
  mutate(multimod = df > 1,
         method = "GMM") %>%
  rename(modality_est = df) %>%
  select(i, name, n, c, true_modes, multimod, modality_est, method)

save(GMM_df, file = "simulation_study/results/results_GMM.RData")

# Apply Bimodality coefficient --------------------------# 
test_BC <- function(i, benchmark){
  bimodality_coefficient(as.numeric(benchmark[[i]]))
}

parSim(
  i = 1:length(benchmark_distributions), # index of benchmark distribution
  reps = 1, 
  write = T, 
  export = c("bimodality_coefficient","benchmark_distributions","test_BC"),
  name = "benchmark_BC", 
  nCores = 8,
  expression = {
    test_BC(i=i, benchmark=benchmark_distributions)
  }
)

BC_df <- read.table("benchmark_BC.txt", header = TRUE) 
BC_df <- left_join(BC_df, overview_benchmark, by="i") %>%
  select(i, name, n, c, true_modes, df) %>%
  mutate(multimod = df > (5/9),
         modality_est = NA, 
         method = "BC") %>%
  select(i, name, n, c, true_modes, multimod, modality_est, method)

save(BC_df, file = "simulation_study/results/results_BC.RData")


# Apply DensMM method (Haslbeck et al)--------------------------#
test_densMM <- function(i, benchmark){ # treating claw as contiuous
  if (i < 169)  DensMMdet(as.numeric(benchmark[[i]]))$M else DensMMdet(as.numeric(benchmark[[i]]), categorical = FALSE)$M
}

parSim(
  i = 1:length(benchmark_distributions), # index of benchmark distribution
  reps = 1, 
  write = T, 
  export = c("DensMMdet","benchmark_distributions","test_densMM"),
  name = "benchmark_densMM", 
  nCores = 8,
  expression = {
    test_densMM(i=i, benchmark=benchmark_distributions)
  }
)

DensMM_df <- read.table("benchmark_densMM.txt", header = TRUE)
DensMM_df <- left_join(DensMM_df, overview_benchmark, by="i") %>%
  select(i, name, n, c, true_modes, df) %>%
  mutate(multimod = df > 1, 
         method = "DensMM") %>%
  rename(modality_est = df) %>%
  select(i, name, n, c, true_modes, multimod, modality_est, method)


save(DensMM_df, file = "simulation_study/results/results_DensMM.RData")


# Apply DFU -------------------------------------------------#
test_dfu <- function(i, benchmark){
  dfu(as.numeric(benchmark[[i]]))
}

parSim(
  i = 1:length(benchmark_distributions), # index of benchmark distribution
  reps = 1, 
  write = T, 
  export = c("dfu","benchmark_distributions","test_dfu"),
  name = "benchmark_dfu", 
  nCores = 8,
  expression = {
    test_dfu(i=i, benchmark=benchmark_distributions)
  }
)

DFU_df <- read.table("benchmark_dfu.txt", header = TRUE)
DFU_df <- left_join(DFU_df, overview_benchmark, by="i") %>%
  select(i, name, n, c, true_modes, df) %>%
  mutate(multimod = df > 0,
         modality_est = NA,
         method = "DFU") %>%
  select(i, name, n, c, true_modes, multimod, modality_est, method)

save(DFU_df, file = "alternative_methods/results_DFU.RData")


# Apply Remode-----------------------------------------------------------------#
test_Remode <- function(i, benchmark){
  remode(table(benchmark[i]), alpha_correction = "max_modes")$nr_of_modes[1] 
}

parSim(
  i = c(1:length(benchmark_distributions)), # index of benchmark distribution
  reps = 1, 
  write = T, 
  export = c("test_Remode","benchmark_distributions","remode","remode_find_maxima", "perform_binomial_test"),
  name = "benchmark_Remode", 
  nCores = 8,
  expression = {
    test_Remode(i=i, benchmark=benchmark_distributions)
  }
)
Remode_df <- read.table("benchmark_Remode.txt", header = TRUE)
Remode_df <- left_join(Remode_df, overview_benchmark, by="i") %>%
  select(i, name, n, c, true_modes, df) %>%
  rename(modality_est = df) %>%
  mutate(multimod = modality_est > 1,
         method = "Remode") %>%
  select(i, name, n, c, true_modes, multimod, modality_est, method)

save(Remode_df, file = "simulation_study/results/results_Remode.RData")


# For accuracy analysis of results, see Script 2: simulation_study/analyse_accuracies.R
