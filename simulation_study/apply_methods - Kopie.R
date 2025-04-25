# TEST RUN SIMULATION FOR LARGE N AND MEASURE TIME ----------


# LOAD ALL FUNCTIONS AND SIMULATED DISTRIBUTIONS

library(parSim)
library(dplyr)
library(stringr)

# load remode package
# A) either run 
#    Sys.setenv(GITHUB_PAT = "YOUR_TOKEN") # insert PAT token 
#    devtools::install_github("hvdmaas/remode", auth_token = Sys.getenv("GITHUB_PAT"))
# B) or download R project here: https://github.com/hvdmaas/remode
#    open R project and run devtools::load_all()
library(remode)

# load functions for established modality detection methods 
source("simulation_study/source_alternative_functions.R")

# load benchmark distribution 
benchmark_distributions <- readRDS("simulation_study/benchmark_distributions.RData")


# APPLY FUNCTIONS TO BENCHMARK DISTRIBUTIONS -------------------------------------#

# retrieve distributions with c=10 and N=50000
matching_names <- c(grep("N_50000_C_10", names(benchmark_distributions), value = TRUE),
                    "Claw_N_50000")
benchmark_distributions <- benchmark_distributions[matching_names]


# keep track of distributions & true modality
true_modes <- ifelse(grepl("Uni", names(benchmark_distributions)), 1, 
                     ifelse(grepl("Bimodal", names(benchmark_distributions)), 2, 
                            ifelse(grepl("Trimodal", names(benchmark_distributions)), 3, 5)))
overview_benchmark <- data.frame(  # store info on distributions & time var
  name = names(benchmark_distributions),
  i = 1:length(true_modes),
  true_modes = true_modes
)


# Apply silvermans method --------------------------------------#
test_silverman <- function(i, benchmark){
  silverman_it(as.numeric(benchmark[[i]]))
}


parSim(
  i = 1:22, # index of benchmark distribution
  reps = 1, 
  write = T, 
  export = c("silverman.test","silverman_it",
             "benchmark_distributions","test_silverman"),
  name = "benchmark_silverman", 
  nCores = 8, 
  expression = {
    start_time = Sys.time()
    modality_est <- test_silverman(i=i, benchmark=benchmark_distributions)
    end_time = Sys.time()
    time_taken = round(as.numeric(end_time - start_time, units = "secs"),2)
    
    list(modality_est = modality_est, time_taken = time_taken)
  }
)

silverman_df <- read.table("benchmark_silverman.txt", 
                           header = TRUE)
silverman_df <- left_join(silverman_df, 
                          overview_benchmark,
                          by="i") %>%
  select(i, name, true_modes, modality_est, time_taken) %>%
  mutate(multimod = modality_est > 1, 
         method = "silverman")

save(silverman_df, file = "simulation_study/results/results_silverman_time.RData")


# Apply dip statistic -----------------------------------------# 
test_dip <- function(i, benchmark){
  x <- as.numeric(benchmark[[i]])
  res <- dip.test(x)
  res$p.value < 0.01
}

parSim(
  i = 1:22, # index of benchmark distribution
  reps = 1,
  write = T, 
  export = c("dip.test","benchmark_distributions","test_dip"),
  name = "benchmark_dip", 
  nCores = 8,
  expression = {
    start_time = Sys.time()
    multimod = test_dip(i=i, benchmark=benchmark_distributions)
    end_time = Sys.time()
    time_taken = round(as.numeric(end_time - start_time, units = "secs"),2)
    
    list(multimod = multimod, time_taken = time_taken)
  }
)

dip_df <- read.table("benchmark_dip.txt", header = TRUE)
dip_df <- left_join(dip_df, overview_benchmark, by = "i") %>%
  select(i, name, true_modes, multimod, time_taken) %>%
  mutate(modality_est = NA, 
         method = "dip")

save(dip_df, file = "simulation_study/results/results_dip_time.RData")


# Ameijeriras-Alonso method for uni- vs. multimodality ------------------------#
# NOTE: error: cannot allocate vectors of such size
test_ACR <- function(i, benchmark){
  x <- as.numeric(benchmark[[i]])
  res <- modetest(as.numeric(x), B = 50, mod0 = 1, gridsize = c(2,2))
  res$p.value <= 0.01 # return if multimodal
} 

parSim(
  i = 1:22, # index of benchmark distribution
  reps = 1,
  write = T, 
  export = c("test_ACR","benchmark_distributions","modetest"),
  name = "benchmark_ACR_unimodality", 
  nCores = 8,
  expression = {
    start_trime = Sys.time()
    multimod = test_ACR(i=i, benchmark=benchmark_distributions)
    end_time = Sys.time()
    time_taken = round(as.numeric(end_time - start_time, units = "secs"),2)
    
    list(multimod = multimod, time_taken = time_taken)
  }
)
ACR_df <- read.table("benchmark_ACR_unimodality.txt", header = TRUE)
ACR_df <- left_join(ACR_df, overview_benchmark, by="i") %>%
  select(i, name, true_modes, multimod, time_taken) %>%
  mutate(modality_est = NA, 
         method = "ACR multimodality")

save(ACR_df, file = "simulation_study/results/results_ACR_multimod_time.RData")


# apply Gaussian Mixture Modeling ---------------------#
test_GMM <- function(i, benchmark){
  x <- as.numeric(benchmark[[i]])
  res <- summary(mclustBIC(x))[3]
  as.numeric(str_split(names(res), ",")[[1]][2])
}

parSim(
  i = 1:22, # index of benchmark distribution
  reps = 1, 
  write = T, 
  export = c("mclustBIC","benchmark_distributions","test_GMM","str_split"),
  name = "benchmark_GMM", 
  nCores = 8,
  expression = {
    start_trime = Sys.time()
    modality_est = test_GMM(i=i, benchmark=benchmark_distributions)
    end_time = Sys.time()
    time_taken = round(as.numeric(end_time - start_time, units = "secs"),2)
    
    list(modality_est = modality_est, time_taken = time_taken)
  }
)

GMM_df <- read.table("benchmark_GMM.txt", header = TRUE) # NOTE: all errors
GMM_df <- left_join(GMM_df, overview_benchmark, by="i") %>%
  mutate(multimod = modality_est > 1,
         method = "GMM") %>%
  select(i, name,true_modes, multimod, modality_est, method)

save(GMM_df, file = "simulation_study/results/results_GMM_time.RData")

# Apply Bimodality coefficient --------------------------# 
test_BC <- function(i, benchmark){
  bimodality_coefficient(as.numeric(benchmark[[i]]))
}

parSim(
  i = 1:22, # index of benchmark distribution
  reps = 1, 
  write = T, 
  export = c("bimodality_coefficient","benchmark_distributions","test_BC"),
  name = "benchmark_BC", 
  nCores = 8,
  expression = {
    start_time = Sys.time()
    bc = test_BC(i=i, benchmark=benchmark_distributions)
    end_time = Sys.time()
    time_taken = round(as.numeric(end_time - start_time, units = "secs"),2)
    
    list(bc = bc, time_taken = time_taken)
  }
)

BC_df <- read.table("benchmark_BC.txt", header = TRUE) 
BC_df <- left_join(BC_df, overview_benchmark, by="i") %>%
  mutate(multimod = bc > (5/9),
         modality_est = NA, 
         method = "BC") %>%
  select(i, name, true_modes, multimod, modality_est, method, time_taken)

save(BC_df, file = "simulation_study/results/results_BC_time.RData")


# Apply DensMM method (Haslbeck et al)--------------------------#
test_densMM <- function(i, benchmark){ # treating claw as continuous
  if (i < 21)  DensMMdet(as.numeric(benchmark[[i]]))$M else DensMMdet(as.numeric(benchmark[[i]]), categorical = FALSE)$M
}

parSim(
  i = 1:22, # index of benchmark distribution
  reps = 1, 
  write = T, 
  export = c("DensMMdet","benchmark_distributions","test_densMM"),
  name = "benchmark_densMM", 
  nCores = 8,
  expression = {
    start_time = Sys.time()
    modality_est = test_densMM(i=i, benchmark=benchmark_distributions)
    end_time = Sys.time()
    time_taken = round(as.numeric(end_time - start_time, units = "secs"),2)
    
    list(modality_est = modality_est, time_taken = time_taken)
  }
)

DensMM_df <- read.table("benchmark_densMM.txt", header = TRUE)
DensMM_df <- left_join(DensMM_df, overview_benchmark, by="i") %>%
  mutate(multimod = modality_est > 1, 
         method = "DensMM") %>%
  select(i, name, true_modes, multimod, modality_est, method, time_taken)


save(DensMM_df, file = "simulation_study/results/results_DensMM_time.RData")


# Apply DFU -------------------------------------------------#
test_dfu <- function(i, benchmark){
  dfu(as.numeric(benchmark[[i]]))
}

parSim(
  i = 1:22, # index of benchmark distribution
  reps = 1, 
  write = T, 
  export = c("dfu","benchmark_distributions","test_dfu"),
  name = "benchmark_dfu", 
  nCores = 8,
  expression = {
    start_time = Sys.time()
    dfu = test_dfu(i=i, benchmark=benchmark_distributions)
    end_time = Sys.time()
    time_taken = round(as.numeric(end_time - start_time, units = "secs"),2)
    
    list(dfu = dfu, time_taken = time_taken)
  }
)

DFU_df <- read.table("benchmark_dfu.txt", header = TRUE)
DFU_df <- left_join(DFU_df, overview_benchmark, by="i") %>%
  mutate(multimod = dfu > 0,
         modality_est = NA,
         method = "DFU") %>%
  select(i, name, true_modes, multimod, modality_est, method, time_taken)

save(DFU_df, file = "simulation_study/results/results_DFU_time.RData")


# Apply Remode-----------------------------------------------------------------#
test_Remode <- function(i, benchmark){
  remode(table(benchmark[i]), alpha_correction = "max_modes")$nr_of_modes[1] 
}

parSim(
  i = c(1:22), # index of benchmark distribution
  reps = 1, 
  write = T, 
  export = c("test_Remode","benchmark_distributions","remode"),
  name = "benchmark_Remode", 
  nCores = 8,
  expression = {
    start_time = Sys.time()
    modality_est = test_Remode(i=i, benchmark=benchmark_distributions)
    end_time = Sys.time()
    time_taken = round(as.numeric(end_time - start_time, units = "secs"),2)
    
    list(modality_est = modality_est, time_taken = time_taken)
  }
)

Remode_df <- read.table("benchmark_Remode.txt", header = TRUE)
Remode_df <- left_join(Remode_df, overview_benchmark, by="i") %>%
  mutate(multimod = modality_est > 1,
         method = "Remode") %>%
  select(i, name, true_modes, multimod, modality_est, method, time_taken)

save(Remode_df, file = "simulation_study/results/results_Remode_time.RData")


# ANALYZE AVERAGE TIME PER METHOD FOR N=50000: -------------------
print(paste("Silverman:",round(mean(silverman_df$time_taken),2), "secs"))  #43.95
print(paste("Dens MM:",round(mean(DensMM_df$time_taken),2), "secs")) # 0.1
print(paste("Remode:",round(mean(Remode_df$time_taken),2), "secs")) # 0.09
print(paste("DFU:",round(mean(DFU_df$time_taken),2), "secs")) # 0.08
print(paste("Dip:",round(mean(dip_df$time_taken),2), "secs")) # 0.02
print(paste("BC:",round(mean(BC_df$time_taken),2), "secs")) # 0.05

