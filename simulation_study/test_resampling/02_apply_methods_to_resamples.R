# TEST REPEATED APPLICATION OF METHODS TO RESAMPLED DATA SAMPLES OF BENCHMARK #

library(parSim)
library(dplyr)
library(stringr)

# Step 2: Apply modality detection methods to all resamples of the chosen benchmark cases

# load remode package
# A) either run 
#    Sys.setenv(GITHUB_PAT = "YOUR_TOKEN") # insert PAT token 
#    devtools::install_github("hvdmaas/remode", auth_token = Sys.getenv("GITHUB_PAT"))
# B) or download R project here: https://github.com/hvdmaas/remode
#    open R project and run devtools::load_all()
library(remode)
source("simulation_study/src/source_alternative_functions.R")

resampled_distributions <- readRDS("simulation_study/test_resampling/resampled_distributions.RData")


# keep track of distributions & true modality
resampled_results <- data.frame(
  i = 1:length(resampled_distributions),
  name = names(resampled_distributions),
  iteration = rep(1:10, times = length(resampled_distributions)) 
)

resampled_results$true_modes <- ifelse(grepl("Uni", resampled_results$name), 1, 
                                       ifelse(grepl("Bimodal", resampled_results$name), 2, 
                                              ifelse(grepl("Trimodal", resampled_results$name), 3, 5)))


# Apply silvermans method --------------------------------------#
test_silverman <- function(i, benchmark){
  silverman_it(as.numeric(benchmark[[i]]))
}

parSim(
  i = 1:length(resampled_distributions), # index of benchmark distribution
  reps = 1, 
  write = T, 
  export = c("silverman.test","silverman_it",
             "resampled_distributions","test_silverman"),
  name = "resampled_silverman", 
  nCores = 8, 
  expression = {
    test_silverman(i=i, benchmark=resampled_distributions)
  }
)

silverman_df <- read.table("resampled_silverman.txt", 
                           header = TRUE)
silverman_df <- left_join(silverman_df, 
                          resampled_results,
                          by="i") %>%
  rename(modality_est = df) %>%
  select(i, name, true_modes, modality_est) %>%
  mutate(multimod = modality_est > 1, 
         method = "silverman")

save(silverman_df, file = "simulation_study/test_resampling/results_silverman_resampled.RData")


# Apply dip statistic -----------------------------------------# 
test_dip <- function(i, benchmark){
  x <- as.numeric(benchmark[[i]])
  res <- dip.test(x)
  res$p.value < 0.01
}

parSim(
  i = 1:length(resampled_distributions), 
  reps = 1,
  write = T, 
  export = c("dip.test","resampled_distributions","test_dip"),
  name = "benchmark_dip", 
  nCores = 8,
  expression = {
    test_dip(i=i, benchmark=resampled_distributions)
  }
)

dip_df <- read.table("benchmark_dip.txt", header = TRUE)
dip_df <- left_join(dip_df, resampled_results, by = "i") %>%
  rename(multimod = df) %>%
  select(i, name, true_modes, multimod) %>%
  mutate(modality_est = NA, 
         method = "dip")

save(dip_df, file = "simulation_study/test_resampling/results_dip_resampled.RData")


# Ameijeriras-Alonso method for uni- vs. multimodality ------------------------#
# NOTE: error: cannot allocate vectors of such size
test_ACR <- function(i, benchmark){
  x <- as.numeric(benchmark[[i]])
  r <- modetest(as.numeric(x), B = 50, mod0 = 1)
  r$p.value <= 0.01 # return if multimodal
} 

parSim(
  i = 1:length(resampled_distributions),  
  reps = 1,
  write = T, 
  export = c("test_ACR","resampled_distributions","modetest"),
  name = "benchmark_ACR_unimodality", 
  nCores = 8,
  expression = {
    test_ACR(i=i, benchmark=resampled_distributions)
  }
)

ACR_df <- read.table("benchmark_ACR_unimodality.txt", header = TRUE)
ACR_df <- left_join(ACR_df, resampled_results, by="i") %>%
  rename(multimod = df) %>%
  select(i, name, true_modes, multimod) %>%
  mutate(modality_est = NA, 
         method = "ACR multimodality")

save(ACR_df, file = "simulation_study/test_resampling/results_ACR_multimod_resampled.RData")


# apply Gaussian Mixture Modeling ---------------------#
test_GMM <- function(i, benchmark){
  x <- as.numeric(benchmark[[i]])
  res <- summary(mclustBIC(x))[3]
  as.numeric(str_split(names(res), ",")[[1]][2])
}

parSim(
  i = 1:length(resampled_distributions), 
  reps = 1, 
  write = T, 
  export = c("mclustBIC","resampled_distributions","test_GMM","str_split"),
  name = "benchmark_GMM", 
  nCores = 8,
  expression = {
    test_GMM(i=i, benchmark=resampled_distributions)
  }
)

GMM_df <- read.table("benchmark_GMM.txt", header = TRUE) # NOTE: all errors
GMM_df <- left_join(GMM_df, resampled_results, by="i") %>%
  rename(modality_est = df) %>%
  mutate(multimod = modality_est > 1,
         method = "GMM") %>%
  select(i, name,true_modes, multimod, modality_est, method)

save(GMM_df, file = "simulation_study/test_resampling/results_GMM_resampled.RData")

# Apply Bimodality coefficient --------------------------# 
test_BC <- function(i, benchmark){
  bimodality_coefficient(as.numeric(benchmark[[i]]))
}

parSim(
  i = 1:length(resampled_distributions), 
  reps = 1, 
  write = T, 
  export = c("bimodality_coefficient","resampled_distributions","test_BC"),
  name = "benchmark_BC", 
  nCores = 8,
  expression = {
    test_BC(i=i, benchmark=resampled_distributions)
  }
)

BC_df <- read.table("benchmark_BC.txt", header = TRUE) 
BC_df <- left_join(BC_df, resampled_results, by="i") %>%
  mutate(multimod = df > (5/9),
         modality_est = NA, 
         method = "BC") %>%
  select(i, name, true_modes, multimod, modality_est, method)

save(BC_df, file = "simulation_study/test_resampling/results_BC_resampled.RData")


# Apply DensMM method (Haslbeck et al)--------------------------#
test_densMM <- function(i, benchmark){ # treating claw as continuous
  if (i < 211)  DensMMdet(as.numeric(benchmark[[i]]))$M else DensMMdet(as.numeric(benchmark[[i]]), categorical = FALSE)$M
}

parSim(
  i = 1:length(resampled_distributions), 
  reps = 1, 
  write = T, 
  export = c("DensMMdet","resampled_distributions","test_densMM"),
  name = "benchmark_densMM", 
  nCores = 8,
  expression = {
    test_densMM(i=i, benchmark=resampled_distributions)
  }
)

DensMM_df <- read.table("benchmark_densMM.txt", header = TRUE)
DensMM_df <- left_join(DensMM_df, resampled_results, by="i") %>%
  rename(modality_est = df) %>%
  mutate(multimod = modality_est > 1, 
         method = "DensMM") %>%
  select(i, name, true_modes, multimod, modality_est, method)


save(DensMM_df, file = "simulation_study/test_resampling/results_DensMM_resampled.RData")


# Apply DFU -------------------------------------------------#
test_dfu <- function(i, benchmark){
  dfu(as.numeric(benchmark[[i]]))
}

parSim(
  i = 1:length(resampled_distributions), 
  reps = 1, 
  write = T, 
  export = c("dfu","resampled_distributions","test_dfu"),
  name = "benchmark_dfu", 
  nCores = 8,
  expression = {
    test_dfu(i=i, benchmark=resampled_distributions)
  }
)

DFU_df <- read.table("benchmark_dfu.txt", header = TRUE)
DFU_df <- left_join(DFU_df, resampled_results, by="i") %>%
  mutate(multimod = df > 0,
         modality_est = NA,
         method = "DFU") %>%
  select(i, name, true_modes, multimod, modality_est, method)

save(DFU_df, file = "simulation_study/test_resampling/results_DFU_resampled.RData")


# Apply Remode-----------------------------------------------------------------#
test_Remode <- function(i, benchmark){
  remode(table(benchmark[i]), alpha_correction = "max_modes")$nr_of_modes[1] 
}

start_time_remode = Sys.time()

parSim(
  i = 1:length(resampled_distributions), 
  reps = 1, 
  write = T, 
  export = c("test_Remode","resampled_distributions","remode"),
  name = "benchmark_Remode", 
  nCores = 8,
  expression = {
    test_Remode(i=i, benchmark=resampled_distributions)
  }
)


Remode_df <- read.table("benchmark_Remode.txt", header = TRUE)
Remode_df <- left_join(Remode_df, resampled_results, by="i") %>%
  rename(modality_est = df) %>%
  mutate(multimod = modality_est > 1,
         method = "Remode") %>%
  select(i, name, true_modes, multimod, modality_est, method)

save(Remode_df, file = "simulation_study/test_resampling/results_Remode_resampled.RData")
