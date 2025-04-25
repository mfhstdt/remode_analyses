##### Script to source modality detection functions for simulation studies ####

# methods overview ---
# - Silverman's method (1981); with Hall & York extension (2001)
#      > silverman_it()
# - DIP statistic (Hartigan & Hartigan, 1985)
#     > dip.test()
# - Ameijeiras-Alsonso (2019): bootstrap test of excess mass measure
#     > ACR_it()
# - Gaussian mixture models 
#     > mclustBIC()
# - Bimodality coefficient 
#     > bimodality_coefficient()
# - Density Based Method by Jonas Haslbeck ("DensMM")
#     > DensMMdet()
# - DFU 
#     > dfu()
# - Recursive Mode Detection (Remode)
#    > Remode()

if (!require("silvermantest")) remotes::install_github("jenzopr/silvermantest")
if (!require("diptest")) install.packages("diptest")
if (!require("multimode")) install.packages("multimode")
if (!require("mousetrap")) install.packages("mousetrap") # for BC
if (!require("mclust")) install.packages("mclust")

library(silvermantest)
library(diptest)
library(multimode)
library(mclust)
library(mousetrap)
source("simulation_study/src/DensMMdet.R")
source("simulation_study/src/DFU.R")

# function to apply silverman iteratively 
silverman_it <- function(x){
  # test uni- vs bimodality, adjust p-value as suggested by Hall & York, 2001
  result <- silverman.test(x, 1, adjust = TRUE) 
  # iteratively increase k until no longer significant / more modes found than possible given scale
  k = 1
  while((result@p_value <= 0.01) & (k <= 6)){  
    k = k+1
    result <- silverman.test(x, k)
  }
  return(k)
}

# function to apply ACR method iteratively 
#ACR_it <- function(x){
#  result <- modetest(x)
#  k = 1
#  while(result$p.value <= 0.01){
#    k = k+1
#    result <- modetest(x, mod0 = k, B = 50)
#  }
#  return(k)
#}

