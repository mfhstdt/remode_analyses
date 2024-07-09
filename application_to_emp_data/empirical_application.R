################# APPLICATION OF REMODE TO EMPIRICAL DATASETS ###################

library(dplyr)
library(openxlsx)

# Overview of datasets: 
# - LISS Panel (wave 14; Centerdata, 2021)
# - Combined uropean Values Study and World Values Survey 2017 - 2022
# - Eurobarometer (European Commission, 2022)
# - Survey on attitudes towards COVID-19 vaccines (Chambon et al., 2022); time point 1
# - Data on coma incidences from Di Saverio et al., 2013
# - Global U-Pb database from 2019, as described by Puetz et al., 2024


# LOAD REMODE PACKAGE ------------------------------------------------#
# load remode package
# A) either run devtools::install_github("hvdmaas/remode")
# B) or download R project here: https://github.com/hvdmaas/remode
#    open R project and run 
#    devtools::load_all()

# LOAD DATA ------------------------------------------------------------#
load("application_to_emp_data/open_datasets/liss_attitude.rda") # liss panel

load("application_to_emp_data/open_datasets/ewvs_attitude.rda") # EVS/ WVS

load("application_to_emp_data/open_datasets/euro_attitude.rda") # Eurobarometer

load("application_to_emp_data/open_datasets/covid_attitude.rda") # COVID-19 vaccination

# take medical data from Di Saverio et al., 2013
cm=c(0,0,1.9,.75,.75,1.2,1.4,.85,.6,.62,.65,.9,1.15,1.85,7.05) # height of bars in figure 5
cm=cm/sum(cm)
gcs_data=cm*2935 # multiply by N
gcs_data=round(gcs_data)

# read in Global U-Pb data from (sheet 3) file 41597_2023_2902_MOESM1_ESM.xlsx
# download at https://www.nature.com/articles/s41597-023-02902-9
MOESM1 <- openxlsx::read.xlsx("41597_2023_2902_MOESM1_ESM.xlsx", sheet = 3)
UPb_data <- MOESM1$`Non-Iter..Probability.age.(Ma)`


# APPLY REMODE -------------------------------------------------------#
liss <- remode(table(liss_data), alpha_correction = "max_modes")
liss

ewvs <- remode(table(ewvs_data$attitude), alpha_correction = "max_modes")
ewvs

euro <- remode(table(euro_data$attitude), alpha_correction = "max_modes")
euro

covid <- remode(table(covid_data$mean_attitude), alpha_correction = "max_modes")
covid

gcs <- remode(gcs_data, alpha_correction = "max_modes")
gcs

upb <- remode(hist(UPb_data,breaks=seq(0,max(UPb_data)+10,by=10),plot=FALSE)$counts, 
              alpha_correction = "max_modes")
upb


# PLOT RESULTS AND STABILITY -------------------------------------------------------#

par(mfrow=c(6,2))
barplot(liss, col = "#ED6A5A", main = "2 modes in LISS Panel (N = 4,187)", cex.main = 2,
        yaxt = "n", ylab = "", density = c(rep(20, 5), 70, 20, 70, 20, 20, 20))
remode_stability(liss, percentage_steps = 100, plot = TRUE)

barplot(ewvs, col = "#ED6A5A", main = "3 modes in EVS/WVS (N = 109,963)", cex.main = 2,
        yaxt = "n", ylab = "", density = c(70, rep(20, 3), 70, rep(20,4), 70))
remode_stability(ewvs, plot = TRUE, percentage_steps = 100)

barplot(euro, col = "#ED6A5A", main="1 mode in Eurobarometer (N = 26,187)", cex.main = 2,
        yaxt = "n", ylab = "", density = c(rep(20, 3), 70, 20))
remode_stability(euro, plot = TRUE, percentage_steps = 100)

barplot(covid, col = "#ED6A5A", main = "2 modes in Attitude towards COVID vaccines (N = 1,501)", cex.main = 2,
        yaxt = "n", ylab = "", density = c(100, rep(20, 45), 100, rep(20, 20)))
remode_stability(liss, percentage_steps = 100, plot = TRUE)

barplot(gcs, col = "#ED6A5A", main = "3 modes in GCS scores (N = 2,935)", cex.main = 2,
        yaxt = "n", ylab = "", density = c(rep(20,2), 100, rep(20, 3), 100, rep(20,7), 100))
remode_stability(gcs, percentage_steps = 100, plot = TRUE)

# plot for continuous UPb data with bin size of 10 Ma (as in Puetz et al., 2024)
density <- rep(0,  length(upb$xt))
density[upb$modes] <- 100
col <- rep("lightgrey",  length(upb$xt))
col[upb$modes] <- "#ED6A5A"
mp=barplot(upb$xt, border='lightgrey',col = col,xlab=names(MOESM1)[29],
           main= "20 modes in U-Pb in detrital zircon (N = 589,910)" )
at_positions <- seq(20, 420, by = 20)
labels <- as.character(at_positions*10)
axis(1, at = mp[at_positions], labels = labels,cex=.6)
remode_robustness(upb, percentage_steps = 100, plot = TRUE)


# FOR UPB DATA: FIND STABILITY PER MODE LOCATION -------------------------------------#
stability_Upb <- remode_robustness(upb, percentage_steps = 20, plot = F, iterations=50)
stability_Upb

