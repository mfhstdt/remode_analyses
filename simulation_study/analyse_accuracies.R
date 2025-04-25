#       SIMULATION STUDY: COMPUTE ACCURACY PER MODALITY DETECTION METHOD        # 
#################################################################################

# What happens here? ------------------------------------------------------------#
# Script 2: 
# Compare detected modality with true modality to compute accuracy per method
# This is done for A) discriminating uni- from multimodality for all methods 
# and B) in detecting the number of modes, where applicable 
# C) Plots results (replicating Fig. 3 and Fig. 4 from ReMoDe paper)
#--------------------------------------------------------------------------------#

library(dplyr)
library(stringr)
library(ggplot2)
library(dplyr)
library(patchwork)

# load all results from script 2
rdata_files <- list.files(path = "simulation_study/results", pattern = "\\.RData$", full.names = TRUE)
for (file in rdata_files) {load(file)}

# merge all results 
results_df <- rbind(silverman, dip, ACR, GMM, BC, DensMM, dfu, remode)

# A) accuracy in discriminating uni- from multimodality -------------------------#
acc_multimod <- results_df %>%
  mutate(correct = ifelse((true_modes > 1 & multimod == TRUE | true_modes == 1 & multimod == FALSE), 1, 0)) %>%
  group_by(method,n) %>%
  summarise(acc = mean(correct, na.rm = TRUE))
acc_multimod

# B) accuracy in detecting number of modes --------------------------------------#
acc_modality <- results_df %>%
  filter(method != "dip" & method != "BC" & method != "DFU" & method != "ACR multimodality") %>%
  mutate(corr = ifelse(true_modes == modality_est, 1, 0)) %>%
  group_by(method) %>%
  summarise(acc = mean(corr, na.rm=TRUE))
acc_modality


# C) plot results ---------------------------------------------------------------------------------#

# code distribution type
results_df$dist_type <- c()
for(i in 1:nrow(results_df)){
  results_df$dist_type[i] <- strsplit(results_df$name[i], "_N")[[1]][1]
}
results_df$dist_type2 <- ifelse(grepl("Trimodal_Gaussians", results_df$dist_type), "Trimodal Gaussians",
                             ifelse(grepl("Bimodal_Gaussians", results_df$dist_type), "Bimodal Gaussians",
                                    ifelse(grepl("Bimodal_Special_", results_df$dist_type), "Bimodal Special Case",
                                           ifelse(grepl("Unimodal_Beta", results_df$dist_type), "Unimodal Beta", 
                                                  results_df$dist_type))))
results_df$dist_type2 <- gsub("_", " ", results_df$dist_type2)
results_df$dist_type3 <- ifelse((results_df$dist_type2 == "Unimodal Beta" | results_df$dist_type2 == "Unimodal Special Staircase"), 
                             "Unimodal Non-Gaussian", 
                             ifelse((grepl("Bimodal", results_df$dist_type2) & results_df$dist_type2 != "Bimodal Gaussians"), 
                                    "Bimodal Non-Gaussian", 
                                    ifelse(results_df$dist_type2 == "Trimodal beta mixtures", "Trimodal Non-Gaussian",
                                           results_df$dist_type2)))
results_df$dist_type3 <- gsub("_", " ", results_df$dist_type3)


# FIGURE 3 - discriminating uni- from multimodality ----------#
results_df$correct_multimod <- ifelse((results_df$true_modes > 1 & results_df$multimod == TRUE) 
                                      |  (results_df$true_modes == 1 & results_df$multimod == FALSE),
                                   1, 0)

multimod_accuracies4 <- results_df %>% 
  group_by(dist_type3, n, method) %>%
  summarize(acc = round(mean(correct_multimod),3)) %>%
  tidyr::pivot_wider(
    id_cols = c(dist_type3, n), 
    names_from = method,   
    values_from = acc  
  )

multimod4_long <- multimod_accuracies4 %>%
  tidyr::pivot_longer(
    cols = !c(dist_type3, n), 
    names_to = "method",
    values_to = "mean_accuracy"
  )

colors <- c("DensMM" = "#79C3B8", "Remode" = "#ED6A5A" , "silverman" = "#5CA4A9",
            "GMM" = "black", "ACR multimodality" = "black",
            "BC" = "black", "dip" = "black", "DFU" = "black")

gg_list <- list()

for (i in c( "DFU","ACR multimodality", "Remode",  "silverman", "DensMM", "GMM", "ACR multimodality" ,"BC", "dip", "DFU")) {
  subset <- multimod4_long[multimod4_long$method == i,]
  gg <- ggplot(subset, aes(x = interaction(factor(n), dist_type3), y = mean_accuracy, 
                           group = dist_type3, color = method)) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "darkgrey") +
    geom_line(position = position_dodge(width = 0.5),  show.legend = FALSE) +
    geom_point(position = position_dodge(width = 0.5),  show.legend = FALSE) +
    labs(x = " ", y = "Accuracy", title = i) +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_color_manual(values = colors)
  
  # Include legend only in the first plot
  # if (i == "BC") {
  #   gg <- gg + scale_color_discrete(name = "Distribution Type")
  # } else {
  #   gg <- gg + theme(legend.position = "none")
  # }
  # only x axis labels for last plot 
  if (i == "dip") {
    gg <- gg + scale_x_discrete(labels = rep(unique(subset$n), 8))
  } else {
    gg <- gg + scale_x_discrete(labels = NULL)
  }
  
  gg_list[[i]] <- gg
  
}

# Arrange plots in a grid
gg_plot_grid <- wrap_plots(gg_list, ncol = 1)

# Display the grid of plots
gg_plot_grid



# FIGURE 4 - Detecting number of modes ------------------------#

# data frame with methods that detect the number of modes
df <- results_df[results_df$method %in% c("silverman", "DensMM", "Remode"), ]

df$correct_modality <- ifelse(df$true_modes == df$modality_est, 1, 0)

df_correct <- df %>% 
  group_by(dist_type3, n, method) %>%
  summarize(acc = round(mean(correct_modality, na.rm = TRUE),3)) %>%
  tidyr::pivot_wider(
    id_cols = c(dist_type3, n), 
    names_from = method,   
    values_from = acc  
  )

df_long <- df_correct %>%
  tidyr::pivot_longer(
    cols = !c(dist_type3, n), 
    names_to = "method",
    values_to = "mean_accuracy"
  )


method_linetypes <- c("DensMM" = "dashed", "Remode" = "solid", "silverman" = "dotted")
method_colors <- c("DensMM" = "#79C3B8", "Remode" = "#ED6A5A" , "silverman" = "#5CA4A9")
ggplot(df_long, aes(x = factor(n), y = mean_accuracy, color = method, 
                    linetype = method, shape = method, group = method)) +
  geom_line(size = 1, alpha = 0.5) +
  geom_point(size = 3) +
  facet_wrap(~ dist_type3, scales = "free_x", nrow = 4, ncol = 2) +
  scale_linetype_manual(values = method_linetypes) +  
  scale_color_manual(values = method_colors) +       
  scale_shape_manual(values = c(15, 16, 17, 18)) +   
  labs(
    title = "Mean Accuracy in Detecting the Correct Number of Modes",
    subtitle = "by Sample Size and Method",
    x = "Sample Size (n)",
    y = "Mean Accuracy",
    color = "Method",
    linetype = "Method",
    shape = "Method"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    strip.text = element_text(size = 12), 
    panel.spacing = unit(1, "lines")  
  )
