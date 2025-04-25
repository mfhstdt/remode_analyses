# TEST REPEATED APPLICATION OF METHODS TO RESAMPLED DATA SAMPLES OF BENCHMARK #

# STEP 3: compare accuracy by dist type of resampled application vs one-time-run on benchmark
library(dplyr)


# load all results from script 2
#TODO: fix code


# accuracy of resampled runs -------------------------------------------------------

#rdata_files <- list.files(path = "simulation_study/test_resampling", pattern = "\\.RData$", full.names = TRUE)
#for (file in rdata_files) {load(file)}


remove_it <- function(s) {
  return(gsub("_it_\\d+", "", s))
}

ACR_resampled_acc <- ACR_df %>%
  mutate(name = remove_it(name)) %>%
  mutate(
    name = case_when(
      name == "Claw_N_500_C_10" ~ "Claw_N_500",
      TRUE ~ name
    )
  ) %>%
  mutate(correct = ifelse(
    (true_modes > 1 & multimod == TRUE | true_modes == 1 & multimod == FALSE),
    1, 
    0)) %>%
  group_by(name) %>%
  summarise(acc_resample = mean(correct, na.rm = TRUE))


BC_resampled_acc <- BC_df %>%
  mutate(name = remove_it(name)) %>%
  mutate(
    name = case_when(
      name == "Claw_N_500_C_10" ~ "Claw_N_500",
      TRUE ~ name
    )
  ) %>%
  mutate(correct = ifelse(
    (true_modes > 1 & multimod == TRUE | true_modes == 1 & multimod == FALSE),
    1, 
    0)) %>%
  group_by(name) %>%
  summarise(acc_resample = mean(correct, na.rm = TRUE))



DensMM_resampled_acc <- DensMM_df %>%
  mutate(name = remove_it(name)) %>%
  mutate(
    name = case_when(
      name == "Claw_N_500_C_10" ~ "Claw_N_500",
      TRUE ~ name
    )
  ) %>%
  mutate(correct = ifelse(
    (true_modes > 1 & multimod == TRUE | true_modes == 1 & multimod == FALSE),
    1, 
    0)) %>%
  group_by(name) %>%
  summarise(acc_resample = mean(correct, na.rm = TRUE))



DFU_resampled_acc <- DFU_df %>%
  mutate(name = remove_it(name)) %>%
  mutate(
    name = case_when(
      name == "Claw_N_500_C_10" ~ "Claw_N_500",
      TRUE ~ name
    )
  ) %>%
  mutate(correct = ifelse(
    (true_modes > 1 & multimod == TRUE | true_modes == 1 & multimod == FALSE),
    1, 
    0)) %>%
  group_by(name) %>%
  summarise(acc_resample = mean(correct, na.rm = TRUE))



DIP_resampled_acc <- dip_df %>%
  mutate(name = remove_it(name)) %>%
  mutate(
    name = case_when(
      name == "Claw_N_500_C_10" ~ "Claw_N_500",
      TRUE ~ name
    )
  ) %>%
  mutate(correct = ifelse(
    (true_modes > 1 & multimod == TRUE | true_modes == 1 & multimod == FALSE),
    1, 
    0)) %>%
  group_by(name) %>%
  summarise(acc_resample = mean(correct, na.rm = TRUE))



GMM_resampled_acc <- GMM_df %>%
  mutate(name = remove_it(name)) %>%
  mutate(
    name = case_when(
      name == "Claw_N_500_C_10" ~ "Claw_N_500",
      TRUE ~ name
    )
  ) %>%
  mutate(correct = ifelse(
    (true_modes > 1 & multimod == TRUE | true_modes == 1 & multimod == FALSE),
    1, 
    0)) %>%
  group_by(name) %>%
  summarise(acc_resample = mean(correct, na.rm = TRUE))


Remode_resampled_acc <- Remode_df %>%
  mutate(name = remove_it(name)) %>%
  mutate(
    name = case_when(
      name == "Claw_N_500_C_10" ~ "Claw_N_500",
      TRUE ~ name
    )
  ) %>%
  mutate(correct = ifelse(
    (true_modes > 1 & multimod == TRUE | true_modes == 1 & multimod == FALSE),
    1, 
    0)) %>%
  group_by(name) %>%
  summarise(acc_resample = mean(correct, na.rm = TRUE))


Silverman_resampled_acc <- silverman_df %>%
  mutate(name = remove_it(name)) %>%
  mutate(
    name = case_when(
      name == "Claw_N_500_C_10" ~ "Claw_N_500",
      TRUE ~ name
    )
  ) %>%
  mutate(correct = ifelse(
    (true_modes > 1 & multimod == TRUE | true_modes == 1 & multimod == FALSE),
    1, 
    0)) %>%
  group_by(name) %>%
  summarise(acc_resample = mean(correct, na.rm = TRUE))



# accuracy of single benchmark runs----------------------------------------------
rdata_files <- list.files(path = "simulation_study/results", pattern = "\\.RData$", full.names = TRUE)
for (file in rdata_files) {load(file)}


ACR_benchmark_acc <- ACR %>%
  filter(grepl("N_500_C_10", name) | name == "Claw_N_500") %>%
  mutate(correct = ifelse(
    (true_modes > 1 & multimod == TRUE | true_modes == 1 & multimod == FALSE),
    1, 
    0)) %>%
  mutate(
    name = case_when(
      name == "Unimodal_Special_case_13_N_500_C_10" ~ "Unimodal_Special_Staircase_N_500_C_10",
      name == "Bimodal_Gaussians_small_overlap_N_500_C_10" ~ "Bimodal_Gaussians_weight50,50_N_500_C_10",
      name == "Bimodal_Special_case 11_weighted_N_500_C_10" ~ "Bimodal_Special_Small_Mode_weighted_N_500_C_10",
      name == "Bimodal_Special_case_11_N_500_C_10" ~ "Bimodal_Special_Small_Mode_N_500_C_10",
      name == "Bimodal_Special_case_10_N_500_C_10" ~ "Bimodal_Special_Valley_N_500_C_10",
      TRUE ~ name
      )
    ) %>%
  group_by(name) %>%
  summarise(acc_benchmark = mean(correct, na.rm = TRUE))


BC_benchmark_acc <- BC %>%
  filter(grepl("N_500_C_10", name) | name == "Claw_N_500") %>%
  mutate(correct = ifelse(
    (true_modes > 1 & multimod == TRUE | true_modes == 1 & multimod == FALSE),
    1, 
    0)) %>%
  mutate(
    name = case_when(
      name == "Unimodal_Special_case_13_N_500_C_10" ~ "Unimodal_Special_Staircase_N_500_C_10",
      name == "Bimodal_Gaussians_small_overlap_N_500_C_10" ~ "Bimodal_Gaussians_weight50,50_N_500_C_10",
      name == "Bimodal_Special_case 11_weighted_N_500_C_10" ~ "Bimodal_Special_Small_Mode_weighted_N_500_C_10",
      name == "Bimodal_Special_case_11_N_500_C_10" ~ "Bimodal_Special_Small_Mode_N_500_C_10",
      name == "Bimodal_Special_case_10_N_500_C_10" ~ "Bimodal_Special_Valley_N_500_C_10",
      TRUE ~ name
    )
  ) %>%
  group_by(name) %>%
  summarise(acc_benchmark = mean(correct, na.rm = TRUE))


DensMM_benchmark_acc <- DensMM %>%
  filter(grepl("N_500_C_10", name) | name == "Claw_N_500") %>%
  mutate(correct = ifelse(
    (true_modes > 1 & multimod == TRUE | true_modes == 1 & multimod == FALSE),
    1, 
    0)) %>%
  mutate(
    name = case_when(
      name == "Unimodal_Special_case_13_N_500_C_10" ~ "Unimodal_Special_Staircase_N_500_C_10",
      name == "Bimodal_Gaussians_small_overlap_N_500_C_10" ~ "Bimodal_Gaussians_weight50,50_N_500_C_10",
      name == "Bimodal_Special_case 11_weighted_N_500_C_10" ~ "Bimodal_Special_Small_Mode_weighted_N_500_C_10",
      name == "Bimodal_Special_case_11_N_500_C_10" ~ "Bimodal_Special_Small_Mode_N_500_C_10",
      name == "Bimodal_Special_case_10_N_500_C_10" ~ "Bimodal_Special_Valley_N_500_C_10",
      TRUE ~ name
    )
  ) %>%
  group_by(name) %>%
  summarise(acc_benchmark = mean(correct, na.rm = TRUE))


DFU_benchmark_acc <- dfu %>%
  filter(grepl("N_500_C_10", name) | name == "Claw_N_500") %>%
  mutate(correct = ifelse(
    (true_modes > 1 & multimod == TRUE | true_modes == 1 & multimod == FALSE),
    1, 
    0)) %>%
  mutate(
    name = case_when(
      name == "Unimodal_Special_case_13_N_500_C_10" ~ "Unimodal_Special_Staircase_N_500_C_10",
      name == "Bimodal_Gaussians_small_overlap_N_500_C_10" ~ "Bimodal_Gaussians_weight50,50_N_500_C_10",
      name == "Bimodal_Special_case 11_weighted_N_500_C_10" ~ "Bimodal_Special_Small_Mode_weighted_N_500_C_10",
      name == "Bimodal_Special_case_11_N_500_C_10" ~ "Bimodal_Special_Small_Mode_N_500_C_10",
      name == "Bimodal_Special_case_10_N_500_C_10" ~ "Bimodal_Special_Valley_N_500_C_10",
      TRUE ~ name
    )
  ) %>%
  group_by(name) %>%
  summarise(acc_benchmark = mean(correct, na.rm = TRUE))


DIP_benchmark_acc <- dip %>%
  filter(grepl("N_500_C_10", name) | name == "Claw_N_500") %>%
  mutate(correct = ifelse(
    (true_modes > 1 & multimod == TRUE | true_modes == 1 & multimod == FALSE),
    1, 
    0)) %>%
  mutate(
    name = case_when(
      name == "Unimodal_Special_case_13_N_500_C_10" ~ "Unimodal_Special_Staircase_N_500_C_10",
      name == "Bimodal_Gaussians_small_overlap_N_500_C_10" ~ "Bimodal_Gaussians_weight50,50_N_500_C_10",
      name == "Bimodal_Special_case 11_weighted_N_500_C_10" ~ "Bimodal_Special_Small_Mode_weighted_N_500_C_10",
      name == "Bimodal_Special_case_11_N_500_C_10" ~ "Bimodal_Special_Small_Mode_N_500_C_10",
      name == "Bimodal_Special_case_10_N_500_C_10" ~ "Bimodal_Special_Valley_N_500_C_10",
      TRUE ~ name
    )
  ) %>%
  group_by(name) %>%
  summarise(acc_benchmark = mean(correct, na.rm = TRUE))



GMM_benchmark_acc <- GMM %>%
  filter(grepl("N_500_C_10", name) | name == "Claw_N_500") %>%
  mutate(correct = ifelse(
    (true_modes > 1 & multimod == TRUE | true_modes == 1 & multimod == FALSE),
    1, 
    0)) %>%
  mutate(
    name = case_when(
      name == "Unimodal_Special_case_13_N_500_C_10" ~ "Unimodal_Special_Staircase_N_500_C_10",
      name == "Bimodal_Gaussians_small_overlap_N_500_C_10" ~ "Bimodal_Gaussians_weight50,50_N_500_C_10",
      name == "Bimodal_Special_case 11_weighted_N_500_C_10" ~ "Bimodal_Special_Small_Mode_weighted_N_500_C_10",
      name == "Bimodal_Special_case_11_N_500_C_10" ~ "Bimodal_Special_Small_Mode_N_500_C_10",
      name == "Bimodal_Special_case_10_N_500_C_10" ~ "Bimodal_Special_Valley_N_500_C_10",
      TRUE ~ name
    )
  ) %>%
  group_by(name) %>%
  summarise(acc_benchmark = mean(correct, na.rm = TRUE))



Remode_benchmark_acc <- remode %>%
  filter(grepl("N_500_C_10", name) | name == "Claw_N_500") %>%
  mutate(correct = ifelse(
    (true_modes > 1 & multimod == TRUE | true_modes == 1 & multimod == FALSE),
    1, 
    0)) %>%
  mutate(
    name = case_when(
      name == "Unimodal_Special_case_13_N_500_C_10" ~ "Unimodal_Special_Staircase_N_500_C_10",
      name == "Bimodal_Gaussians_small_overlap_N_500_C_10" ~ "Bimodal_Gaussians_weight50,50_N_500_C_10",
      name == "Bimodal_Special_case 11_weighted_N_500_C_10" ~ "Bimodal_Special_Small_Mode_weighted_N_500_C_10",
      name == "Bimodal_Special_case_11_N_500_C_10" ~ "Bimodal_Special_Small_Mode_N_500_C_10",
      name == "Bimodal_Special_case_10_N_500_C_10" ~ "Bimodal_Special_Valley_N_500_C_10",
      TRUE ~ name
    )
  ) %>%
  group_by(name) %>%
  summarise(acc_benchmark = mean(correct, na.rm = TRUE))


Silverman_benchmark_acc <- silverman %>%
  filter(grepl("N_500_C_10", name) | name == "Claw_N_500") %>%
  mutate(correct = ifelse(
    (true_modes > 1 & multimod == TRUE | true_modes == 1 & multimod == FALSE),
    1, 
    0)) %>%
  mutate(
    name = case_when(
      name == "Unimodal_Special_case_13_N_500_C_10" ~ "Unimodal_Special_Staircase_N_500_C_10",
      name == "Bimodal_Gaussians_small_overlap_N_500_C_10" ~ "Bimodal_Gaussians_weight50,50_N_500_C_10",
      name == "Bimodal_Special_case 11_weighted_N_500_C_10" ~ "Bimodal_Special_Small_Mode_weighted_N_500_C_10",
      name == "Bimodal_Special_case_11_N_500_C_10" ~ "Bimodal_Special_Small_Mode_N_500_C_10",
      name == "Bimodal_Special_case_10_N_500_C_10" ~ "Bimodal_Special_Valley_N_500_C_10",
      TRUE ~ name
    )
  ) %>%
  group_by(name) %>%
  summarise(acc_benchmark = mean(correct, na.rm = TRUE))


# compare by method --------------------------------------------------------------

ACR_acc_comparison <- merge(
  ACR_benchmark_acc, 
  ACR_resampled_acc,
  by = "name",
  all = TRUE
)
ACR_acc_comparison$diff_acc_ACR <- ACR_acc_comparison$acc_benchmark - ACR_acc_comparison$acc_resample

BC_acc_comparison <- merge(
  BC_benchmark_acc, 
  BC_resampled_acc,
  by = "name",
  all = TRUE
)
BC_acc_comparison$diff_acc_BC <- BC_acc_comparison$acc_benchmark - BC_acc_comparison$acc_resample

DensMM_acc_comparison <- merge(
  DensMM_benchmark_acc, 
  DensMM_resampled_acc,
  by = "name",
  all = TRUE
)
DensMM_acc_comparison$diff_acc_DensMM <- DenMM_acc_comparison$acc_benchmark - DenMM_acc_comparison$acc_resample

DFU_acc_comparison <- merge(
  DFU_benchmark_acc, 
  DFU_resampled_acc,
  by = "name",
  all = TRUE
)
DFU_acc_comparison$diff_acc_DFU <- DFU_acc_comparison$acc_benchmark - DFU_acc_comparison$acc_resample

DIP_acc_comparison <- merge(
  DIP_benchmark_acc, 
  DIP_resampled_acc,
  by = "name",
  all = TRUE
)
DIP_acc_comparison$diff_acc_DIP <- DIP_acc_comparison$acc_benchmark - DIP_acc_comparison$acc_resample

GMM_acc_comparison <- merge(
  GMM_benchmark_acc, 
  GMM_resampled_acc,
  by = "name",
  all = TRUE
)
GMM_acc_comparison$diff_acc_GMM <- GMM_acc_comparison$acc_benchmark - GMM_acc_comparison$acc_resample

Remode_acc_comparison <- merge(
  Remode_benchmark_acc, 
  Remode_resampled_acc,
  by = "name",
  all = TRUE
)
Remode_acc_comparison$diff_acc_Remode <- Remode_acc_comparison$acc_benchmark - Remode_acc_comparison$acc_resample

Silverman_acc_comparison <- merge(
  Silverman_benchmark_acc, 
  Silverman_resampled_acc,
  by = "name",
  all = TRUE
)
Silverman_acc_comparison$diff_acc_Silverman <- Silverman_acc_comparison$acc_benchmark - Silverman_acc_comparison$acc_resample

# Merge for total overview --------------------------------------------------------
acc_comparison_df <- Reduce(function(x, y) merge(x, y, by = "name", all = TRUE),
                            list(Remode_acc_comparison, 
                                 Silverman_acc_comparison, 
                                 GMM_acc_comparison, 
                                 DensMM_acc_comparison,
                                 DFU_acc_comparison, 
                                 ACR_acc_comparison, 
                                 BC_acc_comparison, 
                                 DIP_acc_comparison))

acc_comparison_df <- acc_comparison_df %>%
  select(name, starts_with("diff_acc_"))

saveRDS(acc_comparison_df, "simulation_study/test_resampling/accuracy_comparison_df.RData")

write.csv(acc_comparison_df, file = "acc_comparison_df.csv", row.names = FALSE)

