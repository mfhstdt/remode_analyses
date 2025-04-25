# TEST REPEATED APPLICATION OF METHODS TO RESAMPLED DATA SAMPLES OF BENCHMARK #

# Step 1: create 10 random samples per dist type
n = 500; c = 10 # test on all distribution types where N=500 and c=10
resampled_distributions <- list()


# uniform dist  
i = 1
for(it in 1:10){
  dist <- sample(1:10, n, replace=TRUE)
  resampled_distributions[[i]] <- factor(dist, levels=1:c)
  names(resampled_distributions)[i] <- paste0("Uniform_N_",n,"_C_",c,"_it_",it)
  i = i+1
}

# simulate Gaussian
i = 11
for(it in 1:10){
  dist <- rnorm(n, 0, 1) # standard normal
  dist <- (dist - min(dist)) / (max(dist) - min(dist)) * 0.99999 + 0.00001 # scale to 0-1 without truncating
  dist <- ceiling(dist * c)
  resampled_distributions[[i]] <- factor(dist, levels=1:c)
  names(resampled_distributions)[i] <- paste0("Unimodal_Gaussian_N_",n,"_C_",c,"_it_",it)
  i = i+1
}


# simulate betas 
i = 21
betas_unimodal <- list(
  c(2,8),
  c(4,4), 
  c(100,100))
for(k in 1:length(betas_unimodal)){
  for(it in 1:10){
    dist <- rbeta(n, betas_unimodal[[k]][1], betas_unimodal[[k]][2])
    dist <- ceiling(dist*c)
    resampled_distributions[[i]] <- factor(dist, levels=1:c)
    names(resampled_distributions)[i] <- paste0("Unimodal_Beta_",betas_unimodal[[k]][1], ",", betas_unimodal[[k]][2],"_N_", n,"_C_",c,"_it_",it)
    i=i+1
  }
}


# add special case: stair case distribution
i = 51
for(it in 1:10){
  if(c == 5) prob <- seq(0.1, 0.3, by = 0.05) else prob <- (seq(1, 10, by = 1) / 55)
  dist <- sample(1:length(prob), size = n, replace = TRUE, prob = prob)
  dist <- factor(dist, levels=1:c)
  resampled_distributions[[i]] <- dist
  names(resampled_distributions)[i] <- paste0("Unimodal_Special_Staircase_N_", n,"_C_",c,"_it_",it) 
  i=i+1
}


# simulate mixture of 2 Gaussians (small overlap) - equally weighted
i = 61
for(it in 1:10){
  pi = c(0.5, 0.5) # mixture proportions
  mu = c(0.2, 0.8); sd = (0.2/1.645) # component means and SD for scale 0-1
  # stochastic sampling from 2 Gaussians
  components <- sample(1:2, size = n, replace = TRUE, prob = pi)
  dist <- rnorm(n, mean = mu[components], sd = sd)
  # rescale to 0-1 without truncating
  dist <- (dist - min(dist)) / (max(dist) - min(dist)) * 0.99999 + 0.00001 
  dist <- ceiling(dist * c) # scale to c categories
  resampled_distributions[[i]] <- factor(dist, levels=1:c)
  names(resampled_distributions)[i] <- paste0("Bimodal_Gaussians_weight50,50_N_", n,"_C_",c,"_it_",it)
  i=i+1
}

# simulate mixture of 2 Gaussians (small overlap) - weight 60-40
i = 71
for(it in 1:10){   
  pi = c(0.6, 0.4) # mixture proportions
  mu = c(0.15, 0.85); sd = (0.2/1.645) # component means and SD for scale 0-1
  # stochastic sampling from 2 Gaussians
  components <- sample(1:2, size = n, replace = TRUE, prob = pi)
  dist <- rnorm(n, mean = mu[components], sd = sd)
  # rescale to 0-1 without truncating
  dist <- (dist - min(dist)) / (max(dist) - min(dist)) * 0.99999 + 0.00001 
  dist <- ceiling(dist * c) # scale to c categories
  resampled_distributions[[i]] <- factor(dist, levels=1:c)
  names(resampled_distributions)[i] <- paste0("Bimodal_Gaussians_weight60,40_N_", n,"_C_",c,"_it_",it)
  i = i+1
}


# simulate mixture of 2 Gaussians (small overlap) - weight 70-30
i = 81
for(it in 1:10){   
  pi = c(0.7, 0.3) # mixture proportions
  mu = c(0.15, 0.9); sd = (0.2/1.645) # component means and SD for scale 0-1
  # stochastic sampling from 2 Gaussians
  components <- sample(1:2, size = n, replace = TRUE, prob = pi)
  dist <- rnorm(n, mean = mu[components], sd = sd)
  # rescale to 0-1 without truncating
  dist <- (dist - min(dist)) / (max(dist) - min(dist)) * 0.99999 + 0.00001 
  dist <- ceiling(dist * c) # scale to c categories
  resampled_distributions[[i]] <- factor(dist, levels=1:c)
  names(resampled_distributions)[i] <- paste0("Bimodal_Gaussians_weight70,30_N_", n,"_C_",c,"_it_",it)
  i = i+1
}

# simulate mixture of 2 Gaussians (small overlap) - weight 80-20
i = 91
for(it in 1:10){   
  pi = c(0.8, 0.2) # mixture proportions
  mu = c(0.15, 0.85); sd = (0.2/1.645) # component means and SD for scale 0-1
  # stochastic sampling from 2 Gaussians
  components <- sample(1:2, size = n, replace = TRUE, prob = pi)
  dist <- rnorm(n, mean = mu[components], sd = sd)
  # rescale to 0-1 without truncating
  dist <- (dist - min(dist)) / (max(dist) - min(dist)) * 0.99999 + 0.00001 
  dist <- ceiling(dist * c) # scale to c categories
  resampled_distributions[[i]] <- factor(dist, levels=1:c)
  names(resampled_distributions)[i] <- paste0("Bimodal_Gaussians_weight80,20_N_", n,"_C_",c,"_it_",it)
  i = i+1
}

# simulate mixture of 2 Gaussians (small overlap) - weight 90-10
i = 101
for(it in 1:10){  
  pi = c(0.8, 0.2) # mixture proportions
  mu = c(0.15, 0.95); sd = (0.2/1.645) # component means and SD for scale 0-1
  # stochastic sampling from 2 Gaussians
  components <- sample(1:2, size = n, replace = TRUE, prob = pi)
  dist <- rnorm(n, mean = mu[components], sd = sd)
  # rescale to 0-1 without truncating
  dist <- (dist - min(dist)) / (max(dist) - min(dist)) * 0.99999 + 0.00001 
  dist <- ceiling(dist * c) # scale to c categories
  resampled_distributions[[i]] <- factor(dist, levels=1:c)
  names(resampled_distributions)[i] <- paste0("Bimodal_Gaussians_weight90,10_N_", n,"_C_",c,"_it_",it)
  i = i+1
}

# simulate mixture of 2 Gaussians (larger overlap)
i = 111
for(it in 1:10){  
  pi = c(0.5, 0.5) # mixture proportions
  mu = c(0.2, 0.8); sd = (0.2/1.645) # component means and SD for scale 0-1
  # stochastic sampling from 2 Gaussians
  components <- sample(1:2, size = n, replace = TRUE, prob = pi)
  dist <- rnorm(n, mean = mu[components], sd = sd)
  # rescale without truncating
  dist <- (dist - min(dist)) / (max(dist) - min(dist)) * 0.99999 + 0.00001
  dist <- ceiling(dist * c)
  resampled_distributions[[i]] <- factor(dist, levels=1:c)
  names(resampled_distributions)[i] <- paste0("Bimodal_Gaussians_larger_overlap_N_", n,"_C_",c,"_it_",it)
  i=i+1
}


# simulate from beta(0.5, 0.5) 
i = 121
for(it in 1:10){  
  dist <- rbeta(n, 0.5, 0.5)
  dist <- ceiling(dist*c)
  dist <- factor(dist, levels=1:c)
  resampled_distributions[[i]] <- dist
  names(resampled_distributions)[i] <- paste0("Bimodal_Beta_0.5,0.5_N_", n,"_C_",c,"_it_",it)
  i=i+1
}

# add special cases
# valley
i = 131
for(it in 1:10){  
  if(c == 5) prob=c(0.24, 0.24, 0.04, 0.24, 0.24) else prob=c(0.154, 0.154, 0.154, 0.03, 0.008, 0.008, 0.03, 0.154, 0.154, 0.154)
  dist <- rep(1:length(prob), times=prob*n)
  resampled_distributions[[i]] <- factor(dist, levels=1:c)
  names(resampled_distributions)[i] <- paste0("Bimodal_Special_Valley_N_", n,"_C_",c,"_it_",it)
  i=i+1
}

# small local mode
i = 141
for(it in 1:10){  
  if(c == 5) prob=c(0.84, 0.03, 0.03, 0.03, 0.07) else prob=c(0.474, 0.474, 0.002, 0.002, 0.002, 0.002, 0.002, 0.002, 0.02, 0.02)
  dist <- rep(1:length(prob), times=prob*n)
  resampled_distributions[[i]] <- factor(dist, levels=1:c)
  names(resampled_distributions)[i] <- paste0("Bimodal_Special_Small_Mode_N_", n,"_C_",c,"_it_",it) 
  i=i+1
}

# small local mode with stronger weight 
i = 151
for(it in 1:10){  
  if(c == 5) prob=c(0.64, 0.03, 0.03, 0.03, 0.27) else prob=c(0.394, 0.394, 0.002, 0.002, 0.002, 0.002, 0.002, 0.002, 0.1, 0.1)
  dist <- rep(1:length(prob), times=prob*n)
  resampled_distributions[[i]] <- factor(dist, levels=1:c)
  names(resampled_distributions)[i] <- paste0("Bimodal_Special_Small_Mode_weighted_N_", n,"_C_",c,"_it_",it)
  i=i+1
}

# GENERATE TRIMODAL DISTRIBUTIONS --------------

# mixture of 3 Gaussians (same weight)
i = 161
for(it in 1:10){  
  pi = c(1/3, 1/3, 1/3) # mixture proportions
  mu = c(1/6, 0.5, 5/6) # component means
  sd = (1/6) / 3.719 # taken from rearranged z-value formula (99.9 percentile)
  # stochastic sampling from 3 Gaussians
  components <- sample(1:3, size = n, replace = TRUE, prob = pi)
  dist <- rnorm(n, mean = mu[components], sd = sd)
  # rescale to 0-1
  dist <- (dist - min(dist)) / (max(dist) - min(dist)) * 0.99999 + 0.00001
  dist <- ceiling(dist * c)
  resampled_distributions[[i]] <- factor(dist, levels=1:c)
  names(resampled_distributions)[i] <- paste0("Trimodal_Gaussians_N_", n,"_C_",c,"_it_",it)
  i=i+1
}

# mixture of 3 Gaussians (weight 20-50-30)
i = 171
for(it in 1:10){  
  pi = c(0.2, 0.5, 0.3) # mixture proportions
  mu = c(1/6, 0.5, 5/6) # component means
  sd = (1/6) / 3.719 # taken from rearranged z-value formula (99.9 percentile)
  # stochastic sampling from 3 Gaussians
  components <- sample(1:3, size = n, replace = TRUE, prob = pi)
  dist <- rnorm(n, mean = mu[components], sd = sd)
  # rescale to 0-1
  dist <- (dist - min(dist)) / (max(dist) - min(dist)) * 0.99999 + 0.00001
  dist <- ceiling(dist * c)
  resampled_distributions[[i]] <- factor(dist, levels=1:c)
  names(resampled_distributions)[i] <- paste0("Trimodal_Gaussians_weight20,50,30_N_", n,"_C_",c,"_it_",it)
  i=i+1
}

# mixture of 3 Gaussians (weight 50-30-20)
i = 181
for(it in 1:10){  
  pi = c(0.5, 0.3, 0.2) # mixture proportions
  mu = c(1/6, 0.5, 5/6) # component means
  sd = (1/6) / 3.719 # taken from rearranged z-value formula (99.9 percentile)
  # stochastic sampling from 3 Gaussians
  components <- sample(1:3, size = n, replace = TRUE, prob = pi)
  dist <- rnorm(n, mean = mu[components], sd = sd)
  # rescale to 0-1
  dist <- (dist - min(dist)) / (max(dist) - min(dist)) * 0.99999 + 0.00001
  dist <- ceiling(dist * c)
  resampled_distributions[[i]] <- factor(dist, levels=1:c)
  names(resampled_distributions)[i] <- paste0("Trimodal_Gaussians_weight50,30,20_N_", n,"_C_",c,"_it_",it)
  i=i+1
}

# mixture of 3 Gaussians (weight 30-20-50)
i = 191
for(it in 1:10){  
  pi = c(0.3, 0.2, 0.5) # mixture proportions
  mu = c(1/6, 0.5, 5/6) # component means
  sd = (1/6) / 3.719 # taken from rearranged z-value formula (99.9 percentile)
  # stochastic sampling from 3 Gaussians
  components <- sample(1:3, size = n, replace = TRUE, prob = pi)
  dist <- rnorm(n, mean = mu[components], sd = sd)
  # rescale to 0-1
  dist <- (dist - min(dist)) / (max(dist) - min(dist)) * 0.99999 + 0.00001
  dist <- ceiling(dist * c)
  resampled_distributions[[i]] <- factor(dist, levels=1:c)
  names(resampled_distributions)[i] <- paste0("Trimodal_Gaussians_weight30,20,50_N_", n,"_C_",c,"_it_",it)
  i=i+1
}


# simulate mixture of 3 betas
i = 201
for(it in 1:10){  
  pi = c(1/3, 1/3, 1/3) # mixture proportions
  shape1 = c(50, 1, 8) # beta dist. parameters of components
  shape2 = c(50, 8, 1)
  # stochastic sampling from 3 beta distributions
  components <- sample(1:3, size=n, replace=TRUE, prob=pi)
  dist <- rbeta(n, shape1[components], shape2[components])
  # rescale to 0-1
  dist <- ceiling(dist*c)
  resampled_distributions[[i]] <- factor(dist, levels=1:c)
  names(resampled_distributions)[i] <- paste0("Trimodal_beta_mixtures_N_", n,"_C_",c,"_it_",it)
  i=i+1
}

# lastly: claw distribution
i = 211
for(it in 1:10){  
  dist=rnorm(.5*n,0,1)
  for(j in 0:4) dist = c(dist,rnorm(.1*n, j/2-1, .1))
  xt=hist(dist,breaks=30, plot=F)$counts
  dist <- rep(seq_along(xt), xt)
  resampled_distributions[[i]] <- factor(dist)
  names(resampled_distributions)[i] <- paste0("Claw_N_", n,"_C_",c,"_it_", it)
  i=i+1
}


saveRDS(resampled_distributions, "simulation_study/test_resampling/resampled_distributions.RData")

# check: plot all samples --
par(mfrow=c(2,4))
for(n in 1:length(resampled_distributions)){
  barplot(table(resampled_distributions[[n]]), main = names(resampled_distributions)[n], cex.main=1)
}
