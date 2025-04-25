# -------- Function: MM Detection via GMM + roots ----------------
# taken from https://github.com/jmbh/ModalitySkewnessPaper/blob/main/aux_Functions.R

# Note that noise is determined by proportion of counts of the mode (as discussed in Haslbeck et al., 2022)

# computes density and roots of its derivative (= number of modes)
DensMMdet <- function(X, adjust=3, n = 100, plot=FALSE, categorical = TRUE) {
  
  # Specify noise 
  Q = (max(table(X)) / length(X))
  if(categorical){ # for likert scale
    noise = Q
  } else { # for continuous
    noise = 0.035 + Q/5
  }
  
  # Add noise
  Xn <- X + rnorm(length(X), 0, noise)
  
  # Density Estimation
  den <- density(Xn, bw="SJ", adjust=adjust, n=n)
  if(plot) plot(den)
  
  # Compute number of reversals
  ni <- length(den$y) 
  
  diff <- den$y[-ni] - den$y[-1]
  
  sign_reversals <- sign(diff[-(ni-1)]) != sign(diff[-1])
  Nrev <- sum(sign_reversals)
  
  Modality <- ceiling(Nrev/2) # since between each mode there is another reversal
  
  outlist <- list("M" = Modality,
                  "den_x" = den$x,
                  "den_y" = den$y)
  
  return(outlist)
}
