# Function to Compute the Distance from Unimodality measure as proposed by Pavlopoulos & Likas, 2022 #
#   Code is adapted from https://github.com/ipavlopoulos/dfu/blob/main/lib.py   # 


# The following function computes the Distance From Unimodality (DFU) score for a given set of data
# For concept of DFU see Pavlopoulos & Likas, 2022
# Adjustments in this function:
# - If the highest peak is at the end point of the scale, then only do either right search or left search; 
# - Divide max_diff by N to make values comparable across distributions with different sample sizes

# Input is a vector, containing categorical raw data of the distribution 
# Note: input must be categorical data 

dfu <- function(x){
  # Get frequencies of each category 
  x_factor <- factor(x, levels = seq(min(x), max(x)))
  freq <- table(x_factor)
  
  # get most frequent category (mode)
  max_value <- max(freq)
  pos_max <- which(freq == max_value)[1]  # Get the first occurrence if there are multiple maxima
  
  max_diff <- 0
  # Right search for the largest difference
  if(as.numeric(names(pos_max)) != max(x)){     # only if mode is not at right end point of scale
    for (i in pos_max:(length(freq) - 1)) {
      diff <- freq[i + 1] - freq[i]
      max_diff <- max(max_diff, diff)
    }
  }
  
  # Left search for the largest difference
  if(as.numeric(names(pos_max)) != min(x)){     # only if mode is not at left end point of scale
    for (i in pos_max:2) {
      diff <- freq[i - 1] - freq[i]
      max_diff <- max(max_diff, diff)
    }
  }
  
  return(max_diff)
}


