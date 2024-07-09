# CODE OF REMODE ALGORITHM WITH ALPHA CORRECTION OPTION 1-6

# define statistical testing based on chosen statistical test for remode_find_maxima
perform_fisher_test <- function(candidate, left_minimum, right_minimum, x, alpha){
  p_left <- fisher.test(matrix(c(x[candidate], sum(x) - x[candidate], x[left_minimum], sum(x) - x[left_minimum]), ncol = 2),
                        alternative = "greater")$p.value
  p_right <- fisher.test(matrix(c(x[candidate], sum(x) - x[candidate], x[right_minimum], sum(x) - x[right_minimum]), ncol = 2),
                         alternative = "greater")$p.value
  
  if (p_left < alpha && p_right < alpha) return(candidate)
}

perform_binomial_test <- function(candidate, left_minimum, right_minimum, x, alpha){
  p_left <- binom.test(c(x[candidate], x[left_minimum]),alternative='greater')$p.value
  p_right <- binom.test(c(x[candidate], x[right_minimum]),alternative='greater')$p.value
  
  if (p_left < alpha && p_right < alpha) return(candidate)
}


remode_find_maxima <- function(x, alpha = 0.05, check = FALSE, test_func) {
  
  # Early return for short vectors
  if (length(x) < 3) {
    if (check) print(paste('x =', paste(x,collapse=","),'stop'))
    return(integer(0)) 
  }
  if (check) 
    print(paste('x =', paste(x,collapse=",")))
  
  
  candidate <- which.max(x) # position candidate maximum
  left_minimum <- which.min(x[1:candidate]) # position left minimum 
  right_minimum <- which.min(x[candidate:length(x)]) + candidate - 1 # position right minimum 
  
  if (check) {
    print(paste('locations extrema =', paste(c(left_minimum, candidate, right_minimum),collapse=",")))
  }
  
  # perform chosen statistical test on both sides
  result <- if ((x[candidate] > x[left_minimum]) & (x[candidate] > x[right_minimum])) {
    test_func(candidate, left_minimum, right_minimum, x, alpha)
  }
  
  if (check) {
    print(paste("mode detected at:", result))
  }
  
  # recursive calls on left and right sides of candidate
  c(result, 
    remode_find_maxima(x[1:(candidate - 1)], alpha = alpha, check = check, test_func = test_func), 
    remode_find_maxima(x[(candidate + 1):length(x)], alpha = alpha, check = check, test_func = test_func) + candidate)
}


remode <- function(xt, alpha = 0.05, alpha_correction, 
                   check = FALSE, f_sign_test = c("fisher", "binomial"), 
                   format_raw = FALSE, levels = seq(min(xt), max(xt)), ...) {  
  
  # define which stat. function to use based on user input
  if (is.function(f_sign_test)) { # function defined by user
    test_func <- f_sign_test
  } else { # user chooses predefined function
    test_type <- match.arg(f_sign_test) # fisher is default test
    test_func <- switch(test_type,
                        fisher = perform_fisher_test,
                        binomial = perform_binomial_test)
  }
  
  # convert raw data to table if required
  if (format_raw) {
    x_factor <- factor(xt, levels = levels)
    xt <- table(x_factor)
  }
  if(length(xt) > 50 & format_raw == FALSE){
    warning("The length of your data suggests it might be in raw format and not a frequency table. 
            If so, please specify by setting format_raw = TRUE.")
  }
  
  # alpha correction based on input 1-6
  le <- length(xt)
  alpha_cor <- switch(as.character(alpha_correction),
                      "1" = alpha / (le - 1), 
                      "2" = alpha / (floor((le + 1) / 2)), # max_nodes
                      "3" = 2 * alpha / (0.5 * le * (le - 1)),
                      "4" = alpha / 3,
                      "5" = alpha, # "none"
                      "6" = alpha / sqrt(le),
                      alpha)
  
  modes <- remode_find_maxima(c(0, xt, 0), alpha = alpha_cor, check = check, test_func = test_func) # add zeros to sides
  modes <- modes - 1 # correct for adding zero's (zero on left side)
  
  nr_of_modes <- length(modes)
  
  result <- list(nr_of_modes = nr_of_modes, modes = modes, xt = xt, alpha = alpha, 
                 alpha_correction = alpha_correction)
  
  class(result) <- 'remode_result'
  
  return(result)
}
