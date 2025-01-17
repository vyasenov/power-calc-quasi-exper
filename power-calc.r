rm(list=ls())
set.seed(13223)   # For reproducibility

library(lmtest)  # For testing fixed effect models
library(plm)     # For fixed effect (within) models

###############
############### Define parameters
###############

mu <- 1.94       # Mean of outcome variable
sigma <- 1.26    # Standard deviation of outcome variable
sd_resid <- 0.8  # Standard deviation of residual from fixed effect model

effect_sizes <- c(0.2, 0.3)  # Effect sizes to test (5%, 10%, 15% lifts)
alpha <- 0.05   # Significance level
desired_power <- 0.8  # Desired power level
num_simulations <- 1000  # Number of simulations

n_users_start <- 100  # Start with a small number of users
n_users_increment <- 100

###############
############### Function to perform Monte Carlo simulation
###############

run_simulation <- function(n_users, effect_size, num_simulations, alpha) {
  significant_results <- 0
  
  for (i in 1:num_simulations) {

    # Generate data
    user_id <- rep(1:n_users, each = 2)
    post <- rep(c(0, 1), length.out = n_users * 2)
    
############### OUTCOME
    # pre-treatment outcomes
    y <- rep(rnorm(n_users, mean = mu, sd = sigma), each=2)    

    # post-treatment outcomes
    y[post == 1] <- y[post == 1] + effect_size
    
    # add noise
    y <- y + rnorm(n_users * 2, mean = 0, sd = sd_resid)   
###############

    # combine all data
    data <- data.frame(user_id, post, y)

    # Fit fixed effect model using plm package
    model <- plm(y ~ post, data = data, index = "user_id", model = "within", effect = "individual")

    # model result
    p_value <- coeftest(model, vcov = vcovHC)[1, 4]

    # Check if the effect is statistically significant
    if (p_value < alpha) {
      significant_results <- significant_results + 1
    }
  }
  
  # Calculate power as the proportion of significant results
  power <- significant_results / num_simulations
  return(power)
}

###############
############### Monte Carlo simulation Function
###############

# Run simulations for different effect sizes and find the minimum sample size to achieve 80% power
for (effect_size in effect_sizes) {
  achieved_power <- 0
  n_users <- n_users_start  # Start with a small number of users

  while (achieved_power < desired_power) {
    achieved_power <- run_simulation(n_users, effect_size, num_simulations, alpha)

    cat("Effect Size:", effect_size, "Sample Size:", n_users, "Power:", achieved_power, "\n")

    n_users <- n_users + n_users_increment  # Increment sample size
  }
  
  cat("Minimum sample size for", effect_size * 100, "% lift:", n_users - n_users_increment, "to achieve 80% power.\n\n")
}
