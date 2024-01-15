# Read in the data
nfldata <- read.csv("spreadspoke_scores.csv")

# Data preprocessing
# Remove rows with missing values
nfldata <- na.omit(nfldata)


# Create new column Y indicating if the home team won or lost
nfldata$Y <- ifelse(nfldata$score_home > nfldata$score_away, 1, 0)


# Helper functions
logit <- function(x) {
  1 / (1 + exp(-x))
}

# Newton-Raphson algorithm
newton_raphson <- function(X, y, tol = 1e-6, max_iter = 1000) {
  n <- nrow(X)
  p <- ncol(X)
  beta <- rep(0, p) # Initialize beta coefficients
  iter <- 0
  
  repeat {
    iter <- iter + 1
    
    # Calculate the predicted probabilities
    eta <- X %*% beta
    p <- logit(eta)
    
    # Calculate the gradient and second deriv of the log-likelihood function
    gradient <- t(X) %*% (y - p)
    W <- diag(as.vector(p * (1 - p)))
    secondorder <- -t(X) %*% W %*% X
    
    # Update the beta coefficients
    delta <- solve(secondorder, gradient)
    beta_new <- beta - delta
    
    # Check for convergence
    if (all(abs(beta_new - beta) < tol) || iter >= max_iter) {
      break
    }
    
    beta <- beta_new
  }
  
  return(beta)
}

# Prepare the design matrix X and response vector y
X <- cbind(1, nfldata$weather_temperature, nfldata$weather_wind_mph, nfldata$weather_humidity)
y <- nfldata$Y

# Fit the logistic regression model using Newton-Raphson method
beta <- newton_raphson(X, y)

# Save the true_betas values
write.table(beta, file = "true_betas.txt",
            col.names = FALSE, row.names = FALSE)

# Function to generate synthetic data
generate_synthetic_data <- function(beta, n) {
  # Generate synthetic values for predictor variables
  synthetic_temperature <- rnorm(n, mean(nfldata$weather_temperature),
                                 sd(nfldata$weather_temperature))
  synthetic_wind_mph <- rnorm(n, mean(nfldata$weather_wind_mph),
                              sd(nfldata$weather_wind_mph))
  synthetic_humidity <- rnorm(n, mean(nfldata$weather_humidity),
                              sd(nfldata$weather_humidity))
  
  # Calculate the log-odds of the home team winning for each data point
  synthetic_logit <- beta[1] + beta[2] * synthetic_temperature + 
    beta[3] * synthetic_wind_mph + beta[4] * synthetic_humidity
  
  # Convert the log-odds to probabilities
  synthetic_probabilities <- logit(synthetic_logit)
  
  # Generate binary outcomes (home team wins or losses)
  synthetic_outcomes <- ifelse(synthetic_probabilities > runif(n), 1, 0)
  
  # Combine synthetic predictor variables and generated binary outcomes
  synthetic_data <- data.frame(weather_temperature = synthetic_temperature,
                               weather_wind_mph = synthetic_wind_mph,
                               weather_humidity = synthetic_humidity,
                               Y = synthetic_outcomes)
  
  return(synthetic_data)
}


# Function to evaluate the model
evaluate_model <- function(y_true, y_pred) {
  rmse <- sqrt(mean((y_true - y_pred) ^ 2))
  return(rmse)
}


N <- 100 # number of simulations
rmse_values <- numeric(N)
estimated_betas <- matrix(0, nrow = N, ncol = 4)
set.seed(2)
for (i in 1:N) {
  # Generate synthetic data
  synthetic_data <- generate_synthetic_data(beta, n = 6366)
  
  # Prepare the design matrix X and response vector y
  X <- cbind(1, synthetic_data$weather_temperature, 
             synthetic_data$weather_wind_mph, synthetic_data$weather_humidity)
  y <- synthetic_data$Y
  
  # Fit the logistic regression model using Newton-Raphson method
  estimated_beta <- newton_raphson(X, y)
  
  # Generate predicted probabilities for the synthetic data
  synthetic_logit <- X %*% estimated_beta
  y_pred <- logit(synthetic_logit)
  
  
  # Store the estimated beta coefficients
  estimated_betas[i,] <- estimated_beta
  
  # Evaluate the model
  rmse <- evaluate_model(y, y_pred)
  rmse_values[i] <- rmse
}

# Save the RMSE values
write.table(rmse_values, file = "rmse_values.txt",
            col.names = FALSE, row.names = FALSE)

# Save the estimated_betas values
write.table(estimated_betas, file = "estimated_betas.txt",
            col.names = FALSE, row.names = FALSE)