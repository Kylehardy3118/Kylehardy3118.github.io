# Read in the RMSE values
rmse_values <- read.table("rmse_values.txt", col.names = c("RMSE"))

# Read in the estimated_betas values
estimated_betas <- read.table("estimated_betas.txt",
                              col.names = c("beta_0",
                                            "beta_1", "beta_2", "beta_3"))
# Read in the true_betas values
beta <- read.table("true_betas.txt",
                              col.names = c("beta"))

# Calculate summary statistics
summary_stats <- summary(rmse_values)

# Print summary statistics
cat("Summary statistics for RMSE values:\n")
print(summary_stats)

# Save simulation results into pdf
pdf("simulation results.pdf")

# Create a histogram of RMSE values
hist(rmse_values$RMSE, main = "Histogram of RMSE values", xlab = "RMSE",
     ylab = "Frequency", col = "lightblue", border = "black")


# Generate histogram for each parameter estimate
par(mfrow = c(2, 2)) # Arrange the histograms in a 2x2 grid

for (i in 1:4) {
  hist(estimated_betas[,i], main = paste("Histogram of estimated beta_",
                                         i - 1, sep = ""),
       xlab = paste("Beta_", i - 1, sep = ""),
       col = "lightblue", border = "black")
  abline(v = beta[i,1], col = "red", lwd = 2)
}

dev.off()