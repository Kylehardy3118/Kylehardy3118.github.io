# Read in the data
nfldata <- read.csv("spreadspoke_scores.csv")

# Data preprocessing
# Remove rows with missing values
nfldata <- na.omit(nfldata)


# Create new column Y indicating if the home team won or lost
nfldata$Y <- ifelse(nfldata$score_home > nfldata$score_away, 1, 0)

# Summary statistics for variables of interest
print(summary(nfldata$weather_temperature))
print(summary(nfldata$weather_wind_mph))
print(summary(nfldata$weather_humidity))

# put all plots into a pdf
pdf("analysis plots.pdf")

# Histograms of weather variables
hist(nfldata$weather_temperature, main="Histogram of Temperature",
     xlab="Temperature (°F)", col="lightblue", border="black")
hist(nfldata$weather_wind_mph, main="Histogram of Wind Speed", 
     xlab="Wind Speed (mph)", col="lightgreen", border="black")
hist(nfldata$weather_humidity, main="Histogram of Humidity",
     xlab="Humidity (%)", col="lightpink", border="black")


# Calculate the percentage of 1's (home team wins) in the Y column
percentage_home_wins <- mean(nfldata$Y) * 100
print(percentage_home_wins)


# Divide temperature data into bins
bin_width <- 15
temperature_bins <- seq(floor(min(nfldata$weather_temperature)),
                        ceiling(max(nfldata$weather_temperature)),
                        by = bin_width)

# Calculate the home team win percentage for each temperature bin
win_percentages <- numeric(length(temperature_bins) - 1)

for (i in 1:(length(temperature_bins) - 1)) {
  lower_bound <- temperature_bins[i]
  upper_bound <- temperature_bins[i + 1]
  games_in_bin <- nfldata[nfldata$weather_temperature >= lower_bound &
                            nfldata$weather_temperature < upper_bound, ]
  home_team_wins <- sum(games_in_bin$Y)
  total_games <- nrow(games_in_bin)
  
  if (total_games > 0) {
    win_percentages[i] <- (home_team_wins / total_games) * 100
  } else {
    win_percentages[i] <- NA
  }
}

# Create a histogram
barplot(win_percentages,
        names.arg = paste(temperature_bins[-length(temperature_bins)], "-",
                          temperature_bins[-1]),
        xlab = "Temperature Range (°F)", ylab = "Home Team Win Percentage",
        col = "lightblue", border = "black",
        main = "Home Team Win Percentage by Temperature")

# Divide humidity data into bins
bin_width <- 10
humidity_bins <- seq(floor(min(nfldata$weather_humidity)),
                     ceiling(max(nfldata$weather_humidity)),
                     by = bin_width)

# Calculate the home team win percentage for each humidity bin
win_percentages <- numeric(length(humidity_bins) - 1)

for (i in 1:(length(humidity_bins) - 1)) {
  lower_bound <- humidity_bins[i]
  upper_bound <- humidity_bins[i + 1]
  games_in_bin <- nfldata[nfldata$weather_humidity >= lower_bound &
                            nfldata$weather_humidity < upper_bound, ]
  home_team_wins <- sum(games_in_bin$Y)
  total_games <- nrow(games_in_bin)
  
  if (total_games > 0) {
    win_percentages[i] <- (home_team_wins / total_games) * 100
  } else {
    win_percentages[i] <- NA
  }
}

# Create a histogram
barplot(win_percentages,
        names.arg = paste(humidity_bins[-length(humidity_bins)], "-",
                          humidity_bins[-1]),
        xlab = "Humidity Range (%)", ylab = "Home Team Win Percentage",
        col = "lightblue", border = "black",
        main = "Home Team Win Percentage by Humidity")


# Divide wind speed data into bins
bin_width <- 5
wind_bins <- seq(floor(min(nfldata$weather_wind_mph)),
                 ceiling(max(nfldata$weather_wind_mph)),
                 by = bin_width)

# Calculate the home team win percentage for each wind bin
win_percentages <- numeric(length(wind_bins) - 1)

for (i in 1:(length(wind_bins) - 1)) {
  lower_bound <- wind_bins[i]
  upper_bound <- wind_bins[i + 1]
  games_in_bin <- nfldata[nfldata$weather_wind_mph >= lower_bound &
                            nfldata$weather_wind_mph < upper_bound, ]
  home_team_wins <- sum(games_in_bin$Y)
  total_games <- nrow(games_in_bin)
  
  if (total_games > 0) {
    win_percentages[i] <- (home_team_wins / total_games) * 100
  } else {
    win_percentages[i] <- NA
  }
}

# Create a histogram
barplot(win_percentages, 
        names.arg = paste(wind_bins[-length(wind_bins)], "-", wind_bins[-1]),
        xlab = "Wind Speed Range (mph)", ylab = "Home Team Win Percentage", 
        col = "lightblue", border = "black",
        main = "Home Team Win Percentage by Wind Speed")


dev.off()