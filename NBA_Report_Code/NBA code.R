NBADATA <- read.csv("NBA_Team_Stats.csv")

NBADATA$Year <- sub("^(\\d+)-.*", "\\1", NBADATA$Year)

# Convert Year to numeric
NBADATA$Year <- as.numeric(NBADATA$Year)

# Subtract 2000 from Year
NBADATA$Year <- NBADATA$Year

model <- lm(Year ~ Pts + Reb + Ast + Stl + Blk + To + Pf + Dreb + Oreb + FGA + FGM + FG. +
              X3m_per_game + X3a_per_game + X3pt. + Ftm + Fta +  FT. + Eff + Deff, data = NBADATA)

summary(model)

# Load necessary libraries
library(ggplot2)
library(dplyr)

# Assuming your data is in a dataframe called NBADATA

# Calculate yearly averages for Points and Blocks
yearly_stats <- NBADATA %>%
  group_by(Year) %>%
  summarise(AvgPts = mean(Pts, na.rm = TRUE),
            AvgBlk = mean(Blk, na.rm = TRUE),
            AvgDeff = mean(Deff + Eff, na.rm = TRUE),
            AvgEff = mean(Eff, na.rm = TRUE))

# Create a time series plot
ggplot() +
  geom_line(data = yearly_stats, aes(x = Year, y = AvgPts), color = "blue") +
  labs(title = "NBA Yearly Average Points",
       x = "Year",
       y = "Average") +
  theme_minimal() +
  scale_y_continuous(sec.axis = sec_axis(~., name = "Points")) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))

# Plotting
ggplot(data = yearly_stats, aes(x = Year, y = AvgDeff)) +
  geom_line(color = "red") +
  labs(title = "NBA Yearly Average Defensive Efficiency",
       x = "Year",
       y = "Defensive Efficiency (Deff)") +
  theme_minimal() +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))

# Plotting
ggplot(data = yearly_stats, aes(x = Year, y = AvgEff)) +
  geom_line(color = "blue") +
  labs(title = "NBA Yearly Average Efficiency",
       x = "Year",
       y = "Efficiency (Eff)") +
  theme_minimal() +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))