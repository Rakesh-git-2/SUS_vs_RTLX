
# Load necessary libraries
library(dplyr)
library(ggplot2)
library(psych)

# setting work directory
setwd("C:/Users/engin/OneDrive/Documents")

# Read the CSV file into a DataFrame
df <- read.csv('SusRtlx.csv')

# Set threshold values for outliers
max_threshold_sus <- 100
min_threshold_sus <- 0
max_threshold_rtlx <- 126
min_threshold_rtlx <- 0

# Remove rows with outliers data for 'SUS.Score'
df <- df %>%
  filter(SUS.Score >= min_threshold_sus & SUS.Score <= max_threshold_sus)

# Remove rows with outlier data for 'RTLX.Score'
df <- df %>%
  filter(RTLX.Score >= min_threshold_rtlx & RTLX.Score <= max_threshold_rtlx)

# Display the modified DataFrame
print(df)


# Summary statistics for 'SUS.Score' and 'RTLX.Score'
summary(df$SUS.Score)
summary(df$RTLX.Score)

# check if data is normally distributed
# Create a new graphics window
par(mfrow = c(1, 2))  # Set up a 1x2 grid for side-by-side plots

# create histogram for SUS score
hist(df$SUS.Score,xlab = "SUS Score", prob = TRUE, col = "#EE897E", main = "SUS Distribution")

# Plot a bell curve for SUS
curve(dnorm(x, mean = mean(df$SUS.Score), sd = sd(df$SUS.Score)), add = TRUE, col = "red")

# create histogram for RTLX Score
hist(df$RTLX.Score,xlab = "RTLX Score", prob = TRUE, col = "#4D5F81", main = "RTLX Distribution")

# Plot a bell curve for RTLX
curve(dnorm(x, mean = mean(df$RTLX.Score), sd = sd(df$RTLX.Score)), add = TRUE, col = "blue")

# Reset the graphics window layout
par(mfrow = c(1, 1))


# Correlation Analysis
# Calculate and visualize the correlation between 'SUS.Score' and 'RTLX.Score'
corr_coeff <- cor(df$SUS.Score, df$RTLX.Score, method = "pearson")
ggplot(data = df, aes(x = SUS.Score, y = RTLX.Score)) +
  geom_point() +
  theme_minimal() +  # Use a minimal theme
  labs(
    x = "System Usability (SUS)",
    y = "Cognitive Overload (RTLX)"
  ) +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

correlation_result <- corr.test(df$SUS.Score, df$RTLX.Score, method = "pearson")

# Get the p-value
p_value <- format(correlation_result$p, scientific = FALSE)

# Report the results
print(correlation_result)
cat("The p-value for the correlation is:", p_value, "\n")