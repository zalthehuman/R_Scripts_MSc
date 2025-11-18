# Load required library
library(ggplot2)

# Create the data frame
algal_counts <- seq(0, 1, length.out = length(scores))
scores <- c(0.15, 0.15, 0.0858, 0.15, 0.1284, 0.0677, 0.1116, 0.15, 0.0402, 0.0359,
            0, 0.072, 0.15, 0.15, 0.15, 0.15, 0.15, 0.15, 0.0912, 0.0583, 0, 0.1138,
            0.15, 0.0353, 0.1167, 0.0792, 0.15, 0.15, 0.0033, 0, 0.128, 0.15, 0.15,
            0.15, 0.15, 0.15, 0.15, 0.0626, 0.0493, 0.0493, 0.0493, 0.0493, 0.0493,
            0.0493, 0.0312, 0, 0.077, 0.15, 0.15, 0.0827, 0.0022, 0.0493, 0.1051,
            0.1044, 0.0858, 0.15, 0.0832, 0.0814, 0.1258, 0.15, 0.0493, 0, 0.15,
            0.1366, 0.121, 0, 0, 0, 0.15, 0.15, 0.15, 0.15)
data <- data.frame(AlgalCount = algal_counts, Score = scores)

# Fit a polynomial regression model
model <- lm(Score ~ poly(AlgalCount, degree = 4, raw = TRUE), data = data)

# Predict values for a smooth curve
pred_data <- data.frame(AlgalCount = seq(min(data$AlgalCount), max(data$AlgalCount), length.out = 300))
pred_data$Score <- predict(model, newdata = pred_data)

# Calculate the first derivative of the polynomial fit
library(pracma)
pred_data$Derivative <- diff(c(0, pred_data$Score)) / diff(c(0, pred_data$AlgalCount))

# Plotting
ggplot(data, aes(x = AlgalCount, y = Score)) +
  geom_point(color = "blue") +
  geom_line(data = pred_data, aes(y = Score), color = "red") +
  geom_line(data = pred_data, aes(y = Derivative * 10), color = "green") + # Scaled for visibility
  labs(title = "Polynomial Fit and Derivative Analysis of Algal Count",
       x = "Normalized Algal Count", y = "Score / Derivative (scaled)") +
  theme_minimal()
