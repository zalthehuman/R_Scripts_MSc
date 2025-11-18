# Load necessary libraries
library(tidyverse)

# Load the data
data <- read.csv("res2.csv")


# Fit a polynomial model of degree 4
model_NH3 <- lm(AlgalCount ~ poly(NH3, 4, raw=TRUE), data=data)

# Plot the data and the model
ggplot(data, aes(x=NH3, y=AlgalCount)) +
  geom_point() +
  geom_smooth(method="lm", formula=y ~ poly(x, 4, raw=TRUE), se=FALSE, col="blue") +
  ggtitle("Polynomial Fit for NH3")


# Calculate derivatives
data$NH3_sorted <- sort(data$NH3)
fit <- predict(model_NH3, newdata = list(NH3 = data$NH3_sorted), type = "response")
derivatives <- diff(fit) / diff(data$NH3_sorted)

# Plot the derivatives
plot(data$NH3_sorted[-1], derivatives, type = "l", col = "red", xlab = "NH3", ylab = "Rate of Change")


# Finding peaks and troughs could be done using additional packages or custom logic
# Example using simple thresholding might involve checking for large changes in the derivatives
significant_changes <- which(abs(derivatives) > some_threshold)


# Assuming'derivatives' calculated as shown previously
std_derivatives <- sd(derivatives)  # Calculate standard deviation of the derivatives

# Choose a threshold, e.g., 1.5 times the standard deviation
some_threshold <- 1.5 * std_derivatives

# Identify significant changes
significant_changes <- which(abs(derivatives) > some_threshold)

# Optionally, plot these significant changes to visually verify them
plot(data$NH3_sorted[-length(data$NH3_sorted)], derivatives, type = "l", col = "red", main = "Derivative of NH3")
points(data$NH3_sorted[-length(data$NH3_sorted)][significant_changes], derivatives[significant_changes], col = "blue", pch = 19)


# Assuming 'x_seq' and 'derivatives' are your data for plotting
plot(x_seq, derivatives, type = 'l', col = 'red', lwd = 2, main = "Derivative of NH3",
     xlab = "NH3", ylab = "Derivative", xlim = c(0, max(x_seq)), ylim = c(min(derivatives), max(derivatives)))

# Adding a grid for better visibility
grid(nx = NULL, ny = NULL, col = "gray", lty = "dotted")

# Making sure the labels and title are clear
title(main = "Derivative of NH3, col.main = "black", font.main = black)

