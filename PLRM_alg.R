# Install the segmented package if it's not already installed
if (!require(segmented)) {
  install.packages("segmented", dependencies = TRUE)
  library(segmented)
} else {
  library(segmented)
}

# Proceed with your previous script
set.seed(0)
x <- seq(0, 100, length.out = 100)
y <- ifelse(x < 50, 2 * x + rnorm(100, 0, 10), 5 * (x - 50) + 150 + rnorm(100, 0, 10))

# Create a data frame
data <- data.frame(AlgalCount = x, Response = y)

# Fit a linear model
lin_mod <- lm(Response ~ AlgalCount, data = data)

# Apply piecewise regression
seg_mod <- segmented(lin_mod, seg.Z = ~AlgalCount, psi = list(AlgalCount = 50))

# Summary of the model
summary(seg_mod)

# Plotting the results
plot(data$AlgalCount, data$Response, pch = 16, col = "blue", main = "Piecewise Linear Regression on Algal Count Data",
     xlab = "Algal Count", ylab = "Response Variable")
lines(seg_mod, col = "red", lwd = 2)
abline(v = seg_mod$psi[2], col = "green", lty = 2)
legend("topleft", legend = c("Data points", "Piecewise Linear Fit", "Breakpoint"),
       col = c("blue", "red", "green"), pch = c(16, NA, NA), lty = c(NA, 1, 2))
