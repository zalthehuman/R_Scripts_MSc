# Load the necessary libraries
library(ggplot2)
library(tidyr)


algal_counts <- data.frame(
  Month = c('January', 'February', 'March', 'April', 'May', 'June',
            'July', 'August', 'September', 'October', 'November', 'December'),
  Season = c('Dry', 'Dry', 'Dry', 'Dry', 'Dry', 'Wet',
             'Wet', 'Wet', 'Wet', 'Wet', 'Wet', 'Dry'),
  `2017` = c(9813, 1010, 26791, 3106, 13930, 35392,
             18021, 2884, 53924, 57616, 336456, 33098),
  `2018` = c(2630, 320, 930, 8767, 4074, 3292,
             24672, 40838, 412059, 17427, 3354, 58151),
  '2019' = c(16674, 29659, 1322, 2458, 94992, 186554, 14010, 3991, 5447, 2298, 8006, 1029),
  '2020' = c(3263, 38280, NA, NA, NA, NA, NA, NA, 61989, 160241, 30681, 1419),
  '2021' = c(6604, 28090, 96652, NA, 19910, 20125, 26788, 7795, 27896, 28648, 14504, 9471),
  '2022' = c(NA, 472033, 6304, 12276, 15618, 127298, 122370, 119172, 8613, 535, 1931, 576)
)
  # ... (continue for the other years)

# Convert the data from wide to long format for ggplot2 using pivot_longer()
algal_long <- pivot_longer(
  algal_counts,
  cols = -c(Month, Season),
  names_to = "Year",
  values_to = "Algal_Count"
)

# Create the boxplot using ggplot2
boxplot <- ggplot(algal_long, aes(x = Season, y = Algal_Count, fill = Season)) +
  geom_boxplot() +
  facet_wrap(~ Year, scales = 'free_y') + # Facet by Year, with independent y scales
  scale_fill_manual(values = c("Dry" = "skyblue", "Wet" = "salmon")) + # Manual color definitions
  theme_minimal() +
  labs(title = "Seasonal Algal Count Variation",
       y = "Algal Count") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(face = "bold"),
        legend.position = "none") # Remove the legend if not needed

# Print the plot
print(boxplot)

# Save the plot to a file
ggsave("algal_counts_boxplot.png", boxplot, width = 10, height = 6)

