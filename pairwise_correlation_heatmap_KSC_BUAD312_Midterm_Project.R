library(tidyverse)
library(moderndive)
library(ggplot2)

# Contributed by Kyle Cacha
# Selects only numeric variables from the dataset
tracks_num <- tracks_simplified |>
  select(where(is.numeric))

# Calculate pairwise correlations
cor_matrix <- cor(tracks_num,
                  use = "pairwise.complete.obs")

# Convert correlation matrix to a dataframe
cor_data <- as.data.frame(as.table(cor_matrix))

# Contributed by Kyle Cacha
# Creating a Pairwise Correlation Heatmap

# Changes the color scheme to white for negative correlations, light green for correlations near zero, and dark green for positive correlations.

ggplot(cor_data, aes(Var1, Var2, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Freq, 2)), size = 2) +
  scale_fill_gradient2(low = "white", mid = "lightgreen",
                       high = "darkgreen", midpoint = 0,
                       limits = c(-1, 1)) +
  labs(title = "Pairwise Correlation Heatmap",
       x = "", y = "", fill = "Correlation") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        axis.text.y = element_text(size = 8),
        panel.grid = element_blank()) +
  coord_equal()
