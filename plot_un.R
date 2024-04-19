# Load necessary library
library(ggplot2)
library(reshape2)  # for melt function
library(viridis)

# Prepare the matrix
result[lower.tri(result)] <- NA  # mask lower triangular part

# Melt the matrix into long format
df <- melt(result)
df <- na.omit(df)  # remove NA values to avoid plotting lower triangle

ggplot(df, aes(x=Var2, y=Var1, fill=value)) +
  geom_tile(color = "white") +  # Add white lines to distinguish the tiles
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "Upper Triangular Matrix", x = "Column Index", y = "Row Index") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +  # Rotate x-axis labels
  coord_fixed(ratio = 1)  # Keep aspect ratio of 1 to make tiles square

p <- ggplot(df, aes(x=Var2, y=Var1, fill=value)) +
  geom_tile(color = "white") +  # Add white borders to distinguish the tiles
  #scale_fill_gradient(low = "blue", high = "red") +
  scale_fill_viridis(option = "C") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),  # Rotate x-axis labels to vertical
        axis.text.y = element_text(hjust = 1),  # Adjust y-axis labels (optional)
        text = element_text(size = 16),  # Increase text size for better readability
        aspect.ratio = 1,
        panel.background = element_rect(fill = "white"), plot.background = element_rect(fill = "white", color = "white")
  )  # Ensure that tiles are square

# Save the plot
ggsave("result.png", p, width = 10, height = 10, units = "in", dpi = 300)
