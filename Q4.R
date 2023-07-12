# Load the required libraries
library(ggplot2)
library(gridExtra)

# Set the dimensions of the dataset
num_rows <- 600
num_columns <- 15

# Set the value range for each column
value_range <- c(-100, 100)

# Create the random dataset
set.seed(42)  # Set the random seed for reproducibility
dataset <- matrix(runif(num_rows * num_columns, min = value_range[1], max = value_range[2]), nrow = num_rows)

# Convert the dataset to a data frame
dataset_df <- as.data.frame(dataset)

# (i) Plot scatter graph between Column 5 and Column 6
scatter_plot <- ggplot(dataset_df, aes(x = V5, y = V6)) +
  geom_point() +
  labs(title = "Scatter Plot: Column 5 vs Column 6")

# (ii) Plot histogram of each column in a single graph
histograms <- lapply(dataset_df, function(column) {
  ggplot(data.frame(x = column), aes(x)) +
    geom_histogram(binwidth = 10, fill = "lightblue", color = "black") +
    labs(title = paste("Histogram of", names(column)))
})
histogram_plot <- grid.arrange(grobs = histograms, ncol = 5)

# (iii) Plot the Box plot of each column in a single graph
boxplots <- lapply(dataset_df, function(column) {
  ggplot(data.frame(x = column), aes(y = x)) +
    geom_boxplot(fill = "lightblue", color = "black") +
    labs(title = paste("Box Plot of", names(column)))
})
boxplot_plot <- grid.arrange(grobs = boxplots, ncol = 5)

# Display the plots
scatter_plot
histogram_plot
boxplot_plot
