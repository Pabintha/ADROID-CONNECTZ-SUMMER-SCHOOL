# Load the required libraries
library(ggplot2)
library(reshape2)
library(gplots)

# Set the dimensions of the dataset
num_rows <- 100
num_columns <- 30

# Set the range of values
min_value <- 1
max_value <- 200

# Create the random dataset
set.seed(42)  # Set the random seed for reproducibility
dataset <- matrix(runif(num_rows * num_columns, min_value, max_value), nrow = num_rows)

# (i) Replace values within the range [10, 60] with NA
dataset[dataset >= 10 & dataset <= 60] <- NA

# Count the number of rows having missing values
rows_with_na <- rowSums(is.na(dataset)) > 0
num_rows_with_na <- sum(rows_with_na)

# Print the count of rows having missing values
cat("Number of rows with missing values:", num_rows_with_na, "\n")

# (ii) Replace NA values with column means
column_means <- colMeans(dataset, na.rm = TRUE)
dataset[is.na(dataset)] <- column_means[col(dataset, na.rm = TRUE)]

# (iii) Find the Pearson correlation among all the columns and plot a heat map
cor_matrix <- cor(dataset)
heatmap(cor_matrix, main = "Pearson Correlation Heatmap")

# Select columns having correlation <= 0.7
columns_to_keep <- which(apply(cor_matrix, 2, function(x) all(x <= 0.7)))
dataset_selected <- dataset[, columns_to_keep]

# (iv) Normalize all the values in the dataset between 0 and 10
normalized_dataset <- scale(dataset_selected, center = FALSE, scale = apply(dataset_selected, 2, max) - apply(dataset_selected, 2, min)) * 10

# (v) Replace all the values in the dataset with 1 if value <= 0.5 else with 0
binary_dataset <- ifelse(normalized_dataset <= 0.5, 1, 0)

# Print the binary dataset
print(binary_dataset)

