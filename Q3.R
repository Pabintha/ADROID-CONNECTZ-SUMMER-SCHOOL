# Load the required libraries
library(cluster)
library(factoextra)

# Set the dimensions of the dataset
num_rows <- 500
num_columns <- 10

# Set the value ranges for each column
value_ranges <- list(
  c(-10, 10),   # Columns 1 to 4
  c(10, 20),    # Columns 5 to 8
  c(-100, 100)  # Columns 9 to 10
)

# Create the random dataset
set.seed(42)  # Set the random seed for reproducibility
dataset <- matrix(NA, nrow = num_rows, ncol = num_columns)

for (i in 1:num_columns) {
  min_value <- value_ranges[[i]][1]
  max_value <- value_ranges[[i]][2]
  dataset[, i] <- runif(num_rows, min_value, max_value)
}

# Apply K-Means clustering and determine optimal number of clusters
kmeans_results <- eclust(dataset, "kmeans", k = 1:10, nstart = 25)
fviz_nbclust(kmeans_results, method = "wss") +
  labs(title = "K-Means Clustering: Optimal Number of Clusters")

# Perform Hierarchical clustering
hierarchical_results <- eclust(dataset, "hclust", method = "ward.D2")
fviz_nbclust(hierarchical_results, method = "wss") +
  labs(title = "Hierarchical Clustering: Optimal Number of Clusters")

