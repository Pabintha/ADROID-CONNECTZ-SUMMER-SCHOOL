# Load the required libraries
library(stats)

# Set the dimensions of the dataset
num_rows <- 500
num_columns <- 5

# Set the value range for each column
value_range <- c(5, 10)

# Create the random dataset
set.seed(42)  # Set the random seed for reproducibility
dataset <- matrix(runif(num_rows * num_columns, min = value_range[1], max = value_range[2]), nrow = num_rows)

# Convert the dataset to a data frame
dataset_df <- as.data.frame(dataset)

# (i) Perform t-Test on each column
ttest_results <- lapply(dataset_df, function(column) {
  t.test(column, mu = mean(dataset_df$V1))
})

# (ii) Perform Wilcoxon Signed Rank Test on each column
wilcoxon_results <- lapply(dataset_df, function(column) {
  wilcox.test(column, mu = median(dataset_df$V1), paired = TRUE)
})

# (iii) Perform Two Sample t-Test and Wilcoxon Rank Sum Test on Column 3 and Column 4
column3 <- dataset_df$V3
column4 <- dataset_df$V4

ttest_2samp <- t.test(column3, column4)
wilcoxon_2samp <- wilcox.test(column3, column4)

# Print the results
cat("=== t-Test Results ===\n")
for (i in 1:length(ttest_results)) {
  cat("Column", i, "\n")
  cat("  p-value:", ttest_results[[i]]$p.value, "\n")
  cat("  Null hypothesis rejected:", ttest_results[[i]]$p.value < 0.05, "\n\n")
}

cat("=== Wilcoxon Signed Rank Test Results ===\n")
for (i in 1:length(wilcoxon_results)) {
  cat("Column", i, "\n")
  cat("  p-value:", wilcoxon_results[[i]]$p.value, "\n")
  cat("  Null hypothesis rejected:", wilcoxon_results[[i]]$p.value < 0.05, "\n\n")
}

cat("=== Two Sample t-Test Results ===\n")
cat("p-value:", ttest_2samp$p.value, "\n")
cat("Null hypothesis rejected:", ttest_2samp$p.value < 0.05, "\n\n")

cat("=== Wilcoxon Rank Sum Test Results ===\n")
cat("p-value:", wilcoxon_2samp$p.value, "\n")
cat("Null hypothesis rejected:", wilcoxon_2samp$p.value < 0.05, "\n")

