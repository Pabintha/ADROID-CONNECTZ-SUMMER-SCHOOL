# Set the number of lines in the file
num_lines <- 1000

# Set the length of each random string
string_length <- 10

# Generate random strings
random_strings <- replicate(num_lines, paste(sample(letters, string_length, replace=TRUE), collapse=""))

# Write the random strings to a file
file_path <- "random_strings.txt"
writeLines(random_strings, file_path)

# Print the file path
cat("File created:", file_path, "\n")
