if (!requireNamespace("shiny", quietly = TRUE)) {
  install.packages("shiny")
}

library(shiny)

# Get all .R files in the current directory
files <- list.files(pattern = "\\.R$", full.names = TRUE)

# Rank the files in ascending order based on their names
sorted_files <- sort(files)

# Check if there are any .R files
if (length(sorted_files) == 0) {
  stop("No .R files found in the current directory.")
}

# Load the latest version of the Shiny app
# Assuming the latest version is the last in the sorted list
latest_app_file <- sorted_files[length(sorted_files)]

# Source the latest app
source(latest_app_file)

shinyApp(ui = ui, server = server)