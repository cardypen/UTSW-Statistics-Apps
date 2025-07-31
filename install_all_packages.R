#install all packages here


if (!requireNamespace("stringr", quietly = TRUE)) {
  install.packages("stringr")
}
# Load necessary library
library(stringr)

# Function to extract package names from library() calls
extract_libraries <- function(file) {
  lines <- readLines(file, warn = FALSE)
  libs <- str_match(lines, "library\\((['\"]?)([A-Za-z0-9\\.]+)\\1\\)")
  unique(na.omit(libs[,3]))
}

# Main function to collect all library calls
collect_libraries <- function(root_dir = ".") {
  # Get all .R files recursively
  r_files <- list.files(root_dir, pattern = "\\.R$", recursive = TRUE, full.names = TRUE)
  
  # Extract libraries from each file
  lib_list <- lapply(r_files, extract_libraries)
  
  # Combine and get unique package names
  unique(unlist(lib_list))
}

# Example usage
required_packages <- collect_libraries(dirname(getwd()))

# required_packages <- c(
#   "shiny",
#   "shinycssloaders",
#   "shinythemes",
#   "shinyjs",
#   "DT",
#   "tidyverse",
#   "readxl",
#   "dplyr",
#   "htmltools",
#   "gtsummary",
#   "gt",
#   "ggpubr",
#   "usethis",
#   "remotes",
#   "here",
#   "irr",
#   "vcd",
#   "boot",
#   "DescTools",
#   "BlandAltmanLeh",
#   "gridExtra",
#   "grDevices",
#   "MRMCsamplesize",
#   "Hmisc",
#   "hablar",
#   "psych",
#   "writexl"
# )

# Function to check and install packages

install_if_missing <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    # Get list of available packages from CRAN (only once)
    if (!exists(".cran_pkgs", envir = .GlobalEnv)) {
      assign(".cran_pkgs", rownames(available.packages()), envir = .GlobalEnv)
    }
    
    # Check if the package is in CRAN
    if (package %in% get(".cran_pkgs", envir = .GlobalEnv)) {
      message("Installing ", package, " from CRAN...")
      install.packages(package)
    } else {
      message("Skipping ", package, ": not available on CRAN.")
    }  
  }
}


# Install all required packages
invisible(lapply(required_packages, install_if_missing))

remotes::install_github("Mathematinho/blandPower")

if(!requireNamespace("rel")) install.packages(here::here("rel_1.4.2.tar.gz"), repos = NULL, type = "source")
