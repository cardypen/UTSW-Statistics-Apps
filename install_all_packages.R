#install all packages here

required_packages <- c(
  "shiny",
  "shinycssloaders",
  "shinythemes",
  "shinyjs",
  "DT",
  "tidyverse",
  "readxl",
  "dplyr",
  "htmltools",
  "gtsummary",
  "gt",
  "ggpubr",
  "usethis",
  "remotes",
  "here",
  "irr",
  "vcd",
  "boot",
  "DescTools",
  "BlandAltmanLeh",
  "gridExtra",
  "grDevices",
  "MRMCsamplesize",
  "Hmisc",
  "hablar",
  "psych",
  "writexl"
)

# Function to check and install packages

install_if_missing <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    install.packages(package)
  }
}


# Install all required packages
invisible(lapply(required_packages, install_if_missing))

remotes::install_github("Mathematinho/blandPower")

if(!requireNamespace("rel")) install.packages(here::here("rel_1.4.2.tar.gz"), repos = NULL, type = "source")
