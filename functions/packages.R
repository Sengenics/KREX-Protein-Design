# packages.R
# Automatic package installation and loading for Protein Analysis Shiny App

# List of required packages (deduplicated from your list)
required_packages <- c(
  "shiny",
  "readxl",
  "DT",
  "dplyr",
  "stringr",
  "httr",
  "jsonlite",
  "openxlsx",
  "data.table",
  "ggplot2",
  "plotly",
  "r3dmol",
  "shinycssloaders",
  "officer",          # For Word document generation
  "flextable"         # For formatted tables in Word
)

# Function to check and install packages
install_if_missing <- function(packages) {
  new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  
  if(length(new_packages) > 0) {
    cat("Installing missing packages:", paste(new_packages, collapse = ", "), "\n")
    install.packages(new_packages, dependencies = TRUE, repos = "https://cloud.r-project.org/")
  } else {
    cat("All required packages are already installed.\n")
  }
}

# Install missing packages
install_if_missing(required_packages)

# Load all packages
cat("Loading packages...\n")
invisible(lapply(required_packages, function(pkg) {
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
  cat("  ✓", pkg, "\n")
}))

cat("\nAll packages loaded successfully!\n")

# Optional: Print package versions for reproducibility
cat("\n=== Package Versions ===\n")
for(pkg in required_packages) {
  version <- packageVersion(pkg)
  cat(sprintf("%-20s %s\n", pkg, version))
}

# Clean up
rm(required_packages, install_if_missing)