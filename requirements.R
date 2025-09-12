# requirements.R
# Install required packages for the Protein Q&A + AlphaFold Shiny app

# CRAN packages
cran_packages <- c(
  "shiny",
  "readxl",
  "dplyr",
  "tidyr",
  "purrr",
  "httr",
  "jsonlite",
  "DT",
  "readr"
)

# Install missing CRAN packages
for (pkg in cran_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
}

# GitHub packages
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes", repos = "https://cloud.r-project.org")
}

# NGLVieweR (AlphaFold viewer) – GitHub only
if (!requireNamespace("NGLVieweR", quietly = TRUE)) {
  remotes::install_github("gnahraff/NGLVieweR")
}

message("✅ All required packages are installed.")
