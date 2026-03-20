###########################################################################################
# RBEROST Project Configuration
# This file automatically activates the renv environment when the project is opened
###########################################################################################

# Activate renv for this project
source("renv/activate.R")

# Optional: Set project-specific options
options(
  repos = c(CRAN = "https://cran.r-project.org"),
  warn = 1,
  scipen = 999
)

cat("RBEROST project loaded. Using renv for package management.\n")
