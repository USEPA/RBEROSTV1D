###########################################################################################
# TITLE: Postprocessing Shiny App Run File
# PURPOSE: To allow users to input a text file and see the results of their NEOS optimization
# BY: Cathy Chamberlin
# DATE: 2/16/2021
###########################################################################################

# This file is intended to be run by itself. Users can select the green 'Run App' button, run each line of code individually, or source the document.
# Lines marked #*# may be changed by the user.

# Current input data refers to Northeastern Regional SPARROW loading data and land use data specific to Vermont and New Hampshire. For other states, users will need to build out the UserInferface and Server files.

##################################################
# 1. Setup
##################################################

packages <- c(
  "tidyverse",
  "shiny", 
  "foreach", 
  "shinycssloaders", 
  "data.table"
  )

# lapply(packages, install.packages) #*# Must install packages before running in R. Remove "#" symbols for "lapply(packages, install.packages)" line of code before running code.
invisible(
  suppressPackageStartupMessages(
    lapply(packages, library, character.only = TRUE)
    )
)

##################################################
# 2. Source User Interface and Server files
##################################################
keep <- c("working_dir")

# Only remove objects that are not in the 'keep' vector
# This allows the code to be run multiple times in a single R session without error
rm(list = setdiff(ls(), keep)) # Clears R memory so the code can be run multiple times in a single R session

shinyapplocation <- paste0(working_dir, "RBEROST-Northeast/R/")

source(paste0(shinyapplocation, "Optimization_UserInterfaceFile-Northeast.R"), local = TRUE) #*#
source(paste0(shinyapplocation, "Optimization_ServerFile-Northeast.R"), local = TRUE) #*#
# fixed - increased maxRequestSize to accomodate updated sparrow data
options(shiny.maxRequestSize = 110 * 1024 ^ 2, scipen = 999) # These option allow the SPARROW and StreamCat files to be uploaded, and prevent numeric data from being printed in scientific notation


##################################################
# 3. Create Shiny app
##################################################
app <- shinyApp(ui, server)

runApp(app, launch.browser = TRUE)

