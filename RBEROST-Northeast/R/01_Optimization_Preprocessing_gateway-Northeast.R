##############################################################
### RBEROST Preprocessing Gateway                          ###
### Date: May 6, 2021                                      ###
### Author: Cathy Chamberlin                               ###
### Purpose: Bridge between RBEROST run file and R scripts ###
##############################################################
setwd(working_dir)
MODE <- "Select"

if(exists("IncludeUncertainty")) {
  if(IncludeUncertainty == TRUE) {
  source(paste0(working_dir,"RBEROST-Northeast/R/01_Optimization_Preprocessing_Uncertainty_Northeast_Seasonal_[PROD].R"))
} else if(IncludeUncertainty == FALSE) {
    source(paste0(working_dir,"RBEROST-Northeast/R/01_Optimization_Preprocessing_Northeast_Seasonal_V6_[PROD].R"))
} else {
    print(
      "Only allowable options are 'IncludeUncertainty = TRUE' or 'IncludeUncertainty = FALSE'. You cannot have both, only one or the other."
    )
  }
} else {
  print(
    "Did you delete the line of code that says 'IncludeUncertainty = TRUE' or 'IncludeUncertainty = FALSE'?"
  )
}
