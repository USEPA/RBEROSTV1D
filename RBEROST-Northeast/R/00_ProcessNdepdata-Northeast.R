###########################################################################################
# PURPOSE: Calculate Change in N-deposition 
# BY: Cathy Chamberlin
# DATE:  12/21/2020
###########################################################################################

# This code analysis follows work in ArcGIS to calculate the centroid of each COMID polygon, and extract the values from the 2012 & 2019 TDEP raster grids from those points
# Lines of code tagged with #*# may need to be editted by the user.

#######################################################
# 1. Setup
#######################################################
packages <- c('sf', 'tidyverse')
# lapply(packages, install.packages) #*# # Run this line of code if packages are not installed
lapply(packages, library, character.only = TRUE)

#######################################################
# 2. Load Data
#######################################################

ndep_postarcgis <- read_sf("./Data/ComidCentroidNDEP.dbf") #*# #The relative pathway should work if code is run from the RStudio Project.

#######################################################
# 3. Format Data
#######################################################

ndep <- as.data.frame(ndep_postarcgis) %>%
  select(
    comid = FEATUREID, TDEP_TN_2012 = ndep_2012_, TDEP_TN_2019 = ndep_2019_
    ) %>%
  mutate(Change_2012_2019 = (TDEP_TN_2012 - TDEP_TN_2019) / TDEP_TN_2012) %>%
  arrange(comid)

########################################################
# 4. Write Data
########################################################

write.csv(
  ndep, 
  file = "./RBEROST-Northeast/Preprocessing/Inputs/NdepChange_2012_2019.csv", #*# The relative pathway should work if code is run from the RStudio Project
  row.names = FALSE
  )
  
