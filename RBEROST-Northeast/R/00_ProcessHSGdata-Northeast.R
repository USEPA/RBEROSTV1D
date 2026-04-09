###########################################################################################
# PURPOSE: Calculate Average Infiltration Rates by COMID to Pass to Urban BMP Efficiency Curves ####
# BY: Cathy Chamberlin ####
# DATE: 1/19/2021 ####
###########################################################################################

# Lines of code tagged with #*# may need to be editted by the user.


#####################################################
# 1. Setup ####
#####################################################

packages <- c('tidyverse', 'foreach')
# lapply(packages, install.packages) #*# # May need to be run if packages are not installed
lapply(packages, library, character.only = TRUE)

source("./RBEROST-Northeast/R/Optimization_HelperFunctions-Northeast.R")

#####################################################
# 2. Load Data ####
#####################################################

# Read in data that was exported from ArcGIS
# These .txt files were created using the GetHSGofDevLand model

data.folder <- "./Data/" #*#

NATSGO.A.raw <- read.csv(file = paste0(data.folder, "NATSGO_HSG-A_byCOMID.txt")) #*#
NATSGO.B.raw <- read.csv(file = paste0(data.folder, "NATSGO_HSG-B_byCOMID.txt")) #*#
NATSGO.C.raw <- read.csv(file = paste0(data.folder, "NATSGO_HSG-C_byCOMID.txt")) #*#
NATSGO.D.raw <- read.csv(file = paste0(data.folder, "NATSGO_HSG-D_byCOMID.txt")) #*#

SSURGO.A.raw <- read.csv(file = paste0(data.folder, "SSURGO_HSG-A_byCOMID.txt")) #*#
SSURGO.B.raw <- read.csv(file = paste0(data.folder, "SSURGO_HSG-B_byCOMID.txt")) #*#
SSURGO.C.raw <- read.csv(file = paste0(data.folder, "SSURGO_HSG-C_byCOMID.txt")) #*#
SSURGO.D.raw <- read.csv(file = paste0(data.folder, "SSURGO_HSG-D_byCOMID.txt")) #*#

######################################################
# 3. Combine data long-form ####
######################################################

# This will make the data more workable

dat.long <- as.data.frame(
  foreach(i = c("NATSGO", "SSURGO"), .combine = 'rbind') %do% {
    foreach(j = c("A", "B", "C", "D"), .combine = 'rbind') %do% {
      dat <- eval(parse(text = paste(i, j, "raw", sep = ".")))
      
      dat.munged <- dat %>%
        select(comid = FEATURE, area_m2 = AREA) %>%
        mutate(source = i, HSG = j)
      
      dat.munged
    }
  }
)

#######################################################
# 4. Assign infiltration rates ####
#######################################################

# Data source: https://directives.sc.egov.usda.gov/OpenNonWebContent.aspx?content=17757.wba, pg 12 of pdf
# USDA NRCS Part 630 Hydrology National Engineering Handbook, Chapter 7 Hydrologic Soil Groups pg 7-4 (210-V1-NEH, May 2007)
# NH BMPs calculated at 0.17 in/hr, 0.27 in/hr, 0.52 in/hr, 1.02 in/hr, 2.41 in/hr and 8.27 in/hr


dat.hsgvariability <- dat.long %>%
  mutate(
    InfltRate = case_when(
      HSG == "A" ~ 5.68, #*# # satisfies cases for deep [> 1.42] & shallow [> 5.67] soils
      HSG == "B" ~ 1.42, #*# # almost satisfies cases for deep [1.42 >= x > 0.57] & shallow [5.67 >=  x > 1.42] soils
      HSG == "C" ~ 0.57, #*# # satisfies cases for deep [0.57 >= x > 0.06] & shallow [1.42 >= x > 0.14] soils
      HSG == "D" ~ 0.06 #*# # satisfies cases for deep [<= 0.06 in/hr] & shallow [ <= 0.14 in/hr] soils
    )
  ) %>%
  rowwise() %>%
  mutate(
    InfltRate_sample = list(case_when(
      HSG == "A" ~ c(runif(10, 1.42, 8.27)), #*# # union of cases for deep [> 1.42] & shallow [> 5.67] soils
      HSG == "B" ~ c(runif(10, 0.57, 5.67)), #*# # union of satisfies cases for deep [1.42 >= x > 0.57] & shallow [5.67 >=  x > 1.42] soils
      HSG == "C" ~ c(runif(10, 0.06, 1.42)), #*# # union of cases for deep [0.57 >= x > 0.06] & shallow [1.42 >= x > 0.14] soils
      HSG == "D" ~ c(runif(10, 0, 0.14)) #*# # union of cases for deep [<= 0.06 in/hr] & shallow [ <= 0.14 in/hr] soils
    )
    )
    )%>%
  group_by(comid, source) %>%
  mutate(
    totarea = sum(area_m2),
    dist_for_resample = list(rep(x = HSG, times = area_m2))
  ) %>%
  rowwise() %>%
  mutate(
    area_resample = list(
      foreach(i = 1:10, .combine = "c") %do% {
          (sum(sample(dist_for_resample, 10, replace = TRUE) == HSG) / 10)
      }
    ),
    weighted_infl = list(
      foreach(i = 1:10, .combine = "c") %do% {
        area_resample[i] * InfltRate_sample[i]
      }
    )
  )

dat.infl <- dat.hsgvariability %>%
  ungroup() %>%
  group_by(comid, source) %>%
  summarize(
    totarea = sum(area_m2),
    weightedinfl = sum(area_m2 * InfltRate),
    weightedavginfl_dist = list(
      foreach(
        i = 1:10, .combine = "c"
        ) %do% {sum(unlist(my_index(weighted_infl, i)))}
      )
  ) %>%
  mutate(avg.infl = weightedinfl / totarea)

#######################################################
# 5. Check for differences between SSURGO and NATSGO  ####
#######################################################

dat.SSURGO <- dat.infl %>% filter(source == "SSURGO") %>% select(-source)
dat.NATSGO <- dat.infl %>% filter(source == "NATSGO") %>% select(-source)

# which(
#   !(
#     dat.SSURGO %>% select(comid, avg.infl) == 
#       dat.NATSGO %>% select(comid, avg.infl)
#     )
#   )

identical(
  dat.SSURGO %>% select(comid, avg.infl), dat.NATSGO %>% select(comid, avg.infl)
  ) # I will use NATSGO since it is more complete nationally


#######################################################
# 6. Write NATSGO data ####
#######################################################

dat.write <- dat.NATSGO %>% #*# #User may choose to use SSURGO instead
  mutate(weightedavginfl_dist = paste(weightedavginfl_dist, sep = ",")) %>%
  select(
    comid, 
    infiltrationrate_inperhr = avg.infl, 
    infiltrationrate_inperhr_dist = weightedavginfl_dist
    )

write.csv(dat.write, file = "./RBEROST-Northeast/Preprocessing/Inputs/NHD+infiltrationrates.csv") #*#
