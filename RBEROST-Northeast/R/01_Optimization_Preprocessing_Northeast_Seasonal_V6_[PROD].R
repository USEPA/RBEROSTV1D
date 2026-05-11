
####################################################################################################### TOP OF CODE ----
# PURPOSE: Develop AMPL files for WMOST scaled-up optimization model screening tool
# Original version:
# Authors: Kate Munson, Yishen Li, Alyssa Le
# Date: 06/19/2020 (Updated 07/1/2020)
# Subsequent Updates (in order)
#   Cathy Chamberlin (ORISE-EPA) 
#     Colin Guider (ICF) & Sam Ennett (ICF) - 2024
#          Sam Ennett (ICF) & Hunter Parker (ICF) - 2025
#             Naomi Detenbeck & Craig Connolly (EPA) - 2026

pacman::p_load("tidyverse", "reshape2", "data.table", "stringr", "foreach", "remotes", "dplyr")

# Scientific notation settings ----
options(scipen = 999)

# source helper functions -----
source("./RBEROST-Northeast/R/Optimization_HelperFunctions-Northeast.R")

use_threshold <- FALSE
threshold <- 1e2

# Conversion factors ----
ft2_to_ac <- 43560 # square feet to acre
yd2_to_ac <- 4840 # square yards to acre
km2_to_ac <- 247.105 # square kilometer to acre

#
#
#
# PART I: Read in user specifications ----

## BMPS specifications: If BMP_Selection field = X, retain BMP for optimization. Otherwise ignore. 
user_specs_BMPs <- fread(
  paste(InPath,user_specs_name,sep=""), data.table = FALSE
)

## Notes:
### Loading targets specification: If TermFlag_X = X, this comid is the most downstream loading target
### If OutofNetworkFlag_X = X, then the only contributing watershed is itself.

## Specifications for loading targets for watersheds in LIS model
user_specs_loadingtargets <- fread(
  paste0(InPath, "01_UserSpecs_loadingtargets.csv")
# The following can be uncommented and used to run a series of user-specified load reductions
# for a watershed if interest without changing the full loadings file
)# %>%   
#mutate(Percent_Reduction = case_when(watershed_name == "Upper Connecticut River" & TN_or_TP == "TP" ~ 0.10))


## Terminal catchments/comids
terminal_comid <- read.csv(paste0(InPath,"01_Preprocessing_Terminal_COMID.csv"))%>% #need to change this line to correctly bring in file
  dplyr::rename("terminal_comid"="comid")%>%
  mutate(
    conc=paste0(watershed_name, terminal_comid))

## All upstream/terminal catchments/comids
upstream_comid <- read.csv(paste0(InPath,"01_Preprocessing_Upstream_COMID.csv"))%>% #need to change this line to correctly bring in file
  select(watershed_name, catchment_comid)%>%
  mutate(
    conc=paste0(watershed_name, catchment_comid)) %>%
  filter(catchment_comid != 7717216) # fixed - catchment not included in SPARROW output for Housatonic R

## Pull in target selection data from RMD
if (MODE=="All") {
  target_selection <- as.vector(unique(user_specs_loadingtargets$watershed_name))
} else if (MODE=="Select") {
  target_selection <- watershed_choices
} else {
  stop("Invalid MODE selection, please select 'All' or 'Select'")
}

## Final dataframe used to pull data for the watersheds that the user wants
final_target_list <- upstream_comid %>%
  merge(., user_specs_loadingtargets, by="watershed_name", all.x=T) %>%
  mutate(
    TerminalFlag=ifelse(conc %in% terminal_comid$conc,"X",NA),
    # Ensure catchment_comid is character type to avoid type mismatch errors
    catchment_comid = as.character(catchment_comid)
  ) %>%
  select(-conc) %>%
  filter(watershed_name %in% target_selection)

#### CTC edited: 1/6/2026 ####

user_specs_loadingtargets_sel <-
  user_specs_loadingtargets %>%
  filter(watershed_name %in% target_selection) %>%
  mutate(Number = match(watershed_name, unique(watershed_name)))

write.csv(user_specs_loadingtargets_sel, paste0(InPath,"01_UserSpecs_loadingtargets_selected.csv"), row.names = F)

#### end edits ####

#
#
## Establish load reduction goal -----

## Note:
### This percent reduction will be applied to total baseline loadings at specified pore point.
### Note to user: If TMDL is associated with a particular load value, calculate percent reduction required to meet target at pore point.

load_perc_reduc_tn <- foreach(i = 1:length(target_selection)) %do% {
  temp_target <- user_specs_loadingtargets %>% 
    filter(TN_or_TP == "TN" & watershed_name == target_selection[i]) %>%
    # group_by(watershed_name) %>%
    summarize(Percent_Reduction = mean(Percent_Reduction, na.rm = TRUE))
  
  temp_target$Percent_Reduction
}

load_perc_reduc_tp <- foreach(i = 1:length(target_selection)) %do% {
  temp_target <- user_specs_loadingtargets %>% 
    filter(TN_or_TP == "TP" & watershed_name == target_selection[i]) %>%
    # group_by(watershed_name) %>%
    summarize(Percent_Reduction = mean(Percent_Reduction, na.rm = TRUE))
  
  temp_target$Percent_Reduction
}

#
#
#
# PART 2: PREPROCESS DATA INPUTS -----

## Read raw SPARROW data inputs -----

### USGS Northeastern SPARROW Model Input Data, 2020: https://www.sciencebase.gov/catalog/item/5d4192aee4b01d82ce8da477
#NOTE: Some HUC12 values are missing in the NE SPARROW dataset causing missing ag efficiencies
#later on due to failed matches so created new comid-HUC12 file for LIS comids
# This is used to fill in missing HUC12s for LIS only in ne_comid_huc12
# An abbreviated version, ne_sparrow_model_input_short.csv, is created that has corrected HUC12s
# for LIS catchments and only variables needed for RBEROSTv1D so this section only needs to be
# run once, after which ne_sparrow_model_input_short.csv will be read in instead
# In the future if the extent of RBEROSTv1D is expanded to cover other portions of the Northeast
# additional missing HUC12s may need to be filled in and this section re-run.

complete_comid_huc12 <- read.dbf(paste0(InPath,"lic_flowline_centroidinside_NAD83.dbf"), as.is = TRUE) %>%
  mutate(ComID = comid)

# ND: need total banklength to make up for missing stream lengths in Sparrow file
riparian.existingbuffer <- fread(paste0(InPath, "LengthinBuffer_ICF25ND.csv")) #*# updated

ne_comid_huc12 <- fread(paste0(InPath,"ne_sparrow_model_input.csv",sep="")) %>%
  select(ComID,TermFlag,IncAreaKm2,DivFrac,HUC_12,urban_km2) %>% # keep only subset needed for RBEROST
  left_join(complete_comid_huc12, by = "ComID") %>% # fill in missing huc12 values for LIS
  mutate(HUC_12 = if_else(is.na(HUC_12), as.integer64(huc12), HUC_12)) %>%
  mutate(HUC_12_Rev = huc12) %>% 
  rename(comid_old = comid) %>% #rename to prevent conflict in later renaming of ComID
  mutate(urban_km2 = -99) %>% # fill in with missing value to prevent later error message
# ND: urban_km2 to be calculated from streamcat 2019 later
  left_join(riparian.existingbuffer|>select(COMID,totalbanklength_ft), by = c("ComID" = "COMID")) 
# use totalbanklength_ft instead of LENGTHKM in SPARROW which has missing values
# already character with leading zero so don't need to transform as below #*# 
# ne_comid_huc12$HUC_12_Rev <- str_pad(ne_comid_huc12$huc12, width=12, pad="0", side = "left")  #*#

write.csv(ne_comid_huc12,file = paste0(InPath,"ne_sparrow_model_input_short.csv"), row.names = FALSE)

# Fixed - added missing catchment areas for South Shore in SPARROW loading files
library(foreign)
SShore_catchareas <- read.dbf(paste0(working_dir,"RBEROST-Northeast/Preprocessing/Inputs/Catchment_R2_ClipLIS.dbf"), as.is = TRUE) %>% 
  mutate(FEATUREID = as.character(FEATUREID)) %>%
  mutate(DEL_FRACSS = 1)
### USGS Northeastern SPARROW Seasonal Model Output Data
print("Reading in SPARROW TN data...")
sparrow_cons_out_tn <- fread(
#  paste(InPath,"Predict_NoBFlowN_WSeptic.csv",sep=""), data.table = FALSE) %>% #*#
# ND: Use updated loads including S Shore watersheds and final SPARROW model data
  paste(InPath,"Predict_NoBFlowN_WSeptic_final.csv",sep=""), data.table = FALSE) %>%  
  # TN model is missing a total incremental load variable, create one:
  mutate(
    PLOAD_INC_TOTAL = rowSums(
      cbind(
#        PLOAD_INC_PMN, PLOAD_INC_ATN, PLOAD_INC_SCS, PLOAD_INC_URB_NoSeptic,# ND updated name
        PLOAD_INC_PMN, PLOAD_INC_ATN, PLOAD_INC_SCS, PLOAD_INC_URB,       
        PLOAD_INC_BFN, PLOAD_INC_AFN, PLOAD_INC_DFN, PLOAD_INC_AMN,
        PLOAD_INC_STO, SepticLoadtoReachkgN #, PLOAD_INC_ST
      ),
      na.rm = TRUE
    ),
    year_season = str_sub(comid_time, -3),
    year = str_sub(year_season, 1, 2)
  ) %>%
  # Filter years to selected model years
  filter(year %in% model_years) %>%
  rename(in_total = PLOAD_INC_TOTAL, # Rename columns to match static SPARROW
         # Point Source Loads
         in_poin = PLOAD_INC_PMN,
         # Agricultural Loads
         in_fert_ag = PLOAD_INC_AFN,
         in_manu = PLOAD_INC_AMN,
         # Atmospheric Loads
         in_atmo = PLOAD_INC_ATN,
         # Urban Land Loads
#         in_urb = PLOAD_INC_URB_NoSeptic, # ND renamed
         in_urb = PLOAD_INC_URB,         
         in_fert_dev = PLOAD_INC_DFN,
         # Septic load
         in_septic = SepticLoadtoReachkgN
  ) #*#

### Create correct season column
if (any(is.na(sparrow_cons_out_tn$season))) {
  sparrow_cons_out_tn$season <- str_sub(sparrow_cons_out_tn$year_season, 3)
}

# fix type
sparrow_cons_out_tn$season <- as.character(sparrow_cons_out_tn$season)

### Create correct comid column
if (any(is.na(sparrow_cons_out_tn$comid)) | !is.character(sparrow_cons_out_tn$comid)) {
  sparrow_cons_out_tn$comid <- as.character(str_sub(sparrow_cons_out_tn$comid_time, 1, -4))
}

### Add missing S Shore catchment areas and delivery fractions
sparrow_cons_out_tn <- sparrow_cons_out_tn %>%
  left_join(select(SShore_catchareas,FEATUREID,AreaSqKM,DEL_FRACSS),by = c("comid" = "FEATUREID")) %>%
  mutate(
    inc_area = ifelse(is.na(inc_area), AreaSqKM, inc_area) 
  ) %>%
  mutate(
    DEL_FRAC = ifelse(is.na(DEL_FRAC), DEL_FRACSS, DEL_FRAC) 
  )
# sparrow_cons_out_tn_check <- sparrow_cons_out_tn %>% filter(comid == 9509276)

### QA Check: Do all years/seasons have the same number of COMIDs?
if (nrow(sparrow_cons_out_tn %>%
         mutate(year = str_sub(comid_time, -3)) %>%
         group_by(year) %>%
         tally() %>%
         distinct(n)) > 1) {
  stop("Some years/seasons have different numbers of COMIDs for the Nitrogen SPARROW model")
}

## TN standard errors
# tn_sparrow_se <- fread(paste0(InPath,"ne_dynamic_sparrow_se_tn.csv")) #*#
# Updated loads with S Shore watersheds and final SPARROW model loads
tn_sparrow_se <- fread(paste0(InPath,"ne_dynamic_sparrow_se_tn_final.csv")) 

tn_sparrow_se_new <- tn_sparrow_se %>%
  mutate(
    year_season = str_sub(comid_time, -3),
    year = str_sub(year_season, 1, 2),
    season = str_sub(year_season, 3),
    comid = as.character(str_sub(comid_time, 1, -4))
  ) %>%
  select(comid, comid_time, year, year_season, season, starts_with("SE_")) %>%
  select(comid, comid_time, year, year_season, season, contains("INC"),SE_SepticLoadtoReachkgN)

# tn_sparrow_se_new_check <- tn_sparrow_se_new %>% filter(comid == 9509276) #*#

tn_se <- tn_sparrow_se_new %>%
  select(-c(comid_time, year_season)) %>%
  filter(year %in% model_years) %>%
  # added missing septic SE^2 N.D.
  mutate(sin_total = sqrt(SE_PLOAD_INC_ATN ^ 2 + SE_PLOAD_INC_SCS ^ 2 + SE_PLOAD_INC_BFN ^ 2 + SE_PLOAD_INC_STO ^ 2 + SE_PLOAD_INC_PMN ^ 2 + SE_PLOAD_INC_AMN ^ 2 + SE_PLOAD_INC_URB ^ 2 + SE_SepticLoadtoReachkgN ^ 2),
         sin_other = sqrt(SE_PLOAD_INC_ATN ^ 2 + SE_PLOAD_INC_SCS ^ 2 + SE_PLOAD_INC_BFN ^ 2 + SE_SepticLoadtoReachkgN ^ 2),
         sin_storage = SE_PLOAD_INC_STO,
         sin_septic = SE_SepticLoadtoReachkgN,
         sin_fert_ag = 0, # not present in SPARROW data with SE
         sin_fert_dev = 0) %>% # not present in SPARROW data with SE
  rename(
    sin_poin = SE_PLOAD_INC_PMN,
    sin_manu = SE_PLOAD_INC_AMN,
    # Urban Land Loads
    sin_urb = SE_PLOAD_INC_URB 
      ) %>%
  select(comid, year, season, starts_with("sin"))

sparrow_cons_out_tn <- merge(sparrow_cons_out_tn, tn_se, by = c("comid", "year", "season"))

### Rename columns to match static SPARROW
print("Reading in SPARROW TP data...")
sparrow_cons_out_tp <- fread(
#  paste(InPath,"Predict_P.csv",sep=""), data.table = FALSE) %>% #*#
# ND: Updated loadings with S Shore watersheds and final USGS SPARROW loads
 paste(InPath,"Predict_P_final.csv",sep=""), data.table = FALSE) %>% 
  mutate(PLOAD_INC_SED = 0) %>% # ND: N.S. in SPARROW model %>%
  mutate(PLOAD_INC_STO = 0) %>% # ND: N.S. in SPARROW model %>%
  mutate(ip = PLOAD_INC_PMP+PLOAD_INC_SED+PLOAD_INC_SCS+PLOAD_INC_URB+PLOAD_INC_FOR+PLOAD_INC_AFP+PLOAD_INC_ATN+PLOAD_INC_AMP+PLOAD_INC_STO, #+PLOAD_INC_ST 
         year_season = str_sub(comid_time, -3),
         year = str_sub(year_season, 1, 2)) %>%
  # Filter years to selected model years
  filter(year %in% model_years) %>%
  rename(ip_poin = PLOAD_INC_PMP,
         ip_fert = PLOAD_INC_AFP,
         ip_manu = PLOAD_INC_AMP,
         ip_rock = PLOAD_INC_SED,
         ip_forest = PLOAD_INC_FOR,
         ip_atmo = PLOAD_INC_ATN,
         ip_urb = PLOAD_INC_URB) #*#

### Create correct season column
# ND: added this because season is missing as variable
sparrow_cons_out_tp$season <- as.character(str_sub(sparrow_cons_out_tp$year_season, 3))
if (any(is.na(sparrow_cons_out_tp$season))) {
  sparrow_cons_out_tp$season <- as.character(str_sub(sparrow_cons_out_tp$year_season, 3))
}

# fix type
sparrow_cons_out_tp$season <- as.character(sparrow_cons_out_tp$season)

### Create correct comid column
if (any(is.na(sparrow_cons_out_tp$comid)) | !is.character(sparrow_cons_out_tp$comid)) {
  sparrow_cons_out_tp$comid <- as.character(str_sub(sparrow_cons_out_tp$comid_time, 1, -4))
}

### Add missing S Shore catchment areas
sparrow_cons_out_tp <- sparrow_cons_out_tp %>%
  left_join(select(SShore_catchareas,FEATUREID,AreaSqKM,DEL_FRACSS),by = c("comid" = "FEATUREID")) %>%
  mutate(
    inc_area = ifelse(is.na(inc_area), AreaSqKM, inc_area)
  )%>%
  mutate(
    DEL_FRAC = ifelse(is.na(DEL_FRAC), DEL_FRACSS, DEL_FRAC) 
  )

# sparrow_cons_out_tp_check <- sparrow_cons_out_tp %>% filter(comid == 9509276)

### QA Check: Do all years/seasons have the same number of COMIDs?
if (nrow(sparrow_cons_out_tp %>%
         mutate(year = str_sub(comid_time, -3)) %>%
         group_by(year) %>%
         tally() %>%
         distinct(n)) > 1) {
  stop("Some years/seasons have different numbers of COMIDs for the Phosphorus SPARROW model")
}

## TP standard errors
# tp_sparrow_se <- fread(paste0(InPath,"ne_dynamic_sparrow_se_tp.csv")) #*#
# Updated file with final SPARROW model outputs
tp_sparrow_se <- fread(paste0(InPath,"ne_dynamic_sparrow_se_tp_final.csv"))


tp_sparrow_se_new <- tp_sparrow_se %>%
  mutate(
    year_season = str_sub(comid_time, -3),
    year = str_sub(year_season, 1, 2), 
    season = str_sub(year_season, 3),
    comid = as.character(str_sub(comid_time, 1, -4))
  ) %>%
  select(comid, comid_time, year, year_season, season, starts_with("SE_")) %>%
  select(comid, comid_time, year, year_season, season, contains("INC"))

tp_se <- tp_sparrow_se_new %>%
  select(-c(comid_time, year_season)) %>%
  filter(year %in% model_years) %>%
  mutate(sip_other = sqrt(SE_PLOAD_INC_SCS ^ 2 + SE_PLOAD_INC_ATN ^ 2), # KM: SEs for SED and FOR were not included in SPARROW P results
         sip_ag = sqrt(SE_PLOAD_INC_AFP ^ 2 + SE_PLOAD_INC_AMP ^ 2), # KM: SE for ag loads generated by combining fert and manure SEs
         sip_storage = 0) %>% # KN: No storage SEs provided
  rename(
    sip_poin = SE_PLOAD_INC_PMP,
    sip_urb = SE_PLOAD_INC_URB,
    sip_total = SE_PLOAD_INC_TOTAL) %>% ## KM: ok to use total as there are no extra storage loads to remove
  select(comid, year, season, starts_with("sip"))

sparrow_cons_out_tp <- merge(sparrow_cons_out_tp, tp_se, by = c("comid", "year", "season"))

## Prep TN data by season ------------------------------------------------------

### Separate by season
sparrow_cons_out_tn_s <- sparrow_cons_out_tn %>% 
  arrange(year, comid, season) %>%
  select(comid, year, season, in_total, PLOAD_INC_STO)

### Compute transfer coefficients
sm_N <- data.frame(
  sparrow_cons_out_tn_s$comid,
  sparrow_cons_out_tn_s$year,
  sparrow_cons_out_tn_s$season,
  # Calculate transfer coefficients between seasons
  ifelse(
    sparrow_cons_out_tn_s$in_total == 0,
    1,
    ifelse(
      lead(sparrow_cons_out_tn_s$in_total, default = 0) == 0,
      sparrow_cons_out_tn_s$PLOAD_INC_STO / lead(sparrow_cons_out_tn_s$in_total, default = 1),
      sparrow_cons_out_tn_s$PLOAD_INC_STO / sparrow_cons_out_tn_s$in_total
    )
  )
) %>%
  rename(comid = 1,
         year = 2,
         season = 3,
         coeff = 4)

### Overwrite year 20 transfer coefficients, no Y21S1 to compare to
sm_N$coeff <- ifelse(sm_N$year == 20 & sm_N$season == 4, 0, sm_N$coeff)


#
#
## Prep TP Data by Season ---------
sparrow_cons_out_tp_s <- sparrow_cons_out_tp %>% 
  arrange(year, comid, season) %>%
  select(comid, year, season, ip, PLOAD_INC_STO)

sm_P <- data.frame(
  sparrow_cons_out_tp_s$comid,
  sparrow_cons_out_tp_s$year,
  sparrow_cons_out_tp_s$season,
  # Calculate transfer coefficients between seasons
  ifelse(
    sparrow_cons_out_tp_s$ip == 0,
    1,
    ifelse(
      lead(sparrow_cons_out_tp_s$ip, default = 0) == 0,
      sparrow_cons_out_tp_s$PLOAD_INC_STO / lead(sparrow_cons_out_tp_s$ip, default = 1),
      sparrow_cons_out_tp_s$PLOAD_INC_STO / sparrow_cons_out_tp_s$ip
    )
  )
) %>%
  rename(comid = 1,
         year = 2,
         season = 3,
         coeff = 4)

# # Transfer coefficient threshold
if (use_threshold) {
  sm_N[,4][sm_N[,4] > threshold] <- 0
  sm_P[,4][sm_P[,4] > threshold] <- 0
}

# Overwrite year 20 transfer coefficients, no Y21S1 to compare to
sm_P$coeff <- ifelse(sm_P$year == 20 & sm_P$season == 4, 0, sm_P$coeff)
# ND: Set P transfer coefficient to zero because P storage term in dynamic SPARROW
# model is insignificant so all subsequent storage terms should be zero
sm_P$coeff <- 0
#
#
## Read in STREAMCAT API data -----
StreamCat_api <- fread(paste0(InPath, "CT_NH_MA_VT_NY_RI_StreamCatData_2019.csv"))
# Ensure comid is character type to avoid type mismatch errors
StreamCat_api$comid <- as.character(StreamCat_api$comid)
# Note to user: Make sure that both the variable names and associated data match the state(s) that your watershed falls within.

## Data modifications: specify incremental loads as point, urban, agricultural, septic, and other -----

### Change all COMID columns to lower case
names(ne_comid_huc12)[names(ne_comid_huc12) == "ComID"] <- "comid"
# ND comid already present from matched comid-HUC12 dataset for LIS Basin so don't copy over
# Note to user: if making adjustment to SPARROW region, must identify which column names that reflect agricultural, point, and urban 
# nutrient sources based on SPARROW regional model metadata. 

temp_inc_tn <- sparrow_cons_out_tn %>% 
select(c("comid", "season", "year", "in_poin", "in_atmo", "in_urb", 
"in_fert_ag", "in_fert_dev", "in_manu", "in_fert_dev", "in_septic", 
"PLOAD_INC_SCS", "PLOAD_INC_BFN", "PLOAD_INC_ST", "PLOAD_INC_STO")) #*#

### Build ag load stream
temp_inc_tn$in_ag <- with(temp_inc_tn, in_fert_ag+in_manu) #*#

### Build urban load stream
temp_inc_tn$in_urb <- with(temp_inc_tn, in_urb+in_fert_dev) #*#

temp_inc_tn_rev <- temp_inc_tn

temp_inc_tn_rev[is.na(temp_inc_tn_rev)] <- 0

# Also treating storage separately
temp_inc_tn_rev$in_other <- with(temp_inc_tn_rev, in_atmo+PLOAD_INC_SCS+PLOAD_INC_BFN) #*#
temp_inc_tn_rev$in_storage <- with(temp_inc_tn_rev, PLOAD_INC_STO) #*#PLOAD_INC_ST+

inc_tn <- temp_inc_tn_rev %>% 
  select(c("comid", "season", "year", "in_poin", "in_urb", "in_ag", "in_septic", 
  "in_other", "in_storage")) #*#

temp_inc_tp <- sparrow_cons_out_tp
temp_inc_tp$ip_ag <- with(temp_inc_tp, ip_fert+ip_manu) #*#
temp_inc_tp$ip_other <- with(temp_inc_tp, ip_rock+PLOAD_INC_SCS+ip_atmo+ip_forest) #*#
# Again, treating storage separately
temp_inc_tp$ip_storage <- with(temp_inc_tp, PLOAD_INC_STO) #*#

inc_tp <- temp_inc_tp %>% 
  select(c("comid", "season", "year", "ip_poin", "ip_urb", "ip_ag", "ip_other", 
  "ip_storage")) #*#


inc_tn_rev <- inc_tn
inc_tn_rev[is.na(inc_tn_rev)] <- 0


## Specify urban and agricultural areas -----
### Note: change units from km2 to acres

### Select urban area and incremental area from SPARROW input data
temp_sparrow_area <- sparrow_cons_out_tn %>% 
  select(c("comid","inc_area")) %>%
  distinct()

length(unique(temp_sparrow_area$comid)) 

### Select percentage of incremental area that is cropland
streamcat_ag_urban <- StreamCat_api %>% 
select(c("comid","PctCrop2019Cat", "PctUrbHi2019Cat", "PctUrbLo2019Cat", 
"PctUrbMd2019Cat", "PctUrbOp2019Cat")) #*#

### Sum urban sub-designations to get total urban area
#streamcat_ag_urban$urban_pct <- with(streamcat_ag_urban, 
#PctUrbHi2019Cat+PctUrbLo2019Cat+PctUrbMd2019Cat+PctUrbOp2019Cat)
# ND: Revise to be consistent with definition of urban area in dynamic SPARROW
streamcat_ag_urban$urban_pct <- with(streamcat_ag_urban, 
PctUrbHi2019Cat+PctUrbMd2019Cat)

### Convert the sparrow input area from KM to acres
temp_sparrow_area$inc_ac <- temp_sparrow_area$inc_area*km2_to_ac #*#

### Use the streamcat area percentages to idenitfy ag and urban areas in each catchment
# temp_area <- merge(temp_sparrow_area, streamcat_ag_urban, by="comid")
temp_area <- left_join(temp_sparrow_area, streamcat_ag_urban, by="comid") #ND get rid of excess LU data not in area of interest

# Fill missing values with zeros
temp_area[is.na(temp_area)] <- 0

# Calculate agricultural and urban areas
temp_area$ag_ac <- with(temp_area,(PctCrop2019Cat/100)*inc_ac) #*#
temp_area$urban_ac <- with(temp_area,(urban_pct/100)*inc_ac) #*#

# Get the actual COMID values that appear multiple times
duplicate_comid_values <- temp_area %>%
  count(comid) %>%
  filter(n > 1) %>%
  pull(comid)

### Summarize area
area <- temp_area %>% 
  select(c("comid","urban_ac","ag_ac")) %>% 
  group_by(comid) %>%
  summarize(urban_ac = mean(urban_ac, na.rm = T), ag_ac = mean(ag_ac, na.rm = T), .groups = "drop")

#
#
## Septic Upgrades and Sewer Conversions Data -----

### Eventually want to convert this to a standalone input included
class0ups <- readxl::read_xlsx(paste0(InPath, "Unclassified_Septic_Parcels_LIS.xlsx" )) %>%
# Include septic data for S Shore
# class0ups <- readxl::read_xlsx(paste0(InPath, "Unclassified_Septic_Parcels_LIS_ND.xlsx" )) %>%  
  mutate(
    Upgrade_Efficiency = 0
  )%>%
  mutate(
    comid = as.character(comid),
    SepticUpgradeClass = as.integer(SepticUpgradeClass),
    SepticUpgradeParcels = as.integer(SepticUpgradeParcels),
    Upgrade_Efficiency = as.integer(Upgrade_Efficiency)
  )

### Number of COMID
length(unique(class0ups$comid))

### Read in septic upgrade data
septic.upgrade_raw <- fread(paste0(InPath, "01_RevSepticUpgrde_ParcelEff.csv")) %>%
# Include septic data for S Shore
# septic.upgrade_raw <- fread(paste0(InPath, "01_RevSepticUpgrde_ParcelEff_ND.csv")) %>%
  mutate(
    comid = as.character(comid)
  )
length(unique(septic.upgrade_raw$comid))

### Merge in the number of class 0 parcels per COMID
septic.upgrade_raw <- bind_rows(class0ups%>%filter(comid %in% final_target_list$catchment_comid), septic.upgrade_raw%>%filter(comid %in% final_target_list$catchment_comid))
length(unique(septic.upgrade_raw$comid))

### Format septic upgrades data
septic.upgrade <- septic.upgrade_raw%>%
  rename(parcels = SepticUpgradeParcels, efficiency_per_parcel = Upgrade_Efficiency) %>%
  mutate(efficiency_per_parcel = efficiency_per_parcel/100)# Revise efficiency to be a decimal not a whole number

### Determine total parcels per COMID
septic.upgrade_totals <- septic.upgrade%>%
  group_by(comid)%>%
  summarise(
    total_parcels = sum(parcels)
  )%>%
  ungroup()

### Remove class 0
septic.upgrade <- septic.upgrade %>%
  filter(SepticUpgradeClass != 0)

### Check to make sure there are no NAs in the data
septic_qa <- septic.upgrade_totals%>%
  summarise(across(everything(), ~sum(is.na(.)))) # All 0
rm(septic_qa) # Remove QA check

### Read in sewer conversion data
septic.conversion_raw <- fread(paste0(InPath, "01_RevSepticConv_ParcelEff.csv")) %>%
# use updated septic info including S Shore
# septic.conversion_raw <- fread(paste0(InPath, "01_RevSepticConv_ParcelEff_ND.csv"))%>%  
  filter(comid %in% final_target_list$catchment_comid)
septic.conversion_totals <- septic.conversion_raw%>%
  group_by(comid)%>%
  summarise(
    total_conv_parcels = sum(SewerConversionParcels),
  )
septic.conversion <- septic.conversion_raw%>%
#  filter(SewerConversionClass != 0) %>% # ND keep septic_convert records w zero eff
  # fix: change comid type to character to allow matches
  mutate(comid = as.character(comid)) %>%
  group_by(comid)%>%
  summarise(
    parcels = sum(SewerConversionParcels),
    efficiency_per_parcel = last(Convert_Efficiency)
  ) %>%
  # Revise efficiency to be a decimal not a whole number
  mutate(efficiency_per_parcel = efficiency_per_parcel/100)

total_septic_totals_temp <- merge(septic.upgrade_totals, septic.conversion_totals, by = "comid", all.x = TRUE, all.y = TRUE)

total_septic_totals_temp[is.na(total_septic_totals_temp)] <- 0

total_septic_totals <- total_septic_totals_temp%>%
  mutate(
    final_total = total_parcels + total_conv_parcels)%>%
  select(comid, final_total)
rm(total_septic_totals_temp)

#
#
## Specify the length of streambank already buffered ----

### Preprocessed Riparian Data
# fixed riparian files
riparian.loadings <- fread(paste0(InPath, "RiparianLoadings_ICF25ND.csv")) #*# updated
riparian.existingbuffer <- fread(paste0(InPath, "LengthinBuffer_ICF25ND.csv")) #*# updated
riparian.efficiencies <- fread(paste0(InPath, "RiparianEfficiencies_ICF24.csv")) #*#

if (!is.character(riparian.loadings$COMID)) {
  riparian.loadings$comid <- as.character(riparian.loadings$COMID)
}

# fixed to avoid mismatched types
if (!is.character(riparian.loadings$season)) {
  riparian.loadings$season <- as.character(riparian.loadings$season)
}

 if (!is.character(riparian.existingbuffer$comid)) { 
  riparian.existingbuffer$comid <- as.character(riparian.existingbuffer$comid)
 }
riparian.existingbuffer <- riparian.existingbuffer %>% mutate(comid = as.character(COMID))

if (!is.character(riparian.efficiencies$comid)) {  
  riparian.efficiencies$comid <- as.character(riparian.efficiencies$comid)
}

### Pull riparian buffer details from user specs
RiparianBuffer_BMPs <- with(
  user_specs_BMPs, BMP[which(BMP_Category == "ripbuf"& BMP_Selection == "X")]
) 

### Pull buffer widths from userpsecs
RiparianBuffer_Widths <- with(
  user_specs_BMPs, UserSpec_RD_in[
    which(BMP_Category == "ripbuf"& BMP_Selection == "X")
  ]
) 

### Round userspecs buffer widths
UserSpecs_bufferwidth_nearest <- custom.round(
  x = RiparianBuffer_Widths, breaks = c(20, 40, 60, 80, 100)
)

if(length(RiparianBuffer_BMPs) > 0) {
  riparian_buffer_maximp <- foreach(
    i = 1:length(RiparianBuffer_BMPs), .combine = "merge"
  ) %do% {
    RiparianBuffer_BMPs_tmp <- RiparianBuffer_BMPs[i]
    
    riparian.existingbuffer %>%
      mutate(comid = as.character(COMID)) %>% #ND Fix name so merge will occur
      select(
        comid,
        totalbanklength_ft, 
        contains(as.character(UserSpecs_bufferwidth_nearest[i]))
      ) %>%
      rename_with(
        .fn = ~gsub(
          paste0("_", UserSpecs_bufferwidth_nearest[i], "ft_ft"), '', .
        ), 
        .col = contains(as.character(UserSpecs_bufferwidth_nearest[i]))
      ) %>%
      mutate(
        maximp = case_when(
          RiparianBuffer_BMPs_tmp == "Forested_Buffer" ~ 
            totalbanklength_ft - Forest_buffer,
          RiparianBuffer_BMPs_tmp == "Grassed_Buffer" ~ 
            totalbanklength_ft - Forest_buffer - Grass_buffer
        )
      ) %>%
      mutate(maximp = case_when(maximp < 0 ~ 0, maximp >= 0 ~ maximp)) %>%
      select(comid, totalbanklength_ft, maximp) %>%      
      rename_with(.fn = ~paste(RiparianBuffer_BMPs_tmp), .cols = "maximp")
  }
}

# try with i = 1, then i = 2 and compare lengths
RiparianBuffer_BMPs_tmp <- RiparianBuffer_BMPs[1]

riparian.existingbuffer1 <- riparian.existingbuffer %>%
  select(
    comid,
    totalbanklength_ft, 
    contains(as.character(UserSpecs_bufferwidth_nearest[1]))
  ) %>%
  rename_with(
    .fn = ~gsub(
      paste0("_", UserSpecs_bufferwidth_nearest[i], "ft_ft"), '', .
    ), 
    .col = contains(as.character(UserSpecs_bufferwidth_nearest[1]))
  ) %>%
  mutate(
    maximp = case_when(
      RiparianBuffer_BMPs_tmp == "Forested_Buffer" ~ 
        totalbanklength_ft - Forest_buffer,
      RiparianBuffer_BMPs_tmp == "Grassed_Buffer" ~ 
        totalbanklength_ft - Forest_buffer - Grass_buffer
    )
  ) %>%
  mutate(maximp = case_when(maximp < 0 ~ 0, maximp >= 0 ~ maximp)) %>%
  select(comid, totalbanklength_ft, maximp) %>%      
  rename_with(.fn = ~paste(RiparianBuffer_BMPs_tmp), .cols = "maximp")

# try with i = 2
RiparianBuffer_BMPs_tmp <- RiparianBuffer_BMPs[2]

riparian.existingbuffer2 <- riparian.existingbuffer %>%
  select(
    comid,
    totalbanklength_ft, 
    contains(as.character(UserSpecs_bufferwidth_nearest[2]))
  ) %>%
  rename_with(
    .fn = ~gsub(
      paste0("_", UserSpecs_bufferwidth_nearest[i], "ft_ft"), '', .
    ), 
    .col = contains(as.character(UserSpecs_bufferwidth_nearest[2]))
  ) %>%
  mutate(
    maximp = case_when(
      RiparianBuffer_BMPs_tmp == "Forested_Buffer" ~ 
        totalbanklength_ft - Forest_buffer,
      RiparianBuffer_BMPs_tmp == "Grassed_Buffer" ~ 
        totalbanklength_ft - Forest_buffer - Grass_buffer
    )
  ) %>%
  mutate(maximp = case_when(maximp < 0 ~ 0, maximp >= 0 ~ maximp)) %>%
  select(comid, totalbanklength_ft, maximp) %>%      
  rename_with(.fn = ~paste(RiparianBuffer_BMPs_tmp), .cols = "maximp")


#
#
## Calculate runoff coefficient for urban area -----

# Specify column for impervious dataset, impervious df changed to api df, 2011 variable changed to 2019 (Updated by H. Parker ~ 7/11/23)
temp_runoffcoeff <- StreamCat_api %>% select(c("comid","PctImp2019Cat")) #*#
temp_runoffcoeff$runoffcoeff <- with(
  temp_runoffcoeff, 0.05 + 0.009 * PctImp2019Cat
) #*#

runoffcoeff <- temp_runoffcoeff %>% select(c("comid", "runoffcoeff"))
runoffcoeff <- as.data.frame(distinct(runoffcoeff)) # Removes duplicates
rm(list = ls(pattern = "^temp")) # Remove temporary datasets

#
#
## Limit dataframe to the COMIDs within the specified watershed ------

reaches_TN_target <- final_target_list %>%
  filter(TN_or_TP == "TN")

reaches_TP_target <- final_target_list %>%
  filter(TN_or_TP == "TP")

# Add a error if the COMIDs in the final target list are not within the SPARROW model
if (nrow(reaches_TN_target) > 0 && all(!reaches_TN_target$catchment_comid %in% sparrow_cons_out_tn$comid)) {
  missing_comids_tn <- reaches_TN_target$catchment_comid[!reaches_TN_target$catchment_comid %in% sparrow_cons_out_tn$comid]
  stop(paste0(
    "\n\nERROR: Some COMIDs in the TN target watersheds are not present in the SPARROW TN model data.\n",
    "Missing COMIDs: ", paste(unique(missing_comids_tn), collapse = ", "), "\n",
    "Total missing COMIDs: ", length(unique(missing_comids_tn)), "\n\n",
    "SOLUTION: Please verify that:\n",
    "1. The watershed selection includes only COMIDs present in the SPARROW model\n", 
    "2. The SPARROW input files contain data for all selected watersheds\n",
    "3. The ComID values in the loading targets file match those in SPARROW data\n"
  ))
}

if (exists("reaches_TP_target") && nrow(reaches_TP_target) > 0 && all(!reaches_TP_target$catchment_comid %in% sparrow_cons_out_tp$comid)) {
  if (all(!reaches_TP_target$catchment_comid %in% sparrow_cons_out_tp$comid)) {
    missing_comids_tp <- reaches_TP_target$catchment_comid[!reaches_TP_target$catchment_comid %in% sparrow_cons_out_tp$comid]
    stop(paste0(
      "\n\nERROR: Some COMIDs in the TP target watersheds are not present in the SPARROW TP model data.\n",
      "Missing COMIDs: ", paste(unique(missing_comids_tp), collapse = ", "), "\n",
      "Total missing COMIDs: ", length(unique(missing_comids_tp)), "\n\n",
      "SOLUTION: Please verify that:\n",
      "1. The watershed selection includes only COMIDs present in the SPARROW model\n", 
      "2. The SPARROW input files contain data for all selected watersheds\n",
      "3. The ComID values in the loading targets file match those in SPARROW data\n"
    ))
  }
}

#
#
## Subset data to conceptual model flowpaths ------

### Specify pour point

### Specify comids of loading targets
pore_pt_tn <- with(
  user_specs_loadingtargets %>% filter(TN_or_TP == "TN"),
  as.character(ComID)
) #*#

pore_pt_tp <- with(
  user_specs_loadingtargets %>% filter(TN_or_TP == "TP"),
  as.character(ComID)
) #*#

### Limit dataframe to the COMIDs within the specified watershed
reaches_all <- ne_comid_huc12 %>% 
  filter(
    comid %in% final_target_list$catchment_comid
  )

### Message regarding SPARROW/StreamCat mismatch

streamcat_subset_all <- as.data.frame(runoffcoeff)[
  runoffcoeff$comid %in% 
    unique(final_target_list$catchment_comid), 
] %>%
  select(comid) %>%
  arrange(comid) %>%
  # set comid to character
  mutate(comid = as.character(comid))

streamcat_subset_tn <- foreach(i = 1:length(target_selection)) %do% {
  
  watershed_comid <- reaches_TN_target %>%
    filter(watershed_name == target_selection[i])
  
  streamcat_subset_tn_tmp <- as.data.frame(runoffcoeff)[
    runoffcoeff$comid %in% watershed_comid$catchment_comid, 
  ] %>%
    select(comid) %>%
    arrange(comid) %>%
    # Ensure comid is character type
    mutate(comid = as.character(comid))
  
  streamcat_subset_tn_tmp
}

streamcat_subset_tp <- foreach(i = 1:length(target_selection)) %do% {
  
  watershed_comid <- reaches_TP_target %>%
    filter(watershed_name == target_selection[i])
  
  if (length(watershed_comid > 0)) {
    streamcat_subset_tp_tmp <- as.data.frame(runoffcoeff)[
      runoffcoeff$comid %in% watershed_comid$catchment_comid, 
    ] %>%
      select(comid) %>%
      arrange(comid) %>%
      # Ensure comid is character type
      mutate(comid = as.character(comid))
  } else {
    streamcat_subset_tp_tmp <- data.frame(comid = NA_character_)
  }
  
  streamcat_subset_tp_tmp
  
}

Message.tn <- foreach(i = 1:length(target_selection)) %do% {
  
  watershed_comid <- reaches_TN_target %>%
    filter(watershed_name == target_selection[i])
  
  # Compare StreamCat data with SPARROW data (not just target list)
  # This accounts for COMIDs that may be in StreamCat but missing from SPARROW
  streamcat_comids <- unique(streamcat_subset_tn[[i]]$comid)
  sparrow_comids_in_watershed <- unique(watershed_comid$catchment_comid[
    watershed_comid$catchment_comid %in% sparrow_cons_out_tn$comid
  ])
  
  if(
    length(streamcat_comids) > length(sparrow_comids_in_watershed)
  ) {
    Message.tmp <- paste(
      paste0(
        "Note: For the ", 
        i, 
        if(
          i %in% c(4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 24, 25, 26, 27, 28, 29, 30, 34, 35, 36, 37, 38, 39, 40)
        ) {"th"} else if(
          i %% 10 == 1
        ) {"st"} else if(
          i %% 10 == 2
        ) {"nd"} else if(
          i %% 10 == 3
        ) {"rd"} else {"th"}, 
        " TN target, there are more reaches in the provided StreamCat"
      ),
      "datasets than are included in SPARROW.", 
      "Only the reaches that are included in both datasets will be available",
      "for BMP optimization. Loads from the remaining reaches will be included in", 
      "the 'other_loads' parameter.", 
      sep = "\n"
    )
  } else if(
    length(streamcat_comids) < length(sparrow_comids_in_watershed)
  ) {
    Message.tmp <- paste(
      paste0(
        "Note: For the ", 
        i, 
        if(
          i %in% c(4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 24, 25, 26, 27, 28, 29, 30, 34, 35, 36, 37, 38, 39, 40)
        ) {"th"} else if(
          i %% 10 == 1
        ) {"st"} else if(
          i %% 10 == 2
        ) {"nd"} else if(
          i %% 10 == 3
        ) {"rd"} else {"th"}, 
        " TN target, there are fewer reaches in the provided"
      ),
      "StreamCat datasets than are included in SPARROW.", 
      "Only the reaches that are included in both datasets will be available",
      "for BMP optimization. Loads from the remaining reaches will be included in",
      "the 'other_loads' parameter.", 
      sep = "\n"
    )
  } else {
    Message.tmp <- paste(
      paste0(
        "Note: For the ", 
        i, 
        if(i == 1) {"st"} else if(i == 2) {"nd"} else {"rd"}, 
        " TN target, there are the same number of reaches in SPARROW and Streamcat"
      ),
      "subsetted datasets.", 
      "All reaches available for BMP optimization", 
      sep = "\n"
    )
  }
  
  Message.tmp
}

if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
  invisible(lapply(Message.tn, cat, sep = "\n"))
}

Message.tp <- foreach(i = 1:length(target_selection)) %do% {
  
  watershed_comid <- reaches_TP_target %>%
    filter(watershed_name == target_selection[i])
  
  # Compare StreamCat data with SPARROW data (not just target list)
  # This accounts for COMIDs that may be in StreamCat but missing from SPARROW
  streamcat_comids <- unique(streamcat_subset_tp[[i]]$comid)
  sparrow_comids_in_watershed <- unique(watershed_comid$catchment_comid[
    watershed_comid$catchment_comid %in% sparrow_cons_out_tp$comid
  ])
  
  if(
    length(streamcat_comids) > length(sparrow_comids_in_watershed)
  ) {
    Message.tmp <- paste(
      paste0(
        "Note: For the ", 
        i, 
        if(
          i %in% c(4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 24, 25, 26, 27, 28, 29, 30, 34, 35, 36, 37, 38, 39, 40)
        ) {"th"} else if(
          i %% 10 == 1
        ) {"st"} else if(
          i %% 10 == 2
        ) {"nd"} else if(
          i %% 10 == 3
        ) {"rd"} else {"th"}, 
        " TP target, there are more reaches in the provided StreamCat"
      ),
      "datasets than are included in SPARROW.", 
      "Only the reaches that are included in both datasets will be available",
      "for BMP optimization.Loads from the remaining reaches will be included in",
      "the 'other_loads' parameter.", 
      sep = "\n"
    )
  } else if(
    length(streamcat_comids) < length(sparrow_comids_in_watershed)
  ) {
    Message.tmp <- paste(
      paste0(
        "Note: For the ", 
        i, 
        if(
          i %in% c(4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 24, 25, 26, 27, 28, 29, 30, 34, 35, 36, 37, 38, 39, 40)
        ) {"th"} else if(
          i %% 10 == 1
        ) {"st"} else if(
          i %% 10 == 2
        ) {"nd"} else if(
          i %% 10 == 3
        ) {"rd"} else {"th"}, 
        " TP target, there are fewer reaches in the provided"
      ),
      "StreamCat datasets than are included in SPARROW.", 
      "Only the reaches that are included in both datasets will be available",
      "for BMP optimization. Loads from the remaining reaches will be included in",
      "the 'other_loads' parameter.", 
      sep = "\n"
    )
  } else {
    Message.tmp <- paste(
      paste0(
        "Note: For the ", 
        i, 
        if(
          i %in% c(4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 24, 25, 26, 27, 28, 29, 30, 34, 35, 36, 37, 38, 39, 40)
        ) {"th"} else if(
          i %% 10 == 1
        ) {"st"} else if(
          i %% 10 == 2
        ) {"nd"} else if(
          i %% 10 == 3
        ) {"rd"} else {"th"}, 
        " TP target, there are the same number of reaches in SPARROW and Streamcat"
      ),
      "subsetted datasets.", 
      "All reaches available for BMP optimization", 
      sep = "\n"
    )
  }
  
  Message.tmp
}

if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
  invisible(lapply(Message.tp, cat, sep = "\n"))
}

#
#
## Determine Riparian Loadings ----- 
riparian.loadings_tn <- foreach(i = 1:length(target_selection)) %do% {
  
  watershed_comid <- reaches_TN_target %>%
    filter(watershed_name == target_selection[i])
  
  riparian.loadings %>% 
    filter(comid %in% watershed_comid$catchment_comid) %>%
    select(comid, season, year, N_riparian_kgyr) %>%
    mutate(year = as.character(str_pad(year, width = 2, side = "left", pad = "0"))) %>%
    right_join(
      ., 
      sparrow_cons_out_tn %>%
        filter(comid %in% watershed_comid$catchment_comid) %>%
        select(comid, season, year, in_total),
      by = c("comid", "season", "year")
    ) %>%
    mutate(
      N_riparian_kgyr = case_when(
        N_riparian_kgyr > in_total ~ in_total,
        is.na(N_riparian_kgyr) ~ 0,
        TRUE ~ N_riparian_kgyr
      ),
      season = as.character(season)
    ) %>%
    select(-in_total)
}

riparian.loadings_tp <- foreach(i = 1:length(target_selection)) %do% {
  
  watershed_comid <- reaches_TP_target %>%
    filter(watershed_name == target_selection[i])
  
  riparian.loadings %>% 
    filter(comid %in% watershed_comid$catchment_comid) %>%
    select(comid, season, year, P_riparian_kgyr) %>%
    mutate(year = as.character(str_pad(year, width = 2, side = "left", pad = "0")),
           season = as.character(season)) %>%
    right_join(
      ., 
      sparrow_cons_out_tp %>%
        filter(comid %in% watershed_comid$catchment_comid) %>%
        select(comid, season, year, ip),
      by = c("comid", "season", "year")
    ) %>%
    mutate(
      P_riparian_kgyr = case_when(
        P_riparian_kgyr > ip ~ ip,
        is.na(P_riparian_kgyr) ~ 0,
        TRUE ~ P_riparian_kgyr
      )
    )  %>%
    select(-ip)
}

#
#
## Adjust delivery fraction to pore point -----
max_delfrac_tn <- unlist(
  foreach(i = 1:length(target_selection)) %do% {
    
    watershed_comid <- reaches_TN_target %>%
      filter(watershed_name == target_selection[i] & TerminalFlag == "X")
    
    sparrow_cons_out_tn %>%
      filter(comid %in% watershed_comid$catchment_comid) %>%
      summarize(max_del_frac = max(DEL_FRAC, na.rm = TRUE), .groups = "drop") %>%
      pull(max_del_frac)
    
  }
)

temp_delfrac_rev_tn <- foreach(i = 1:length(target_selection)) %do% {
  
  watershed_comid <- reaches_TN_target %>%
    filter(watershed_name == target_selection[i])
  
  sparrow_cons_out_tn %>% 
    filter(comid %in% watershed_comid$catchment_comid)
  
}

invisible(
  foreach(i = 1:length(temp_delfrac_rev_tn)) %do% {
    
    temp_delfrac_rev_tn[[i]]$delfrac_rev <- with(
      temp_delfrac_rev_tn[[i]], ifelse(DEL_FRAC == 0, 0,
                                       ifelse(max_delfrac_tn[i] == 0, 0, DEL_FRAC / max_delfrac_tn[i])) 
    )
    
  }
)

delfrac_rev_tn <- foreach(i = 1:length(temp_delfrac_rev_tn)) %do% {
  
  temp_delfrac_rev_tn[[i]] %>% 
    select(c("comid", "season", "year", "DEL_FRAC")) %>%
    mutate(season = as.character(season))
  
}

max_delfrac_tp <- unlist(
  foreach(i = 1:length(target_selection)) %do% {
    
    watershed_comid <- reaches_TP_target %>%
      filter(watershed_name == target_selection[i] & TerminalFlag == "X")
    
    if (nrow(watershed_comid) > 0) {
      
      sparrow_cons_out_tp %>%
        filter(comid %in% watershed_comid$catchment_comid) %>%
        summarize(max_del_frac = max(DEL_FRAC, na.rm = TRUE), .groups = "drop") %>%
        pull(max_del_frac)
      
    } else { 1 } 
    
  }
)

temp_delfrac_rev_tp <- foreach(i = 1:length(target_selection)) %do% {
  
  watershed_comid <- reaches_TP_target %>%
    filter(watershed_name == target_selection[i])
  
  sparrow_cons_out_tp %>% filter(comid %in% watershed_comid$catchment_comid)
  
}

invisible(
  foreach(i = 1:length(temp_delfrac_rev_tp)) %do% {
    
    temp_delfrac_rev_tp[[i]]$delfrac_rev <- with(
      temp_delfrac_rev_tp[[i]], DEL_FRAC / max_delfrac_tp[i]
    )
    
  }
)

delfrac_rev_tp <- foreach(i = 1:length(temp_delfrac_rev_tp)) %do% {
  
  temp_delfrac_rev_tp[[i]] %>% select(c("comid", "year", "season", "DEL_FRAC"))
  
}

## Define costs ------

temp_bmp_costs <- user_specs_BMPs %>%
  filter(BMP_Selection == "X") %>%
  select(
    BMP_Category, BMP, contains(c("capital", "operations")), UserSpec_RD_in
  )

### Conversion from area-specific costs to cost per acre for ag BMPs and to costs per linear foot for Riparian BMPs
temp_bmp_costs_rev <- temp_bmp_costs %>%
  mutate(
    across(
      .cols = c(contains(c("capital_", "operations_")) & !contains("units")), 
      .fns = ~replace_na(., NA_real_)
    ),
    across(
      .cols = c(contains(c("capital_", "operations_")) & !contains("units")), 
      .fns = ~case_when(
        BMP_Category == "ag" ~ case_when(
          capital_units == "ft2" ~ . * ft2_to_ac,
          capital_units == "km2" ~ . * km2_to_ac,
          capital_units == "yd2" ~ . * yd2_to_ac,
          capital_units == "ac" ~ .
        ),
        BMP_Category == "urban" ~ .,
        BMP_Category == "point" ~ .,
        BMP_Category == "ripbuf" ~ case_when(
          capital_units == "ft2" ~ . * UserSpec_RD_in,
          capital_units == "km2" ~ . * km2_to_ac / ft2_to_ac * UserSpec_RD_in,
          capital_units == "yd2" ~ . * yd2_to_ac / ft2_to_ac * UserSpec_RD_in,
          capital_units == "ac" ~ . / ft2_to_ac * UserSpec_RD_in
        )
      ), 
      .names = "{.col}_rev"
    )
  )

temp1 <- temp_bmp_costs_rev %>% 
  select(
    c(
      "BMP_Category",
      "BMP",
      contains(c("capital_", "operations_")) & !contains("units") & 
        contains("rev")
    )
  ) %>%
  rename(
    category = BMP_Category,
    bmp = BMP
  ) %>%
  rename_at(
    vars(contains(c("capital_", "operations_"))), list( ~ gsub("_rev", "", .))
  ) %>%
  # Annualize capital costs based on planning horizon and interest rate
  mutate(
    across(
      .cols = contains("capital_"), 
      .fns = ~(
        . * (
          interest_rate * ((1 + interest_rate) ^ horizon) / (
            ((1 + interest_rate) ^ horizon) - 1
          )
        )
      )
    )
  )

temp2 <- temp1[temp1$category == 'point',]
temp2[is.na(temp2)] <- 0
temp1[temp1$category == 'point',] <- temp2
bmp_costs <- copy(temp1)
rm(temp1, temp2)


### Specify point source costs as either capital or operations, depending on WWTP state
# Added Upper CT River WWTPs with low cost retrofit upgrades possible
if("point" %in% bmp_costs$category) {
  point_comid <- fread(paste(InPath, "WWTP_COMIDs.csv", sep="")) %>% 
  # point_comid <- fread(paste(InPath, "CTC_WWTP_COMIDs.csv", sep="")) %>% 
    # Remove all special charachters and spaces from WWTP names
    mutate(Plant_Name = gsub("[^[:alnum:]]", "_", Plant_Name)) #*#
  # Note to user: Can update this file if more WWTPs desired for analysis; must also update UserSpecs_BMPs.csv with costs
  names(point_comid) <- c("State", "bmp", "NPDES_ID", "comid")
  
  temp_bmp_costs_point <- merge(
    bmp_costs[bmp_costs$category =="point",], 
    point_comid,
    by = "bmp",
    all.x = TRUE
  ) %>%
    mutate(
      capital = my_key_fun(., "State", ~paste0("capital_", .x)),
      operations = my_key_fun(., "State", ~paste0("operations_", .x))
    ) 
  
  if(any(is.na(temp_bmp_costs_point))) {
    stop(
      paste0(
        "Point costs for ", 
        paste(
          temp_bmp_costs_point %>% 
            filter(is.na(capital) | is.na(operations)) %>% 
            select(bmp) %>% 
            pull(), 
          collapse = ", "
        ),
        " not provided. Please ensure costs are available in `01_UserSpecs_BMPs.csv`"
      )
    )
  }
  
  bmp_costs_point <- temp_bmp_costs_point %>% 
    select(c("category", "comid", "capital", "operations", "State"))
  
} else {
  bmp_costs_point <- data.frame(
    category = "point", comid = NA, capital = NA, operations = NA, State = NA
  )
}

### Define the fraction of agricultural costs that reflect base payment versus actual agricultural BMP costs
agcost_frac <- 100/75 #*#

### Remove temporary datasets
rm(list = ls(pattern = "^temp"))


## Define urban cost correction coefficients ------
### These are based on the type of urban land. The costs given in UserSpecs are for new development. 
### Retrofitting is multiplied by a factor of 2 and difficult retrofit by a factor of 3.
### Open and low density development we assume will be new development, med density will be retrofits, and high density will be difficult retrofits.

urban_cost_coeffs <- StreamCat_api %>%
  mutate(
    total_urban = PctUrbOp2019Cat + 
      PctUrbLo2019Cat + 
      PctUrbMd2019Cat + 
      PctUrbHi2019Cat,
    urban_cost_coef = case_when(
      total_urban == 0 ~ 1,
      total_urban > 0 ~ 
        ((PctUrbOp2019Cat + PctUrbLo2019Cat) / total_urban) * 1 +
        (PctUrbMd2019Cat / total_urban) * 2 +
        (PctUrbHi2019Cat / total_urban) * 3
    )
  ) %>%
  select(comid, urban_cost_coef)

#
#
#
# PART 3: DEFINE EFFICIENCIES -----

## Agricultural BMPs -----

### Read in efficiency data for Fert_20 and Manure_Injection BMPs
temp_ag_effic_fert_man <- fread(
  paste(InPath, "AgBMPEffic_FertManure.csv", sep = "")
) #*#
temp_ag_effic_fert_man_cast_tn <- reshape2::dcast(
  temp_ag_effic_fert_man, Category ~ BMP, value.var = "N_Efficiency"
) #*#
temp_ag_effic_fert_man_cast_tp <- reshape2::dcast(
  temp_ag_effic_fert_man, Category ~ BMP, value.var = "P_Efficiency"
) #*#

### Read in efficiency data for ACRE database BMPs
temp_acre <- if(AgBMPcomparison == "No Practice") {
# fread(paste0(InPath, "ACRE_HUC12_HRU_Summary_compareNoPractice_ICF25.csv")) #*#
# Use updated data that includes Pawcatuck and S Shore and cover crop efficiency
 fread(paste0(InPath, "ACRE_HUC12_HRU_Summary_compareNoPractice_ICF25ND.csv"))  
} else if(AgBMPcomparison == "Baseline") {
# fread(paste0(InPath, "ACRE_HUC12_HRU_Summary_compareBaseline_ICF25.csv")) #*#
  # Use updated data that includes Pawcatuck and S Shore and cover crop efficiency 
fread(paste0(InPath, "ACRE_HUC12_HRU_Summary_compareBaseline_ICF25ND.csv"))  
} else {
  stop(
    'AgBMPcomparison must be set to either "No Practice" or "Baseline", quotation marks included.'
  )
}#*#

temp_acre$bmp <- with(
  temp_acre,
  ifelse(
    Scenario=="CONSERVATION",
    "Conservation",
    ifelse(
      Scenario=="Contour Farming",
      "Contour_Farming",
      ifelse(
        Scenario=="Terraces and Waterway",
        "Terrace_Waterway",
        ifelse(
          Scenario=="Terraces Only",
          "Terrace_Only",
          ifelse(
            Scenario=="Waterway Only", "Waterway_Only", as.character(Scenario)
          )
        )
      )
    )
  )
) #*#

temp_acre$Scenario <- NULL
temp_acre$HUC12_Rev <- str_pad(temp_acre$HUC12, width=12, pad="0")
temp_acre$HUC10_Rev <- str_pad(temp_acre$HUC10, width=10, pad="0")
temp_acre$HUC8_Rev <- str_pad(temp_acre$HUC8, width=8, pad="0")

### Combine ACRE bmps with Manure_Injection and Fert_20 BMP-specific efficiencies
temp_Ag_BMPs <- user_specs_BMPs[
  user_specs_BMPs$BMP_Selection == "X" & (user_specs_BMPs$BMP_Category == "ag"),
]
Ag_BMPs <- paste0(temp_Ag_BMPs$BMP)

temp_acre_cast_tn <- reshape2::dcast(
  temp_acre, HUC8_Rev+HUC10_Rev+HUC12_Rev ~ bmp, value.var = "MeanTN_Effic"
) %>% 
  rename(HUC8 = HUC8_Rev, HUC10 = HUC10_Rev, HUC12 = HUC12_Rev)

temp_acre_cast_tn_HUC8 <- temp_acre_cast_tn %>%
  filter(is.na(HUC12) & is.na(HUC10))

temp_acre_cast_tn_HUC10 <- temp_acre_cast_tn %>%
  filter(is.na(HUC12) & is.na(HUC8))

temp_acre_cast_tn_HUC12 <- temp_acre_cast_tn %>%
  filter(!is.na(HUC12), !is.na(HUC10), !is.na(HUC8))

ACRE_BMPs <- names(temp_acre_cast_tn[, -c(1:3)])

#PROBLEM - HUC12 MISSING FOR SELECTED COMIDS
reaches_huc12_tn <- ne_comid_huc12 %>% 
  filter(comid %in% reaches_TN_target$catchment_comid) %>% 
  select(comid, HUC12 = HUC_12_Rev)  %>%
  mutate(HUC10 = left(HUC12, 10), HUC8 = left(HUC12, 8))

temp_acre_reaches_HUC12_tn <- merge(
  reaches_huc12_tn[, c("comid", "HUC12")],
  temp_acre_cast_tn_HUC12[, c("HUC12", ACRE_BMPs)],
  by = "HUC12",
  all.x = TRUE
) %>%
  rename_at(vars(one_of(ACRE_BMPs)), list( ~ paste0(., "_HUC12")))

temp_acre_reaches_HUC10_tn <- merge(
  reaches_huc12_tn[, c("comid", "HUC10")],
  temp_acre_cast_tn_HUC10[, c("HUC10", ACRE_BMPs)],
  by = "HUC10",
  all.x = TRUE
) %>%
  rename_at(vars(one_of(ACRE_BMPs)), list( ~ paste0(., "_HUC10")))

temp_acre_reaches_HUC8_tn <- merge(
  reaches_huc12_tn[, c("comid", "HUC8")],
  temp_acre_cast_tn_HUC8[, c("HUC8", ACRE_BMPs)],
  by = "HUC8",
  all.x = TRUE
) %>%
  rename_at(vars(one_of(ACRE_BMPs)), list( ~ paste0(., "_HUC8")))

if (nrow(temp_acre_reaches_HUC12_tn) > 0) {
  temp_acre_reaches_tn <- merge(
    merge(
      merge(
        reaches_huc12_tn, 
        temp_acre_reaches_HUC12_tn, 
        by = c("comid", "HUC12"),
        all.x = TRUE
      ),
      temp_acre_reaches_HUC10_tn,
      by = c("comid", "HUC10"),
      all.x = TRUE
    ),
    temp_acre_reaches_HUC8_tn,
    by = c("comid", "HUC8"),
    all.x = TRUE
  )
  temp_acre_reaches_tn[ , ACRE_BMPs] <- NA
  
  acre_reaches_tn <- temp_acre_reaches_tn %>%
    mutate(
      across(
        all_of(ACRE_BMPs), 
        ~ case_when(
          !is.na(temp_acre_reaches_tn[[paste0(cur_column(), "_HUC12")]]) ~ 
            temp_acre_reaches_tn[[paste0(cur_column(), "_HUC12")]],
          !is.na(temp_acre_reaches_tn[[paste0(cur_column(), "_HUC10")]]) ~ 
            temp_acre_reaches_tn[[paste0(cur_column(), "_HUC10")]],
          !is.na(temp_acre_reaches_tn[[paste0(cur_column(), "_HUC8")]]) ~ 
            temp_acre_reaches_tn[[paste0(cur_column(), "_HUC8")]]
        )
      )
    ) %>%
    select(comid, all_of(ACRE_BMPs))
  
  ag_effic_bycomid_tn <- add_column(
    acre_reaches_tn, temp_ag_effic_fert_man_cast_tn %>% select(-Category)
    ) %>% 
    select(comid, all_of(Ag_BMPs)) %>%
    mutate(across(all_of(Ag_BMPs), ~ replace_na(., -999)))# By setting unknowns to -999, the model will not implement BMPs that have missing data.
}

temp_acre_cast_tp <- reshape2::dcast(
  temp_acre, HUC8_Rev+HUC10_Rev+HUC12_Rev ~ bmp, value.var = "MeanTP_Effic"
) %>% 
  rename(HUC8 = HUC8_Rev, HUC10 = HUC10_Rev, HUC12 = HUC12_Rev)

temp_acre_cast_tp_HUC8 <- temp_acre_cast_tp %>%
  filter(is.na(HUC12) & is.na(HUC10))

temp_acre_cast_tp_HUC10 <- temp_acre_cast_tp %>%
  filter(is.na(HUC12) & is.na(HUC8))

temp_acre_cast_tp_HUC12 <- temp_acre_cast_tp %>%
  filter(!is.na(HUC12), !is.na(HUC10), !is.na(HUC8))

reaches_huc12_tp <- ne_comid_huc12 %>% 
  filter(comid %in% reaches_TP_target$catchment_comid) %>% 
  select(comid, HUC12 = HUC_12_Rev)  %>%
  mutate(HUC10 = left(HUC12, 10), HUC8 = left(HUC12, 8))

temp_acre_reaches_HUC12_tp <- merge(
  reaches_huc12_tp[, c("comid", "HUC12")],
  temp_acre_cast_tp_HUC12[, c("HUC12", ACRE_BMPs)],
  by = "HUC12",
  all.x = TRUE
) %>%
  rename_at(vars(one_of(ACRE_BMPs)), list( ~ paste0(., "_HUC12")))

temp_acre_reaches_HUC10_tp <- merge(
  reaches_huc12_tp[, c("comid", "HUC10")],
  temp_acre_cast_tp_HUC10[, c("HUC10", ACRE_BMPs)],
  by = "HUC10",
  all.x = TRUE
) %>%
  rename_at(vars(one_of(ACRE_BMPs)), list( ~ paste0(., "_HUC10")))

temp_acre_reaches_HUC8_tp <- merge(
  reaches_huc12_tp[, c("comid", "HUC8")],
  temp_acre_cast_tp_HUC8[, c("HUC8", ACRE_BMPs)],
  by = "HUC8",
  all.x = TRUE
) %>%
  rename_at(vars(one_of(ACRE_BMPs)), list( ~ paste0(., "_HUC8")))

if (nrow(temp_acre_reaches_HUC12_tp) > 0) {
  temp_acre_reaches_tp <- merge(
    merge(
      merge(
        reaches_huc12_tp, 
        temp_acre_reaches_HUC12_tp, 
        by = c("comid", "HUC12"),
        all.x = TRUE
      ),
      temp_acre_reaches_HUC10_tp,
      by = c("comid", "HUC10"),
      all.x = TRUE
    ),
    temp_acre_reaches_HUC8_tp,
    by = c("comid", "HUC8"),
    all.x = TRUE
  )
  temp_acre_reaches_tp[ , ACRE_BMPs] <- NA
  
  acre_reaches_tp <- temp_acre_reaches_tp %>%
    mutate(
      across(
        all_of(ACRE_BMPs), 
        ~ case_when(
          !is.na(temp_acre_reaches_tp[[paste0(cur_column(), "_HUC12")]]) ~ 
            temp_acre_reaches_tp[[paste0(cur_column(), "_HUC12")]],
          !is.na(temp_acre_reaches_tp[[paste0(cur_column(), "_HUC10")]]) ~ 
            temp_acre_reaches_tp[[paste0(cur_column(), "_HUC10")]],
          !is.na(temp_acre_reaches_tp[[paste0(cur_column(), "_HUC8")]]) ~ 
            temp_acre_reaches_tp[[paste0(cur_column(), "_HUC8")]]
        )
      )
    ) %>%
    select(comid, all_of(ACRE_BMPs))
  
  ag_effic_bycomid_tp <- add_column(
    acre_reaches_tp, temp_ag_effic_fert_man_cast_tp %>% select(-Category)
  ) %>% 
    select(comid, all_of(Ag_BMPs)) %>%
    mutate(across(all_of(Ag_BMPs), ~ replace_na(., -999)))# By setting unknowns to -999, the model will not implement BMPs that have missing data.
}

#
#
## Point Source BMPs ------
#*# Updated removal efficiencies to include low cost retrograde refits in Upper CT
if("point" %in% bmp_costs$category) {
  temp_point_effic <- fread(
    paste(InPath, "WWTP_RemovalEffic_upd.csv", sep = ""), 
    # paste(InPath, "CTC_WWTP_RemovalEffic_upd.csv", sep = ""), 
    col.names = c("BMP_Category", "bmp", "N_Efficiency", "P_Efficiency")
  )
  
  point_effic <- merge(
    temp_point_effic[
      temp_point_effic$bmp %in% 
        user_specs_BMPs$BMP[user_specs_BMPs$BMP_Selection=="X"],
    ],
    point_comid,
    by = c("bmp"),
    all.y = TRUE
  ) %>%
    mutate(across(contains('Efficiency'), ~ as.numeric(.))) %>%
    mutate(across(contains("Efficiency"), ~ replace_na(., -999))) # By setting unknowns to -999, the model will not implement BMPs that have missing data
  
  point_effic_bycomid_tn <- point_effic %>% 
    select(category = BMP_Category, comid, effic = N_Efficiency)
  
  point_effic_bycomid_tp <- point_effic %>% 
    select(category = BMP_Category, comid, effic = P_Efficiency)
} else {
  point_effic_bycomid_tn <- data.frame(
    BMP_Category = "point", comid = NA, effic = NA
  )
  point_effic_bycomid_tp <- data.frame(
    BMP_Category = "point", comid = NA, effic = NA
  )
}

#
#
## Urban BMPs ------ 
temp_Urban_BMPs <- user_specs_BMPs[
  user_specs_BMPs$BMP_Selection == "X" & 
    (user_specs_BMPs$BMP_Category == "urban"),
]
Urban_BMPs <- paste0(temp_Urban_BMPs$BMP)

urban.effic.curves <- fread(
  paste0(InPath, "/UrbanBMPPerformanceCurves.csv"), data.table = FALSE
)

comid.infiltrationrates <- fread(
  paste0(InPath, "/NHD+infiltrationrates.csv")
) %>%
  mutate(comid = as.character(comid))

comid.infiltrationrates.matched <-comid.infiltrationrates %>%
  rename(InfiltrationRate_inperhr = infiltrationrate_inperhr) %>%
  mutate(
    InfiltrationRate_inperhr = custom.round(
      x = InfiltrationRate_inperhr, 
      breaks = c(0, 0.17, 0.27, 0.52, 1.02, 2.41, 8.27)  # NH BMPs calculated at 0.17 in/hr, 0.27 in/hr, 0.52 in/hr, 1.02 in/hr, 2.41 in/hr and 8.27 in/hr
    )
  )

temp_urban_effic <-  user_specs_BMPs[
  user_specs_BMPs$BMP_Category == "urban" & 
    user_specs_BMPs$BMP_Selection == "X",
  c("BMP_Category", "BMP", "Min_RD_in", "Max_RD_in", "UserSpec_RD_in")
] 

urban.BMPs.list <- temp_urban_effic

if(length(Urban_BMPs) > 0) {
  urban_effic.n <- merge(
    temp_urban_effic, 
    urban.effic.curves %>% filter(Pollutant == "N"), 
    by = "BMP"
  ) %>%
    mutate(
      expression = gsub(
        ' x', ' UserSpec_RD_in ', gsub('y ~ ', '', Best.Fit.Curve)
      )
    ) %>%
    rowwise() %>%
    mutate(iter = 1, effic = eval(parse(text = expression))) %>%
    filter(
      BMP %in% user_specs_BMPs$BMP[user_specs_BMPs$BMP_Selection == "X"]
    ) %>%
    select(
      category = BMP_Category, bmp = BMP, effic, InfiltrationRate_inperhr
    ) %>%
    group_by(bmp) %>%
    pivot_wider(
      id_cols = InfiltrationRate_inperhr, names_from = bmp, values_from = effic
    ) %>%
    add_row(InfiltrationRate_inperhr = 0) %>%
    mutate(
      across(
        any_of(
          c("Infiltration_Basin", 
            "Infiltration_Chamber",
            "Infiltration_Trench", 
            "Porous_Pavement_w_subsurface_infiltration")
        ), 
        ~ case_when(InfiltrationRate_inperhr == 0 ~ -999, TRUE ~ .) # Infiltration BMPs should not be used in subcatchments with very low infiltration rates (predominately HSG D). Not only is this unreasonable theoretically, we also do not have an efficiency curve to match these low infiltration rates. The -999 will force the model not to implement these BMPs in areas that have low infiltration rates. There is an additional check, that the max implementation of these BMPs in these comids will be set to 0. 
      )
    ) %>%
    fill(all_of(Urban_BMPs))
  
  
  urban_effic_bycomid_tn <- merge(
    merge(
      comid.infiltrationrates.matched  %>% select(-V1), 
      data.frame(comid = as.character(unlist(streamcat_subset_tn, use.names = FALSE))) %>% na.omit(), 
      by = 'comid',
      all = TRUE
    ), 
    urban_effic.n, 
    by = "InfiltrationRate_inperhr",
    all = TRUE
  ) %>%
    select(-InfiltrationRate_inperhr)
  
  urban_effic.p <- merge(
    temp_urban_effic, 
    urban.effic.curves %>% filter(Pollutant == "P"),
    by = "BMP"
  ) %>%
    mutate(
      expression = gsub(
        ' x', ' UserSpec_RD_in ', gsub('y ~ ', '', Best.Fit.Curve)
      )
    ) %>%
    rowwise() %>%
    mutate(iter = 1, effic = eval(parse(text = expression))) %>%
    filter(
      BMP %in% user_specs_BMPs$BMP[user_specs_BMPs$BMP_Selection == "X"]
    ) %>%
    select(
      category = BMP_Category, bmp = BMP, effic, InfiltrationRate_inperhr
    ) %>%
    pivot_wider(
      id_cols = InfiltrationRate_inperhr, names_from = bmp, values_from = effic
    ) %>%
    add_row(InfiltrationRate_inperhr = 0) %>%
    mutate(
      across(
        any_of(
          c("Infiltration_Basin", 
            "Infiltration_Chamber",
            "Infiltration_Trench", 
            "Porous_Pavement_w_subsurface_infiltration")
        ), 
        ~ case_when(InfiltrationRate_inperhr == 0 ~ -999, TRUE ~ .) # Infiltration BMPs should not be used in subcatchments with very low infiltration rates (predominately HSG D). Not only is this unreasonable theoretically, we also do not have an efficiency curve to match these low infiltration rates. The -999 will force the model not to implement these BMPs in areas that have low infiltration rates. There is an additional check, that the max implementation of these BMPs in these comids will be set to 0. 
      )
    ) %>%
    fill(all_of(Urban_BMPs))
  
  urban_effic_bycomid_tp <- merge(
    merge(
      comid.infiltrationrates.matched  %>% select(-V1), 
      data.frame(comid = as.character(unlist(streamcat_subset_tp, use.names = FALSE))) %>% na.omit(), 
      by = 'comid',
      all = TRUE
    ), 
    urban_effic.p, 
    by = "InfiltrationRate_inperhr",
    all = TRUE
  ) %>%
    select(-InfiltrationRate_inperhr)
}

### Remove temporary datasets
rm(list = ls(pattern = "^temp"))

#
#
## Riparian Buffer Removals ------

if(length(RiparianBuffer_BMPs) > 0) {
  riparian_buffer_efficiencies_N <- foreach(
    i = 1: length(streamcat_subset_tn)
  ) %do% {
    riparian_buffer_efficiencies_N_tmp <- foreach(
      j = 1:length(RiparianBuffer_BMPs), .combine = "merge"
    ) %do% {
      
      short.form.bmp.name <- if(RiparianBuffer_BMPs[j] == "Grassed_Buffer") {
        "Grass"
      } else if(RiparianBuffer_BMPs[j] == "Forested_Buffer") {"Forest"}
      
      riparian.efficiencies.tmp <- riparian.efficiencies %>% 
        select(
          -contains("P"), 
          -V1, 
          -!contains(short.form.bmp.name), 
          -!contains(as.character(UserSpecs_bufferwidth_nearest[j])), 
          -contains("uncertainty"),
          comid
        ) %>%
        rename_with(.fn = ~"Curve.Form", .cols = contains("N")) %>%
        mutate(
          x = RiparianBuffer_Widths[j], 
          expression = gsub("y ~ ", '', Curve.Form)
        ) %>% rowwise() %>%
        mutate(iter = 1, effic = eval(parse(text = expression))) %>%
        select(comid, effic) %>%
        rename_with(.fn = ~paste(RiparianBuffer_BMPs[j]), .cols = "effic")
    }
    
    riparian_buffer_efficiencies_N_tmp %>%
      filter(comid %in% streamcat_subset_tn[[i]]$comid) %>%
      mutate(
        across(
          .cols = any_of(RiparianBuffer_BMPs), 
          .fn = ~case_when(is.na(.) ~ -999, !is.na(.) ~ .))
        
      )
  }
  
  riparian_buffer_efficiencies_P <- foreach(
    i = 1: length(streamcat_subset_tp)
  ) %do% {
    riparian_buffer_efficiencies_P_tmp <- foreach(
      j = 1:length(RiparianBuffer_BMPs), .combine = "merge"
    ) %do% {
      
      short.form.bmp.name <- if(RiparianBuffer_BMPs[j] == "Grassed_Buffer") {
        "Grass"
      } else if(RiparianBuffer_BMPs[j] == "Forested_Buffer") {"Forest"}
      
      riparian.efficiencies.tmp <- riparian.efficiencies %>% 
        select(
          -contains("N"), 
          -V1, 
          -!contains(short.form.bmp.name), 
          -!contains(as.character(UserSpecs_bufferwidth_nearest[j])), 
          -contains("uncertainty"),
          comid
        ) %>%
        rename_with(.fn = ~"Curve.Form", .cols = contains("P")) %>%
        mutate(
          x = RiparianBuffer_Widths[j], 
          expression = gsub("y ~ ", '', Curve.Form)
        ) %>% rowwise() %>%
        mutate(iter = 1, effic = eval(parse(text = expression))) %>%
        select(comid, effic) %>%
        rename_with(.fn = ~paste(RiparianBuffer_BMPs[j]), .cols = "effic")
    }
    
    riparian_buffer_efficiencies_P_tmp %>%
      filter(comid %in% streamcat_subset_tp[[i]]$comid) %>%
      mutate(
        across(
          .cols = any_of(RiparianBuffer_BMPs), 
          .fn = ~case_when(is.na(.) ~ -999, !is.na(.) ~ .))
      )
  }
  
  riparian_buffer_removal_N <- foreach(i = 1:length(streamcat_subset_tn)) %do% {
    left_join(
      riparian.loadings_tn[[i]], riparian.existingbuffer, by = "comid"
    ) %>%
      select(comid, season, year, N_riparian_kgyr, totalbanklength_ft) %>%
      mutate(
        loading_per_bankft_kg_ftyr = N_riparian_kgyr / totalbanklength_ft
      ) %>%
      left_join(., riparian_buffer_efficiencies_N[[i]], by = "comid") %>%
      mutate(
        across(
          .cols = any_of(RiparianBuffer_BMPs), 
          .fn = ~case_when(
            . == -999 ~ -999, . > 0 ~ (. * loading_per_bankft_kg_ftyr)
          )
        )
      ) %>%
      select(comid, season, year, any_of(RiparianBuffer_BMPs)) %>%
      mutate(season = rep(c(1, 2, 3, 4), times = nrow(riparian.loadings_tn[[i]])/4))
    
  }
  
  riparian_buffer_removal_P <- foreach(i = 1:length(streamcat_subset_tp)) %do% {
    left_join(
      riparian.loadings_tp[[i]], riparian.existingbuffer, by = "comid"
    ) %>%
      select(comid, season, year, P_riparian_kgyr, totalbanklength_ft) %>%
      mutate(
        loading_per_bankft_kg_ftyr = P_riparian_kgyr / totalbanklength_ft
      ) %>%
      left_join(., riparian_buffer_efficiencies_P[[i]], by = "comid") %>%
      mutate(
        across(
          .cols = any_of(RiparianBuffer_BMPs), 
          .fn = ~case_when(
            . == -999 ~ -999, . > 0 ~ (. * loading_per_bankft_kg_ftyr)
          )
        )
      ) %>%
      select(comid, season, year, any_of(RiparianBuffer_BMPs)) %>%
      mutate(season = rep(c(1, 2, 3, 4), times = nrow(riparian.loadings_tp[[i]])/4))
    
  }
}

#
#
## Make Dummy Variables if Modules aren't used ------

if(length(Urban_BMPs) == 0) {
  if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    urban_bmp_dummy_tn <- foreach(i = 1:length(streamcat_subset_tn)) %do% {
      data.frame(comid = unique(streamcat_subset_tn[[i]]), none = 0) %>%
        mutate(comid = paste0("'", comid, "'"))
    }
  }
  
  if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
    urban_bmp_dummy_tp <- foreach(i = 1:length(streamcat_subset_tp)) %do% {
      data.frame(comid = unique(streamcat_subset_tp[[i]]), none = 0) %>%
        mutate(comid = paste0("'", comid, "'"))
    }
  }
  
  urban_bmp_dummy <- data.frame(
    comid = unique(streamcat_subset_all), none = 0
  ) %>%
    mutate(comid = paste0("'", comid, "'"))
}

  if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    ripbuf_bmp_dummy_tn <- foreach(i = 1:length(streamcat_subset_tn)) %do% {
      comids <- unique(streamcat_subset_tn[[i]]$comid)
      if(length(comids) > 0) {
        data.frame(comid = comids, none = 0) %>%
          mutate(comid = paste0("'", comid, "'"))
      } else {
        data.frame(comid = character(0), none = numeric(0))
      }
    }
  }
  
  if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
    ripbuf_bmp_dummy_tp <- foreach(i = 1:length(streamcat_subset_tp)) %do% {
      comids <- unique(streamcat_subset_tp[[i]]$comid)
      if(length(comids) > 0) {
        data.frame(comid = comids, none = 0) %>%
          mutate(comid = paste0("'", comid, "'"))
      } else {
        data.frame(comid = character(0), none = numeric(0))
      }
    }
  }
  
  ripbuf_bmp_dummy <- data.frame(
    comid = unique(streamcat_subset_all), none = 0
  ) %>%
    mutate(comid = paste0("'", comid, "'"))


if(length(Ag_BMPs) == 0) {
  if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    ag_bmp_dummy_tn <- foreach(i = 1:length(streamcat_subset_tn)) %do% {
      comids <- unique(streamcat_subset_tn[[i]]$comid)
      if(length(comids) > 0) {
        data.frame(comid = comids, none = 0) %>%
          mutate(comid = paste0("'", comid, "'"))
      } else {
        data.frame(comid = character(0), none = numeric(0))
      }
    }
  }
  
  if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
    ag_bmp_dummy_tp <- foreach(i = 1:length(streamcat_subset_tp)) %do% {
      comids <- unique(streamcat_subset_tp[[i]]$comid)
      if(length(comids) > 0) {
        data.frame(comid = comids, none = 0) %>%
          mutate(comid = paste0("'", comid, "'"))
      } else {
        data.frame(comid = character(0), none = numeric(0))
      }
    }
  }
  
  ag_bmp_dummy <- data.frame(
    comid = unique(streamcat_subset_all), none = 0
  ) %>%
    mutate(comid = paste0("'", comid, "'"))
}

#
#
## Specify septic BMPS ------------------------------------------------------
temp_septic_BMPs <- user_specs_BMPs[
  user_specs_BMPs$BMP_Selection == "X" & (user_specs_BMPs$BMP_Category == "septic"),
]
Septic_BMPs <- paste0(temp_septic_BMPs$BMP)

# Set a flag to check if septic data is available
septic_data_available <- FALSE

# Check if septic BMPs are selected but no septic data exists
if(length(Septic_BMPs) > 0) {
  
  # Check if septic data exists and has implementation potential
  total_septic_parcels <- 0
  
  # Check septic upgrade data
  if(exists("septic.upgrade") && nrow(septic.upgrade) > 0) {
    total_septic_parcels <- total_septic_parcels + sum(septic.upgrade$parcels, na.rm = TRUE)
    if(sum(septic.upgrade$parcels, na.rm = TRUE) > 0) {
      septic_data_available <- TRUE
    }
  }
  
  # Check septic conversion data
  if(exists("septic.conversion") && nrow(septic.conversion) > 0) {
    total_septic_parcels <- total_septic_parcels + sum(septic.conversion$parcels, na.rm = TRUE)
    if(sum(septic.conversion$parcels, na.rm = TRUE) > 0) {
      septic_data_available <- TRUE
    }
  }
  
  # Stop execution if septic BMPs are selected but no data is available
  if(!septic_data_available || total_septic_parcels == 0) {
    stop(paste0(
      "\n\nERROR: Septic BMPs are selected in user specifications but no septic implementation data is available in the watershed.\n",
      "Selected septic BMPs: ", paste(Septic_BMPs, collapse = ", "), "\n",
      "Total septic parcels available: ", total_septic_parcels, "\n\n",
      "SOLUTION: Please remove septic BMPs from your user specifications file (set BMP_Selection to blank instead of 'X' for septic BMPs) and re-run the analysis.\n",
      "Alternatively, verify that septic data files contain valid data for your watershed.\n"
    ))
  }
  
  # If we get here, septic data is available, proceed normally
  message(paste0("Septic data verified: ", total_septic_parcels, " septic parcels available for implementation."))
}

if(length(Septic_BMPs) == 0) {
  if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    septic_bmp_dummy_tn <- foreach(i = 1:length(streamcat_subset_tn)) %do% {
      comids <- unique(streamcat_subset_tn[[i]]$comid)
      if(length(comids) > 0) {
        data.frame(comid = comids, none = 0) %>%
          mutate(comid_form = paste0("'", comid, "'"))
      } else {
        data.frame(comid_form = character(0), none = numeric(0))
      }
    }
  }
  
  if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
    septic_bmp_dummy_tp <- foreach(i = 1:length(streamcat_subset_tp)) %do% {
      comids <- unique(streamcat_subset_tp[[i]]$comid)
      if(length(comids) > 0) {
        data.frame(comid = comids, none = 0) %>%
          mutate(comid_form = paste0("'", comid, "'"))
      } else {
        data.frame(comid_form = character(0), none = numeric(0))
      }
    }
  }
  septic_bmp_dummy <- data.frame(
    comid = unique(streamcat_subset_all), none = 0
  ) %>%
    mutate(comid_form = paste0("'", comid, "'"))
}

#
#
#
# PART 4: WRITE AMPL COMMAND FILE ------- 
cat(
  "#WMOST Optimization Screening Tool AMPL command file
solve;
option display_precision 10;
option presolve_warnings -1;
display solve_result_num, solve_result;
display cost.result;
display cost;
#option display_1col 10000000000;
option display_width 100000000000;
option display_transpose -10000;
option omit_zero_rows 1;
option omit_zero_cols 1;
display seasons;
display years;
display point_dec;
display urban_frac;
display ag_frac;
display ripbuf_length;
",
  file = paste(OutPath, "STcommand_seasonal.amp", sep=""),
  sep = "\n"
)

# If septic data is available, add septic-specific commands
if(length(Septic_BMPs) > 0 & septic_data_available) {
  cat(
    "display septic_frac;\n",
    file = paste(OutPath, "STcommand_seasonal.amp", sep=""),
    append = TRUE
  )
}

  if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
      cat(
        paste0("\ndisplay loads_lim_N1",";"),
        file = paste(OutPath, "STcommand_seasonal.amp", sep=""),
        append = T
      )
    }

  if("TP" %in% user_specs_loadingtargets$TN_or_TP[user_specs_loadingtargets$watershed_name %in% target_selection]) {
      cat(
        paste0("\ndisplay loads_lim_P1", ";"),
        file = paste(OutPath, "STcommand_seasonal.amp", sep=""),
        append = T
      )
    }
#
#
#
# PART 5: WRITE AMPL DATA FILE -------

#
#
## Write urban and agricultural BMPs as vectors ----- 
bmp_urban <- user_specs_BMPs[
  user_specs_BMPs$BMP_Selection == "X" & 
    (user_specs_BMPs$BMP_Category == "urban"),
]
### New York has different design depths
bmp_urban$UserSpec_RD_in_NY <- 1.5
bmp_urban[bmp_urban$BMP == "Porous_Pavement_w_underdrain",]$UserSpec_RD_in_NY <- 1.5 * 12

bmp_urban_vec_comma <- paste0("'", bmp_urban$BMP, "'")
bmp_urban_vec_comma[-length(bmp_urban_vec_comma)] <- paste0(
  bmp_urban_vec_comma[-length(bmp_urban_vec_comma)], ','
)

bmp_urban_vec <- as.character(trimws(gsub(",", "  ", bmp_urban_vec_comma)))
bmp_urban_vec_direct <- bmp_urban$BMP

pervbmp_urban_vec_comma <- paste0(
  "'", with(bmp_urban, BMP[which(grepl("Porous_Pavement", BMP))]), "'"
)
pervbmp_urban_vec_comma[-length(pervbmp_urban_vec_comma)] <- paste0(
  pervbmp_urban_vec_comma[-length(pervbmp_urban_vec_comma)], ','
)
pervbmp_urban_vec <- as.character(gsub(",", "  ", pervbmp_urban_vec_comma))

bmp_ag <- user_specs_BMPs[
  user_specs_BMPs$BMP_Selection == "X" & (user_specs_BMPs$BMP_Category == "ag"),
]
bmp_ag_vec_comma <- paste0("'", bmp_ag$BMP, "'")
bmp_ag_vec_comma[-length(bmp_ag_vec_comma)] <- paste0(
  bmp_ag_vec_comma[-length(bmp_ag_vec_comma)], ','
)

bmp_ag_vec <- as.character(gsub(",","  ", bmp_ag_vec_comma))
bmp_ag_vec_direct <- bmp_ag$BMP

bmp_ripbuf_vec_comma <- paste0("'", RiparianBuffer_BMPs, "'")
bmp_ripbuf_vec_comma[-length(bmp_ripbuf_vec_comma)] <- paste0(
  bmp_ripbuf_vec_comma[-length(bmp_ripbuf_vec_comma)], ','
)
bmp_ripbuf_vec <- as.character(gsub(",","  ", bmp_ripbuf_vec_comma))

#*# HP Septic Addition: 5-2-2025
# Filter Septic_BMPs to only those available in the septic data
available_septic_bmps <- character(0)

# Add upgrade BMPs that are available in the septic.upgrade data
if(exists("septic.upgrade") && nrow(septic.upgrade) > 0) {
  available_upgrade_classes <- unique(septic.upgrade$SepticUpgradeClass)
  available_upgrade_bmps <- paste0("class_", available_upgrade_classes, "_upgrade")
  available_septic_bmps <- c(available_septic_bmps, available_upgrade_bmps)
}

# Add sewer conversion BMP if available in the septic.conversion data
#if(exists("septic.conversion") && nrow(septic.conversion) > 0) {
#  available_septic_bmps <- c(available_septic_bmps, "Sewer_convert")
#}

# ND: Add sewer conversion BMP even if not available to avoid AMPL error
available_septic_bmps <- c(available_septic_bmps, "Sewer_convert")

# Filter Septic_BMPs to only include those available in the data and update the main variable
Septic_BMPs <- Septic_BMPs[Septic_BMPs %in% available_septic_bmps]

# Only proceed if we have valid septic BMPs after filtering
if(length(Septic_BMPs) == 0) {
  warning("No septic BMPs are available in the data. Septic functionality will be disabled.")
}

bmp_septic_vec_comma <- paste0("'", Septic_BMPs, "'")
if(length(Septic_BMPs) > 1) {
  bmp_septic_vec_comma[-length(bmp_septic_vec_comma)] <- paste0(
    bmp_septic_vec_comma[-length(bmp_septic_vec_comma)], ','
  )
}
bmp_septic_vec_direct <- Septic_BMPs
bmp_septic_vec <- as.character(trimws(gsub(",","  ", bmp_septic_vec_comma)))

### Subset dataframes to reaches in our analysis

# Create a complete list of all COMIDs that will be in the model
# Ensure comid type matches the area dataframe
all_model_comids <- data.frame(comid = unique(reaches_all$comid), stringsAsFactors = FALSE)
all_model_comids$comid <- as.character(all_model_comids$comid)

# Ensure area$comid is also character for consistent joining
area$comid <- as.character(area$comid)

# Merge with area data, filling missing values with 0
temp_area_dat <- all_model_comids %>%
  left_join(area, by = "comid") %>%
  mutate(
    urban_ac = ifelse(is.na(urban_ac), 0, urban_ac),
    ag_ac = ifelse(is.na(ag_ac), 0, ag_ac)
  ) %>%
  arrange(comid) %>%
  mutate(comid_form = paste0("'", comid, "'"))

# Identify COMIDs that were zero-filled due to lack of StreamCat data
# Note: area dataframe is based on SPARROW data merged with StreamCat
# COMIDs missing from SPARROW will also be missing from area
# We only want to report COMIDs that have SPARROW data but lack StreamCat data
missing_area_comids <- temp_area_dat %>%
  anti_join(area, by = "comid") %>%
  pull(comid)

# Check which missing COMIDs actually exist in SPARROW
# (if they don't exist in SPARROW, a better error message will be shown elsewhere)
missing_area_with_sparrow <- missing_area_comids[
  missing_area_comids %in% c(sparrow_cons_out_tn$comid, sparrow_cons_out_tp$comid)
]

if(length(missing_area_with_sparrow) > 0) {
  cat("\nNote: The following", length(missing_area_with_sparrow), "COMIDs are in the model but lack StreamCat land use data.\n")
  cat("Setting urban and agricultural areas to 0 for these COMIDs:\n")
  cat(paste(head(missing_area_with_sparrow, 20), collapse = ", "))
  if(length(missing_area_with_sparrow) > 20) {
    cat(paste0(", ... and ", length(missing_area_with_sparrow) - 20, " more"))
  }
  cat("\n\n")
}

area_dat <- temp_area_dat %>% select(c("comid_form", "urban_ac", "ag_ac"))

## Riparian Buffer Coefficients -------
if(length(RiparianBuffer_BMPs) > 0) {
  streambanklength_total_dat <- riparian_buffer_maximp %>% 
    full_join(., streamcat_subset_all, by = "comid") %>%
    filter(comid %in% streamcat_subset_all$comid) %>%
    arrange(comid) %>%
    mutate(
      comid_form = paste0("'", comid, "'"), 
      totalbanklength_ft = case_when(
        is.na(totalbanklength_ft) ~ 0, 
        !is.na(totalbanklength_ft) ~ totalbanklength_ft
      )
    ) %>%
    select(comid_form, totalbanklength_ft)
  
  streambanklength_available_dat <-  riparian_buffer_maximp %>% 
    full_join(., streamcat_subset_all, by = "comid") %>%
    filter(comid %in% streamcat_subset_all$comid) %>%
    arrange(comid) %>%
    mutate(comid_form = paste0("'", comid, "'")) %>%
    mutate(
      across(
        .cols = any_of(RiparianBuffer_BMPs), 
        .fns = ~case_when(is.na(.) ~ 0, !is.na(.) ~ .)
      )
    ) %>%
    select(comid_form, any_of(RiparianBuffer_BMPs))
}

temp_runoffcoeff_dat <- as.data.frame(runoffcoeff)[
  runoffcoeff$comid %in% streamcat_subset_all$comid,
]
temp_runoffcoeff_dat <- temp_runoffcoeff_dat[order(temp_runoffcoeff_dat$comid),]
temp_runoffcoeff_dat$comid_form <- paste0("'", temp_runoffcoeff_dat$comid, "'")
runoffcoeff_dat <- temp_runoffcoeff_dat %>% 
  select(c("comid_form", "runoffcoeff"))

#
#
## Specify state COMIDs ----- 
### Note to user: StreamCat datasets list catchments that are on the border of two states within both states' datasets
### Because agricultural costs are in some cases state-specific, we select costs associated with the state that leads alphabetically for such catchments
COMID_State <- distinct(
  StreamCat_api[
    StreamCat_api$comid %in% streamcat_subset_all$comid, c("comid", "State")
  ]
) %>% 
  group_by(comid) %>%
  arrange(State) %>%
  summarize(State = State[1], .groups = "drop") #*#

### Multiply all loads by revised del_frac
temp_inc_tn_dat <- foreach(i = 1:length(target_selection)) %do% {
  
  watershed_comid <- reaches_TN_target %>%
    filter(watershed_name == target_selection[i])
  
  temp_inc_tn_dat_tmp <- merge(
    inc_tn_rev[inc_tn_rev$comid %in% watershed_comid$catchment_comid,],
    delfrac_rev_tn[[i]],
    by = c("comid", "season", "year"),
    all.x = TRUE
  )
  
  temp_inc_tn_dat_tmp[-c(1,2,3,8)] <- temp_inc_tn_dat_tmp[-c(1,2,3,8)] *
    temp_inc_tn_dat_tmp[["DEL_FRAC"]]
  
  temp_inc_tn_dat_tmp <- temp_inc_tn_dat_tmp[
    order(temp_inc_tn_dat_tmp$comid), 
  ]
}

temp_inc_tp_dat <- foreach(i = 1:length(target_selection)) %do% {
  
  watershed_comid <- reaches_TP_target %>%
    filter(watershed_name == target_selection[i])
  
  temp_inc_tp_dat_tmp <- merge(
    inc_tp[inc_tp$comid %in% watershed_comid$catchment_comid,],
    delfrac_rev_tp[[i]],
    by = c("comid", "season", "year"),
    all.x = TRUE
  )
  
  temp_inc_tp_dat_tmp[-c(1,2,3,8)] <- temp_inc_tp_dat_tmp[-c(1,2,3,8)] * 
    temp_inc_tp_dat_tmp[["DEL_FRAC"]]
  
  temp_inc_tp_dat_tmp <- temp_inc_tp_dat_tmp[
    order(temp_inc_tp_dat_tmp$comid), 
  ]
  
}

temp_sm_N_dat <- foreach(i = 1:length(target_selection)) %do% {
  
  watershed_comid <- reaches_TN_target %>%
    filter(watershed_name == target_selection[i])
  
  temp_sm_N_dat_tmp <- sm_N[sm_N$comid %in% watershed_comid$catchment_comid,] %>%
    arrange(comid)
  
}

temp_sm_P_dat <- foreach(i = 1:length(target_selection)) %do% {
  
  watershed_comid <- reaches_TP_target %>%
    filter(watershed_name == target_selection[i])
  
  temp_sm_P_dat_tmp <- sm_P[sm_P$comid %in% watershed_comid$catchment_comid,] %>%
    arrange(comid)
}

#
#
## Riparian Data ------ 
if(length(RiparianBuffer_BMPs) > 0) {
  temp_riparian_tn_dat <- foreach(i = 1:length(target_selection)) %do% {
    
    watershed_comid <- reaches_TN_target %>%
      filter(watershed_name == target_selection[i])
    
    # fix - remove duplicates within each dataframe before merge
    riparian_buffer_removal_N[[i]] <- riparian_buffer_removal_N[[i]] %>% distinct(comid, season, year, .keep_all = TRUE)
    delfrac_rev_tn[[i]] <- delfrac_rev_tn[[i]] %>% distinct(comid, season, year, .keep_all = TRUE)    
    
    temp_riparian_tn_dat_tmp <- merge(
      riparian.loadings_tn[[i]],
      delfrac_rev_tn[[i]],
      by = c("comid", "season", "year"),
      all = TRUE
    ) %>%
      mutate(in_riparian = N_riparian_kgyr * DEL_FRAC) %>%
      arrange(comid, season) %>%
      select(comid, season, year, in_riparian)
    
  }
  
  temp_riparian_tp_dat <- foreach(i = 1:length(target_selection)) %do% {
    
    watershed_comid <- reaches_TP_target %>%
      filter(watershed_name == target_selection[i])
    
    # fix - remove duplicates within each dataframe before merge
    riparian_buffer_removal_P[[i]] <- riparian_buffer_removal_P[[i]] %>% distinct(comid, season, year, .keep_all = TRUE)
    delfrac_rev_tp[[i]] <- delfrac_rev_tp[[i]] %>% distinct(comid, season, year, .keep_all = TRUE)    
    
    temp_riparian_tp_dat_tmp <- merge(
      riparian.loadings_tp[[i]],
      delfrac_rev_tp[[i]],
      by = c("comid", "season", "year"),
      all = TRUE
    ) %>%
      mutate(ip_riparian = P_riparian_kgyr * DEL_FRAC) %>%
      arrange(comid, season) %>%
      select(comid, season, year, ip_riparian)
    
  }

    temp_riparian_tn_removal <- foreach(i = 1:length(target_selection)) %do% {
    
    watershed_comid <- reaches_TN_target %>%
      filter(watershed_name == target_selection[i])

    # fix - remove duplicates within each dataframe before merge
      riparian_buffer_removal_N[[i]] <- riparian_buffer_removal_N[[i]] %>% distinct(comid, season, year, .keep_all = TRUE)
      delfrac_rev_tn[[i]] <- delfrac_rev_tn[[i]] %>% distinct(comid, season, year, .keep_all = TRUE)  

    riparian_buffer_removal_N[[i]]$season <- as.character(riparian_buffer_removal_N[[i]]$season)
    delfrac_rev_tn[[i]]$season <- as.character(delfrac_rev_tn[[i]]$season)
    
    temp_riparian_tn_removal_tmp <- merge(
      riparian_buffer_removal_N[[i]],
      delfrac_rev_tn[[i]],
      by = c("comid", "season", "year"),
      all = TRUE,
      allow.cartesian=TRUE
      
    ) %>%
      mutate(
        across(
          .col = any_of(RiparianBuffer_BMPs), 
          .fn =  ~case_when(is.na(.) ~ 0, . == -999 ~ 0,!is.na(.) ~ . * DEL_FRAC) #@HP: identified as likely source of large negative riparian removals. needs to have a case that when it's -999, riparian removal is 0. #@HP: Has now been addressed
        )
      ) %>%
      arrange(year, comid, season) %>%
      select(comid, season, year, any_of(RiparianBuffer_BMPs))
    
  }
  
  temp_riparian_tp_removal <- foreach(i = 1:length(target_selection)) %do% {
    
    watershed_comid <- reaches_TN_target %>%
      filter(watershed_name == target_selection[i])

    # fix - remove duplicates within each dataframe before merge
    riparian_buffer_removal_P[[i]] <- riparian_buffer_removal_P[[i]] %>% distinct(comid, season, year, .keep_all = TRUE)
    delfrac_rev_tp[[i]] <- delfrac_rev_tp[[i]] %>% distinct(comid, season, year, .keep_all = TRUE)  
    
        
    riparian_buffer_removal_P[[i]]$season <- as.character(riparian_buffer_removal_P[[i]]$season)
    delfrac_rev_tp[[i]]$season <- as.character(delfrac_rev_tp[[i]]$season)
    
    temp_riparian_tp_removal_tmp <- merge(
      riparian_buffer_removal_P[[i]],
      delfrac_rev_tp[[i]],
      by = c("comid", "season", "year"),
      all = TRUE,
      allow.cartesian=TRUE
    ) %>%
      mutate(
        across(
          .col = any_of(RiparianBuffer_BMPs), 
          .fn =  ~case_when(is.na(.) ~ 0, . == -999 ~ 0,!is.na(.) ~ . * DEL_FRAC) #@HP: identified as likely source of large negative riparian removals. needs to have a case that when it's -999, riparian removal is 0. #@HP: Addressed
        )
      ) %>%
      arrange(year, comid, season) %>%
      select(comid, season, year, any_of(RiparianBuffer_BMPs)) %>%
      distinct()
    
  }
}

#
#
## StreamCat Data -----
temp_inc_tn_dat_SC <- foreach(i = 1:length(temp_inc_tn_dat)) %do% {
  temp_inc_tn_dat[[i]][
    temp_inc_tn_dat[[i]]$comid %in% streamcat_subset_tn[[i]]$comid, 
  ]
}

# Check for missing COMIDs immediately after filtering
# This catches COMIDs in streamcat but not in SPARROW data
if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
  for(i in 1:length(temp_inc_tn_dat_SC)) {
    if (!is.null(temp_inc_tn_dat_SC[[i]]) && !is.null(streamcat_subset_tn[[i]])) {
      expected_comids <- unique(streamcat_subset_tn[[i]]$comid)
      present_comids <- unique(temp_inc_tn_dat_SC[[i]]$comid)
      missing_comids <- setdiff(expected_comids, present_comids)
      
      if(length(missing_comids) > 0 && !all(is.na(missing_comids))) {
        cat("\n")
        cat("================================================================================\n")
        cat("                         CRITICAL DATA ERROR\n")
        cat("================================================================================\n")
        cat("\n")
        cat("ERROR: SPARROW model output is missing TN data for COMID(s) in watershed", i, "\n")
        cat("\n")
        cat("Missing COMID(s):", paste(missing_comids, collapse=", "), "\n")
        cat("Total missing:", length(missing_comids), "COMID(s)\n")
        cat("\n")
        cat("PROBLEM:\n")
        cat("  These COMIDs exist in the StreamCat dataset but are NOT present in the\n")
        cat("  SPARROW model output file (Predict_NoBFlowN_WSeptic.csv).\n")
        cat("\n")
        cat("REQUIRED ACTIONS:\n")
        cat("  1. Verify the SPARROW output file is complete and up-to-date\n")
        cat("  2. Check if missing COMIDs are within the SPARROW model domain\n")
        cat("  3. Investigate why these COMIDs lack SPARROW predictions\n")
        cat("  4. Either:\n")
        cat("     a) Obtain complete SPARROW data for all watershed COMIDs, OR\n")
        cat("     b) Remove missing COMIDs from the watershed delineation\n")
        cat("\n")
        cat("PREPROCESSING STOPPED - Cannot proceed with incomplete data.\n")
        cat("================================================================================\n")
        stop("Missing SPARROW TN data for ", length(missing_comids), " COMID(s) in watershed ", i, 
             ". See error message above for details.")
      }
    }
  }
}

temp_inc_tn_dat_other <- foreach(i = 1:length(temp_inc_tn_dat)) %do% {
  temp_inc_tn_dat[[i]][
    !(temp_inc_tn_dat[[i]]$comid %in% streamcat_subset_tn[[i]]$comid), 
  ]
}

temp_inc_tp_dat_SC <- foreach(i = 1:length(temp_inc_tp_dat)) %do% {
  temp_inc_tp_dat[[i]][
    temp_inc_tp_dat[[i]]$comid %in% streamcat_subset_tp[[i]]$comid, 
  ]
}

# Check for missing COMIDs immediately after filtering for TP
if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
  for(i in 1:length(temp_inc_tp_dat_SC)) {
    if (!is.null(temp_inc_tp_dat_SC[[i]]) && !is.null(streamcat_subset_tp[[i]])) {
      expected_comids <- unique(streamcat_subset_tp[[i]]$comid)
      present_comids <- unique(temp_inc_tp_dat_SC[[i]]$comid)
      missing_comids <- setdiff(expected_comids, present_comids)
      
      if(length(missing_comids) > 0 && !all(is.na(missing_comids))) {
        cat("\n")
        cat("================================================================================\n")
        cat("                         CRITICAL DATA ERROR\n")
        cat("================================================================================\n")
        cat("\n")
        cat("ERROR: SPARROW model output is missing TP data for COMID(s) in watershed", i, "\n")
        cat("\n")
        cat("Missing COMID(s):", paste(missing_comids, collapse=", "), "\n")
        cat("Total missing:", length(missing_comids), "COMID(s)\n")
        cat("\n")
        cat("PROBLEM:\n")
        cat("  These COMIDs exist in the StreamCat dataset but are NOT present in the\n")
        cat("  SPARROW model output file (Predict_P.csv).\n")
        cat("\n")
        cat("REQUIRED ACTIONS:\n")
        cat("  1. Verify the SPARROW output file is complete and up-to-date\n")
        cat("  2. Check if missing COMIDs are within the SPARROW model domain\n")
        cat("  3. Investigate why these COMIDs lack SPARROW predictions\n")
        cat("  4. Either:\n")
        cat("     a) Obtain complete SPARROW data for all watershed COMIDs, OR\n")
        cat("     b) Remove missing COMIDs from the watershed delineation\n")
        cat("\n")
        cat("PREPROCESSING STOPPED - Cannot proceed with incomplete data.\n")
        cat("================================================================================\n")
        stop("Missing SPARROW TP data for ", length(missing_comids), " COMID(s) in watershed ", i, 
             ". See error message above for details.")
            }
    }
  }
}

temp_inc_tp_dat_other <- foreach(i = 1:length(temp_inc_tp_dat)) %do% {
  temp_inc_tp_dat[[i]][
    !(temp_inc_tp_dat[[i]]$comid %in% streamcat_subset_tp[[i]]$comid), 
  ]
}


# (SC for StreamCat)
temp_sm_N_dat_SC <- foreach(i = 1:length(temp_sm_N_dat)) %do% {
  temp_sm_N_dat[[i]][
    temp_sm_N_dat[[i]]$comid %in% streamcat_subset_tn[[i]]$comid,
  ]
}

temp_sm_N_dat_other <- foreach(i = 1:length(temp_sm_N_dat)) %do% {
  temp_sm_N_dat[[i]][
    !(temp_sm_N_dat[[i]]$comid %in% streamcat_subset_tn[[i]]$comid),
  ]
}

# (SC for StreamCat)
temp_sm_P_dat_SC <- foreach(i = 1:length(temp_sm_P_dat)) %do% {
  temp_sm_P_dat[[i]][
    temp_sm_P_dat[[i]]$comid %in% streamcat_subset_tp[[i]]$comid,
  ]
}

temp_sm_P_dat_other <- foreach(i = 1:length(temp_sm_P_dat)) %do% {
  temp_sm_P_dat[[i]][
    !(temp_sm_P_dat[[i]]$comid %in% streamcat_subset_tp[[i]]$comid),
  ]
}

#
#
## Calculate sums of loads for optimization ----- 

if(actionable_load == TRUE){
invisible(
  if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    param_loads_lim_tn <-
      foreach(i = 1:length(temp_inc_tn_dat)) %do% {
        
        load_lim <- temp_inc_tn_dat[[i]] %>% 
          group_by(year, season) %>%
            summarize(total_load = sum(in_poin) + sum(in_urb) + sum(in_septic, na.rm = TRUE) + sum(in_ag) + sum(in_storage),#+ sum(in_other) 
                      load_lim = (1 - as.numeric(load_perc_reduc_tn[i])) * (sum(in_poin) + sum(in_urb) + sum(in_septic, na.rm = TRUE) + sum(in_ag) + sum(in_storage)),# + sum(in_other)
                      .groups = "drop"
            )
          load_lim %>%
            select(season, year, load_lim) #
        }
    }
  )
  print("Generating TN param load limit based only on actionable load components")
}else{
  print("Moving to generate TN param load limit based on all loads")
}
if(actionable_load == FALSE){
  invisible(
    if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
      param_loads_lim_tn <-
        foreach(i = 1:length(temp_inc_tn_dat)) %do% {
        load_lim <- temp_inc_tn_dat[[i]] %>% 
          group_by(year, season) %>%
          summarize(total_load = sum(in_poin) + sum(in_urb) + sum(in_septic, na.rm = TRUE) + sum(in_ag) + sum(in_storage)+ sum(in_other) ,#
                    load_lim = (1 - as.numeric(load_perc_reduc_tn[i])) * (sum(in_poin) + sum(in_urb) + sum(in_septic, na.rm = TRUE) + sum(in_ag) + sum(in_storage) + sum(in_other)),#
                    .groups = "drop"
          )
        
        load_lim %>%
          select(season, year, load_lim) #
         
      }
  }
)
  print("Generating TN param load limit by target percentage based on all load components")
}else{
  print("Generated TN param load limit by target percentage based only on actionable load components")
}

if(actionable_load == TRUE){
invisible(
  if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
    param_loads_lim_tp <-
      foreach(i = 1:length(temp_inc_tp_dat)) %do% {
        
        load_lim <- temp_inc_tp_dat[[i]] %>%
          group_by(year, season) %>%
          summarize(
              total_load = sum(ip_poin) + sum(ip_urb) + sum(ip_ag) + sum(ip_storage),# + sum(ip_other)
              load_lim = (1 - as.numeric(load_perc_reduc_tp[i])) * (sum(ip_poin) + sum(ip_urb) + sum(ip_ag) + sum(ip_storage)),# + sum(ip_other)
              .groups = "drop"
            )
          load_lim %>%
            select(season, year, load_lim) #
        }
    }
  )
  print("Generating TP param load limit by target percentage based only on actionable load components")
}else{
  print("Moving to generate TP param load limit by target percentage based on all loads")
}
if(actionable_load == FALSE){
  invisible(
    if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
      param_loads_lim_tp <-
        foreach(i = 1:length(temp_inc_tp_dat)) %do% {
        load_lim <- temp_inc_tp_dat[[i]] %>%
          group_by(year, season) %>%
          summarize(
            total_load = sum(ip_poin) + sum(ip_urb) + sum(ip_ag) + sum(ip_storage) + sum(ip_other),#
            load_lim = (1 - as.numeric(load_perc_reduc_tp[i])) * (sum(ip_poin) + sum(ip_urb) + sum(ip_ag) + sum(ip_storage) + sum(ip_other)),#
            .groups = "drop"
          )
        
        load_lim %>%
          select(season, year, load_lim) #


      }
  }
)
  print("Generating TP param load limit by target percentage based on all load components")
}else{
  print("Generated TP param load limit by target percentage based only on actionable load components")
}

#
#
## Preprocess COMIDs and BMPs to have '' ----
# Note: Data validation for missing COMIDs already performed earlier
# Execution would have stopped if any COMIDs were missing from SPARROW data

if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
  inc_tn_dat <- foreach(i = 1:length(temp_inc_tn_dat_SC)) %do% {
    if (!is.null(temp_inc_tn_dat_SC[[i]]) && nrow(temp_inc_tn_dat_SC[[i]]) > 0) {
      temp_inc_tn_dat_opt <- temp_inc_tn_dat_SC[[i]]
      temp_inc_tn_dat_opt$comid_form <- paste0(
        "'", temp_inc_tn_dat_opt$comid, "'"
      )
      temp_inc_tn_dat_opt %>% 
        select(c("comid_form", "season", "year", "in_poin", "in_urb", "in_ag", "in_septic", "in_storage", "in_other")) 
    } else {
      NULL
    }
  }
}
# Format TP incremental loads for AMPL
# Data validation for missing COMIDs already performed earlier (see lines ~2275)
if ("TP" %in% user_specs_loadingtargets$TN_or_TP) {
  inc_tp_dat <- foreach(i = 1:length(temp_inc_tp_dat_SC)) %do% {
    if (!is.null(temp_inc_tp_dat_SC[[i]]) && nrow(temp_inc_tp_dat_SC[[i]]) > 0) {
      temp_inc_tp_dat_opt <- temp_inc_tp_dat_SC[[i]]
      temp_inc_tp_dat_opt$comid_form <- paste0("'", temp_inc_tp_dat_opt$comid, "'")
      temp_inc_tp_dat_opt %>%
        select(c("comid_form", "season", "year", "ip_poin", "ip_urb", "ip_ag", "ip_storage", "ip_other"))
    } else {
      NULL  # Skip empty data frames
    }
  }
}

#
#
## Format transfer coefficients -----
# Data validation for missing COMIDs already performed earlier (see lines ~2228 and ~2275)

# Format TN transfer coefficients for AMPL
if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
  sm_N_dat <- foreach(i = 1:length(temp_sm_N_dat_SC)) %do% {
    if (!is.null(temp_sm_N_dat_SC[[i]]) && nrow(temp_sm_N_dat_SC[[i]]) > 0) {
      temp_sm_N_dat_opt <- temp_sm_N_dat_SC[[i]]
      temp_sm_N_dat_opt$comid_form <- paste0(
        "'", temp_sm_N_dat_opt$comid, "'"
      )
      temp_sm_N_dat_opt$year_form <- paste0(
        "'", temp_sm_N_dat_opt$year, "'"
      )
      temp_sm_N_dat_opt %>%
        select(c('comid_form', season, year, 'coeff'))
    } else {
      NULL
    }
  }
}

# Format TP transfer coefficients for AMPL
if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
  sm_P_dat <- foreach(i = 1:length(temp_sm_P_dat_SC)) %do% {
    if (!is.null(temp_sm_P_dat_SC[[i]]) && nrow(temp_sm_P_dat_SC[[i]]) > 0) {
      temp_sm_P_dat_opt <- temp_sm_P_dat_SC[[i]]
      temp_sm_P_dat_opt$comid_form <- paste0(
        "'", temp_sm_P_dat_opt$comid, "'"
      )
      temp_sm_P_dat_opt %>%
        select(c('comid_form', season, year, 'coeff'))
    } else {
      NULL  # Skip empty data frames
    }
  }
}

if(length(RiparianBuffer_BMPs) > 0) {
  riparian_tn_dat <- foreach(i = 1:length(temp_riparian_tn_dat)) %do% {
    
    temp_riparian_tn_dat[[i]] %>%
      filter(comid %in% streamcat_subset_tn[[i]]$comid) %>%
      mutate(comid_form = paste0("'", comid, "'")) %>%
      mutate(season_form = paste0("'", season, "'")) %>%
      mutate(year_form = paste0("'", year, "'")) %>%
      select(comid_form, season, year, in_riparian)
    
  }
  
  riparian_tp_dat <- foreach(i = 1:length(temp_riparian_tp_dat)) %do% {
    
    temp_riparian_tp_dat[[i]] %>%
      filter(comid %in% streamcat_subset_tp[[i]]$comid) %>%
      mutate(comid_form = paste0("'", comid, "'")) %>%
      mutate(season_form = paste0("'", season, "'")) %>%
      mutate(year_form = paste0("'", year, "'")) %>%
      select(comid_form, season, year, ip_riparian)
    
  }
  
  riparian_tn_removal <- foreach(i = 1:length(temp_riparian_tn_removal)) %do% {
    
    temp_riparian_tn_removal[[i]] %>%
      filter(comid %in% streamcat_subset_tn[[i]]$comid) %>%
      mutate(comid_form = paste0("'", comid, "'")) %>%
      mutate(season_form = paste0("'", season, "'")) %>%
      mutate(year_form = paste0("'", year, "'")) %>%
      select(comid_form, season, year, any_of(RiparianBuffer_BMPs))
    
  }
  
  riparian_tp_removal <- foreach(i = 1:length(temp_riparian_tp_removal)) %do% {
    
    temp_riparian_tp_removal[[i]] %>%
      filter(comid %in% streamcat_subset_tp[[i]]$comid) %>%
      mutate(comid_form = paste0("'", comid, "'")) %>%
      mutate(season_form = paste0("'", season, "'")) %>%
      mutate(year_form = paste0("'", year, "'")) %>%
      select(comid_form, season, year, any_of(RiparianBuffer_BMPs))
    
  }
}

#
#
## Format urban BMP costs (do not vary by state) -----
# Note to user: urban BMP costs do not vary by state, thus can set capital and operations costs to the first provided cost alphabetically by state
if(length(Urban_BMPs) > 0) {
  temp_urban_costs_dat <- bmp_costs %>%
    filter(category == "urban") %>%
    select(
      bmp, contains(c("capital", "operations"))
    ) %>%
    pivot_longer(
      ., 
      cols = contains(c("capital", "operations")), 
      names_to = c("CostType", "State"), 
      names_sep = "_"
    ) %>%
    group_by(bmp, CostType) %>%
    summarise(
      index = if(length(which(!is.na(value)) > 0)) {min(which(!is.na(value)))} else {NA},
      Cost = value[index], 
      State = State[index],
      .groups = "keep"
    )
  
  if(
    any(is.na(temp_urban_costs_dat)) | 
    any(!(c("capital", "operations") %in% temp_urban_costs_dat$CostType))
  ) {
    stop(
      paste0(
        "Urban costs (capital and/or operations) for ", 
        paste(
          temp_urban_costs_dat %>% 
            filter(is.na(Cost)) %>% 
            select(bmp) %>% 
            unique() %>%
            pull(), 
          collapse = ", "
        ),
        paste(
          c("capital", "operations")[
            which(
              !(c("capital", "operations") %in% temp_urban_costs_dat$CostType)
            )
          ]
        ),
        " not provided. Please ensure costs are available in `01_UserSpecs_BMPs.csv`"
      )
    )
  }
  
  print(
    paste0(
      "Urban base costs are assumed to be the same across states. RBEROST is using values from ",
      paste(unique(temp_urban_costs_dat$State), collapse = ", "), "."
    )
  )
  
  urban_costs_dat <- temp_urban_costs_dat %>%
    pivot_wider(id_cols = bmp, names_from = CostType, values_from = Cost)
  
  urban_costs_dat$bmp_form  <- paste0("'", urban_costs_dat$bmp, "'") 
}

#
#
## Separate parameters for ag_capital and ag_operations and format costs data ----
if(length(Ag_BMPs) > 0) {
  
  temp_bmp_costs_ag <- merge(
    bmp_costs %>%
      filter(category == "ag") %>%
      select(
        bmp, contains(c("capital", "operations"))
      ),
    COMID_State
  ) %>%
    mutate(
      capital = my_key_fun(., "State", ~paste0("capital_", .x)),
      operations = my_key_fun(., "State", ~paste0("operations_", .x))
    ) %>%
    arrange(comid) %>%
    mutate(comid_form = paste0("'", comid, "'"))
  
  if(
    any(is.na(temp_bmp_costs_ag$capital)) | 
    any(is.na(temp_bmp_costs_ag$operations))
  ) {
    stop(
      paste0(
        "Ag ",
        if(
          any(is.na(temp_bmp_costs_ag$capital)) & 
          any(is.na(temp_bmp_costs_ag$operations))
        ) {paste("capital and operations ")} else if (
          any(is.na(temp_bmp_costs_ag$capital))
        ) {paste("capital ")} else {paste("operations ")},
        "costs for ", 
        paste(
          temp_bmp_costs_ag %>% 
            filter(is.na(capital) | is.na(operations)) %>% 
            select(bmp) %>% 
            unique() %>%
            pull(), 
          collapse = ", "
        ),
        " in ",
        paste(
          temp_bmp_costs_ag %>% 
            filter(is.na(capital) | is.na(operations)) %>% 
            select(State) %>% 
            unique() %>%
            pull(), 
          collapse = ", "
        ),
        " not provided. Please ensure costs are available in `01_UserSpecs_BMPs.csv`"
      )
    )
  }
  
  bmp_costs_ag_capital <- reshape2::dcast(
    temp_bmp_costs_ag[, c("comid", "comid_form", "bmp", "capital")], 
    comid_form + comid ~ bmp,
    value.var = "capital"
  )
  bmp_costs_ag_operations <- reshape2::dcast(
    temp_bmp_costs_ag[, c("comid", "comid_form", "bmp", "operations")],
    comid_form + comid ~ bmp,
    value.var = "operations"
  )
  
  bmp_costs_ag_capital_rev <- bmp_costs_ag_capital[
    order(bmp_costs_ag_capital$comid),
  ]
  ag_costs_cap_dat <- bmp_costs_ag_capital_rev[
    ,names(bmp_costs_ag_capital_rev) != "comid"
  ]
  ag_costs_cap_dat <- ag_costs_cap_dat %>% select(comid_form, everything())
  
  bmp_costs_ag_operations_rev <- bmp_costs_ag_operations[
    order(bmp_costs_ag_operations$comid),
  ]
  ag_costs_op_dat <- bmp_costs_ag_operations_rev[ 
    ,names(bmp_costs_ag_operations_rev) != "comid"
  ]
  ag_costs_op_dat <- ag_costs_op_dat %>% select(comid_form, everything())
}

#
#
## Format Septic BMP Costs ----
if(length(Septic_BMPs) > 0){
  
  septic_bmp_costs_temp <- user_specs_BMPs%>%
    filter(BMP_Selection == "X" & BMP_Category == "septic")%>%
    select(BMP_Category, BMP, capital_VT:operations_RI)%>%
    unique()%>%
    group_by(BMP_Category, BMP)%>%
    summarise(
      cap_cost = mean(capital_VT:capital_RI, na.rm = TRUE),
      op_cost = mean(operations_VT:operations_RI, na.rm = TRUE),
      .groups = "drop"
    )%>%
    merge(., user_specs_BMPs%>%
            filter(BMP_Selection == "X" & BMP_Category == "septic")%>%
            select(BMP_Category, BMP, capital_VT, operations_VT), by = c("BMP_Category", "BMP")
    )%>%
    mutate(
      cap_check = ifelse(cap_cost == capital_VT, 0, 1),
      op_check = ifelse(op_cost == operations_VT, 0, 1)
    )%>%
    select(-capital_VT, -operations_VT, -cap_check, -op_check)
  
  sep_convert_bmp_costs_temp <- septic.conversion%>%
    select(-parcels, -efficiency_per_parcel)%>%
    mutate(
      BMP = "Sewer_convert"
    )%>%
    merge(., septic_bmp_costs_temp%>%select(BMP, cap_cost, op_cost), by = c("BMP")
    )%>%
    select(comid, everything())
  
  sep_upgrade_bmp_costs_temp <- septic.upgrade%>%
    rename(BMP = SepticUpgradeClass)%>%
    mutate(
      BMP = paste0("class_", BMP, "_upgrade")
    )%>%
    select(-parcels, -efficiency_per_parcel)%>%
    merge(., septic_bmp_costs_temp%>%select(BMP, cap_cost, op_cost), by = c("BMP")
    )%>%
    select(comid, everything())
  
  septic_costs_temp <- rbind(sep_convert_bmp_costs_temp, sep_upgrade_bmp_costs_temp)
  
  septic_cap_costs <- septic_costs_temp %>%
    select(comid, BMP, cap_cost) %>%
    pivot_wider(
      id_cols = comid,
      names_from = BMP, 
      values_from = cap_cost
    ) %>%
    right_join(
      data.frame(comid = as.character(unique(unlist(streamcat_subset_tn, use.names = FALSE)))),
      by = "comid"
    ) %>%
    mutate(comid_form = paste0("'", comid, "'")) %>%
    select(comid_form, contains("convert"), contains("class"))
  
  septic_op_costs <- septic_costs_temp%>%
    select(comid, BMP, op_cost)%>%
    pivot_wider(
      id_cols = comid,
      names_from = BMP, 
      values_from = op_cost
    ) %>%
    right_join(
      data.frame(comid = as.character(unique(unlist(streamcat_subset_tn, use.names = FALSE)))),
      by = "comid"
    ) %>%
    mutate(comid_form = paste0("'", comid, "'"))%>%
    select(comid_form, contains("convert"), contains("class"))
  
  # replace NAs in the septic_bmp_costs dataframes with 1e30
  septic_cap_costs[is.na(septic_cap_costs)] <- 1e30
  septic_op_costs[is.na(septic_op_costs)] <- 1e30
}

#
#
## Format riparian costs ------
if(length(RiparianBuffer_BMPs) > 0) {
  temp_bmp_costs_ripbuf <- merge(
    bmp_costs %>%
      filter(category == "ripbuf") %>%
      select(
        bmp, contains(c("capital", "operations"))
      ),
    COMID_State
  ) %>%
    mutate(
      capital = my_key_fun(., "State", ~paste0("capital_", .x)),
      operations = my_key_fun(., "State", ~paste0("operations_", .x))
    ) %>%
    arrange(comid) %>%
    mutate(comid_form = paste0("'", comid, "'"))
  
  if(
    any(is.na(temp_bmp_costs_ripbuf$capital)) | 
    any(is.na(temp_bmp_costs_ripbuf$operations))
  ) {
    stop(
      paste0(
        "Riparian Buffer ",
        if(
          any(is.na(temp_bmp_costs_ripbuf$capital)) & 
          any(is.na(temp_bmp_costs_ripbuf$operations))
        ) {paste("capital and operations ")} else if (
          any(is.na(temp_bmp_costs_ripbuf$capital))
        ) {paste("capital ")} else {paste("operations ")},
        "costs for ", 
        paste(
          temp_bmp_costs_ripbuf %>% 
            filter(is.na(capital) | is.na(operations)) %>% 
            select(bmp) %>% 
            unique() %>%
            pull(), 
          collapse = ", "
        ),
        " in ",
        paste(
          temp_bmp_costs_ripbuf %>% 
            filter(is.na(capital) | is.na(operations)) %>% 
            select(State) %>% 
            unique() %>%
            pull(), 
          collapse = ", "
        ),
        " not provided. Please ensure costs are available in `01_UserSpecs_BMPs.csv`"
      )
    )
  }
  
  bmp_costs_ripbuf_capital <- reshape2::dcast(
    temp_bmp_costs_ripbuf[, c("comid", "comid_form", "bmp", "capital")], 
    comid_form + comid ~ bmp,
    value.var = "capital"
  )
  
  bmp_costs_ripbuf_operations <- reshape2::dcast(
    temp_bmp_costs_ripbuf[, c("comid", "comid_form", "bmp", "operations")],
    comid_form + comid ~ bmp,
    value.var = "operations"
  )
  
  bmp_costs_ripbuf_capital_rev <- bmp_costs_ripbuf_capital[
    order(bmp_costs_ripbuf_capital$comid),
  ]
  ripbuf_costs_cap_dat <- bmp_costs_ripbuf_capital_rev[
    ,names(bmp_costs_ripbuf_capital_rev) != "comid"
  ]
  ripbuf_costs_cap_dat <- ripbuf_costs_cap_dat %>% 
    select(comid_form, everything())
  
  bmp_costs_ripbuf_operations_rev <- bmp_costs_ripbuf_operations[
    order(bmp_costs_ripbuf_operations$comid),
  ]
  ripbuf_costs_op_dat <- bmp_costs_ripbuf_operations_rev[ 
    ,names(bmp_costs_ripbuf_operations_rev) != "comid"
  ]
  ripbuf_costs_op_dat <- ripbuf_costs_op_dat %>% 
    select(comid_form, everything())
}

#
#
## Format point source BMP efficiency data ------
### TN point Source Efficiencies
if (any(point_effic_bycomid_tn$comid %in% 
        unlist(lapply(
          streamcat_subset_tn,
          function(x) x[!is.na(x$comid), ]
        ), use.names = FALSE))) {
  temp_point_effic_dat_tn <- point_effic_bycomid_tn[
    point_effic_bycomid_tn$comid %in%
      unlist(lapply(
        streamcat_subset_tn,
        function(x) x[!is.na(x$comid), ]
      ), use.names = FALSE),
  ] %>%
    select(c("comid", "effic")) 
} else {
  temp_point_effic_dat_tn <- data.frame(
    comid = point_effic_bycomid_tn$comid,
    effic = rep(0, nrow(point_effic_bycomid_tn))
  )
}


temp_point_effic_other_tn <- data.frame(
  comid = unlist(
    streamcat_subset_tn, use.names = FALSE
  )[
    !(
      unlist(streamcat_subset_tn, use.names = FALSE) %in% 
        temp_point_effic_dat_tn$comid
    )
  ]
)

if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
  if(nrow(temp_point_effic_other_tn) > 0) {
    temp_point_effic_other_tn$effic <- 0
  } else {
    temp_point_effic_other_tn$effic <- numeric(0)
  }
  temp_point_effic_dat_rev_tn <- rbind(
    temp_point_effic_dat_tn, temp_point_effic_other_tn
  ) %>% 
    group_by(comid) %>%
    summarize(effic = mean(effic, na.rm = TRUE), .groups = "drop") %>%
    arrange(comid) %>%
    mutate(comid_form = paste0("'", comid, "'"))
  
  point_effic_dat_tn <- temp_point_effic_dat_rev_tn %>% 
    select(c("comid_form", "effic"))
}

### TP point Source Efficiencies
if(any(
  point_effic_bycomid_tp$comid %in% 
  unlist(lapply(
    streamcat_subset_tp,
    function(x) x[!is.na(x$comid), ]
  ), use.names = FALSE)
)) {
  temp_point_effic_dat_tp <- point_effic_bycomid_tp[
    point_effic_bycomid_tp$comid %in% 
      unlist(lapply(
        streamcat_subset_tp,
        function(x) x[!is.na(x$comid), ]
      ), use.names = FALSE),
  ] %>% 
    select(c("comid", "effic"))
} else {
  temp_point_effic_dat_tp <- data.frame(
    comid = point_effic_bycomid_tp$comid,
    effic = rep(0, nrow(point_effic_bycomid_tp))
  )
}

temp_point_effic_other_tp <- data.frame(
  comid = unlist(
    lapply(
      streamcat_subset_tp,
      function(x) x[!is.na(x$comid), ]
    ), use.names = FALSE
  )[
    !(
      unlist(lapply(
        streamcat_subset_tp,
        function(x) x[!is.na(x$comid), ]
      ), use.names = FALSE) %in% 
        temp_point_effic_dat_tp$comid
    )
  ]
)

if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
  if(nrow(temp_point_effic_dat_tp) > 0) {
    
    if(exists("temp_point_effic_other_tp") && nrow(temp_point_effic_other_tp) > 0) {
      temp_point_effic_other_tp$effic <- 0
      
      temp_point_effic_dat_rev_tp <- rbind(
        temp_point_effic_dat_tp, temp_point_effic_other_tp
      ) %>% 
        group_by(comid) %>%
        summarize(effic = mean(effic, na.rm = TRUE), .groups = "drop") %>%
        arrange(comid) %>%
        mutate(comid_form = paste0("'", comid, "'"))
      
      point_effic_dat_tp <- temp_point_effic_dat_rev_tp %>% 
        select(c("comid_form", "effic"))
    } else {
      temp_point_effic_dat_rev_tp <- temp_point_effic_dat_tp %>% 
        group_by(comid) %>%
        summarize(effic = mean(effic, na.rm = TRUE), .groups = "drop") %>%
        arrange(comid) %>%
        mutate(comid_form = paste0("'", comid, "'"))
      
      point_effic_dat_tp <- temp_point_effic_dat_rev_tp %>% 
        select(c("comid_form", "effic"))
    }
    
  } else {
    point_effic_dat_tp <- data.frame(
      comid_form = character(),
      effic = numeric()
    )
  }
}

#
#
## Format agricultural BMP efficiency data ------
if (exists("ag_effic_bycomid_tn")) {
  temp_ag_effic_dat_tn <- ag_effic_bycomid_tn[
    ag_effic_bycomid_tn$comid %in% unlist(lapply(
      streamcat_subset_tn,
      function(x) x[!is.na(x$comid), ]
    ), use.names = FALSE),
  ] %>%
    arrange(comid) %>%
    mutate(comid_form = paste0("'", comid, "'"))
  
  ag_effic_dat_tn <- temp_ag_effic_dat_tn %>% 
    select(-comid) %>% 
    select(comid_form, everything())
}

if (exists("ag_effic_bycomid_tp")) {
  temp_ag_effic_dat_tp <- ag_effic_bycomid_tp[
    ag_effic_bycomid_tp$comid %in% unlist(lapply(
      streamcat_subset_tp,
      function(x) x[!is.na(x$comid), ]
    ), use.names = FALSE),
  ] %>%
    arrange(comid) %>%
    mutate(comid_form = paste0("'", comid, "'"))
  
  ag_effic_dat_tp <- temp_ag_effic_dat_tp %>% 
    select(-comid) %>% 
    select(comid_form, everything())
}

#
#
## Format urban BMP efficiency data -----
if(length(Urban_BMPs) > 0) {
  
  temp_urban_effic_dat_tn <- urban_effic_bycomid_tn[
    urban_effic_bycomid_tn$comid %in% 
      unlist(lapply(
        streamcat_subset_tn,
        function(x) x[!is.na(x$comid), ]
      ), use.names = FALSE),
  ] %>%
    arrange(comid) %>%
    mutate(comid_form = paste0("'", comid, "'"))
  
  urban_effic_dat_tn <- temp_urban_effic_dat_tn %>% 
    select(-comid) %>% 
    select(comid_form, everything())
  
  temp_urban_effic_dat_tp <- urban_effic_bycomid_tp[
    urban_effic_bycomid_tp$comid %in% 
      unlist(lapply(
        streamcat_subset_tp,
        function(x) x[!is.na(x$comid), ]
      ), use.names = FALSE),
  ] %>%
    arrange(comid) %>%
    mutate(comid_form = paste0("'", comid, "'"))
  
  urban_effic_dat_tp <- temp_urban_effic_dat_tp %>% 
    select(-comid) %>% 
    select(comid_form, everything())
}

#
#
## Format septic BMP efficiency data -----
if(length(Septic_BMPs) > 0) {
  
  septic_effic_dat_temp <- septic.upgrade %>% select(comid, SepticUpgradeClass, efficiency_per_parcel)%>%
    mutate(
      SepticUpgradeClass = case_when(
        SepticUpgradeClass == 1 ~ "class_1_upgrade",
        SepticUpgradeClass == 2 ~ "class_2_upgrade",
        SepticUpgradeClass == 3 ~ "class_3_upgrade",
        SepticUpgradeClass == 4 ~ "class_4_upgrade",
        SepticUpgradeClass == 5 ~ "class_5_upgrade",
        SepticUpgradeClass == 6 ~ "class_6_upgrade",
        SepticUpgradeClass == 7 ~ "class_7_upgrade",
        SepticUpgradeClass == 8 ~ "class_8_upgrade"
      )
    )
  
  if(("Sewer_convert" %in% Septic_BMPs) == TRUE){
    
    # Create efficiency for sewer conversions
    sewer_effic_dat_temp <- septic.conversion%>%select(comid, efficiency_per_parcel)%>%
      mutate(
        SepticUpgradeClass = "Sewer_convert"
      )
    
    # Combine septic and sewer efficiency data
    septic_effic_dat_tn <-  rbind(septic_effic_dat_temp, sewer_effic_dat_temp)%>%
      pivot_wider(
        names_from = SepticUpgradeClass,
        values_from = efficiency_per_parcel
      ) %>%
      right_join(
        data.frame(comid = as.character(unique(unlist(streamcat_subset_tn, use.names = FALSE)))),
        by = "comid"
      ) %>%
      mutate(comid_form = paste0("'", comid, "'"))
  } else {
    
    # Combine septic and sewer efficiency data
    septic_effic_dat_tn <-  septic_effic_dat_temp%>%
      pivot_wider(
        names_from = SepticUpgradeClass,
        values_from = efficiency_per_parcel
      ) %>%
      right_join(
        data.frame(comid = as.character(unique(unlist(streamcat_subset_tn, use.names = FALSE)))),
        by = "comid"
      ) %>%
      mutate(comid_form = paste0("'", comid, "'"))
    
  }
  
  # replace NAs with  0
  septic_effic_dat_tn[is.na(septic_effic_dat_tn)] <- 0
}

#
#
## Format point source BMP costs data -----
temp_point_costs_dat <- bmp_costs_point[#@HP: duplicate point costs tracked to the temp_point_costs_dat data frame
  bmp_costs_point$comid %in% streamcat_subset_all$comid,
] %>% 
  select(c("comid", "State", "capital", "operations"))
temp_point_costs_other <- as.data.frame(
  streamcat_subset_all[
    !(streamcat_subset_all$comid %in% temp_point_costs_dat$comid),
  ]
)
names(temp_point_costs_other) <- "comid"
temp_point_costs_other_rev <- merge(
  temp_point_costs_other, COMID_State, by = c("comid"), all.x = TRUE
)
temp_point_costs_other_rev$capital <- 0
temp_point_costs_other_rev$operations <- 0
names(temp_point_costs_other_rev) <- c(
  "comid", "State", "capital", "operations"
)
temp_point_costs_dat_rev <- rbind(
  temp_point_costs_dat, temp_point_costs_other_rev
) %>%
  group_by(comid, State) %>%
  summarize(
    capital = sum(capital, na.rm = TRUE),
    operations = sum(operations, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(comid)
temp_point_costs_dat_rev$comid_form <- paste0(
  "'", temp_point_costs_dat_rev$comid, "'"
)
point_costs_dat <- temp_point_costs_dat_rev %>% 
  select(c("comid_form", "capital", "operations"))

#
#
## Format user defined limits on BMP implementation ----
temp_bmp_imp <- user_specs_BMPs %>% 
  select(BMP_Category, BMP, BMP_Selection, frac_min, frac_max) %>%
  filter(BMP_Selection == "X") 

urban_bmp_imp <- temp_bmp_imp %>% 
  filter(BMP_Category == "urban") %>% 
  select(BMP, frac_min, frac_max)

ag_bmp_imp <- temp_bmp_imp %>% 
  filter(BMP_Category == "ag") %>% 
  select(BMP, frac_min, frac_max)

ripbuf_bmp_imp <- temp_bmp_imp %>% 
  filter(BMP_Category == "ripbuf") %>% 
  select(BMP, frac_min, frac_max)

#
#
## Format user defined design depths for urban BMPs ----
bmp_urban_ratingdepths <- bmp_urban %>% 
  mutate(
    ratingdepth = case_when(
      BMP != "Porous_Pavement_w_underdrain" ~ as.numeric(as.character(UserSpec_RD_in)),
      BMP == "Porous_Pavement_w_underdrain" ~ 4 * 0.2 + # Porous asphalt
        4 * 0.4 + # Chocker course
        UserSpec_RD_in * 0.25 + # filter course
        8 * 0.4 # gravel layer
    )
  ) %>% mutate(
    ratingdepth_NY = case_when(
      BMP != "Porous_Pavement_w_underdrain" ~ as.numeric(as.character(UserSpec_RD_in_NY)),
      BMP == "Porous_Pavement_w_underdrain" ~ 4 * 0.2 +
        4 * 0.4 +
        UserSpec_RD_in_NY * 0.25 +
        8 * 0.4
    )
  ) %>%
  select(BMP, ratingdepth, ratingdepth_NY)

#
#
## Set varying rating depth by state -----
#*# SE: updated 5-8-24
# Create raw file
depth_by_state <- COMID_State

# Create temporary depth matrix for column names
temp_depth <- setNames(data.frame(matrix(NA, nrow = nrow(depth_by_state), ncol = nrow(bmp_urban_ratingdepths) + 1)), 
                       c('comid', bmp_urban_ratingdepths$BMP))

# Merge with COMID_state to get COMIDs and states
depth_by_state <- COMID_State %>%
  left_join(temp_depth, by = "comid")  %>%
  # Loop over each column in depth_by_state (excluding the first two columns)
  mutate(across(-c(comid, State), 
                ~ ifelse(State == 'NY', 
                         bmp_urban_ratingdepths$ratingdepth_NY[bmp_urban_ratingdepths$BMP == cur_column()], 
                         bmp_urban_ratingdepths$ratingdepth[bmp_urban_ratingdepths$BMP == cur_column()]), 
                .names = "{.col}")) %>%
  # Create a new column 'comid_form'
  mutate(comid_form = paste0("'", comid, "'")) %>%
  select(-comid, -State) %>%
  # rename column headers to match BMP vec
  rename_with(
    .fn = ~ paste0("'", ., "'"), # Add apostrophes to each column name
    .cols = -comid_form # Exclude the 'comid_form' column from renaming
  )

# Make matrix of implementability of each urban BMP for each comid's impervious land
# HSG D comids cannot have infiltration-based BMPs
# Porous Pavement can only be implemented on 10% of impervious land.

urban_bmp_implementationpotential_dat <- setNames(
  data.frame(
    matrix(
      ncol = length(Urban_BMPs) + 1, nrow = length(streamcat_subset_all$comid)
    )
  ), 
  c("comid", Urban_BMPs)
) %>%
  mutate(
    comid = streamcat_subset_all$comid,
    comid_form = paste0("'", comid, "'")
  ) %>%
  left_join(., comid.infiltrationrates.matched, by = "comid") %>%
  mutate(
    across(
      .cols = all_of(Urban_BMPs), 
      .fns = ~case_when(
        cur_column() %in% c(
          "Biofiltration_w_Underdrain", 
          "Bioretention_Basin", 
          "Enhanced_Biofiltration_w_ISR", 
          "Extended_Dry_Detention_Basin", 
          "Grass_Swale_w_detention", 
          "Gravel_Wetland", 
          "Sand_Filter_w_underdrain", 
          "Wet_Pond"
        ) ~ 
          1, 
        cur_column() == "Porous_Pavement_w_underdrain" ~ 0.1,
        cur_column() %in% c(
          "Infiltration_Basin", "Infiltration_Chamber", "Infiltration_Trench"
        ) ~ case_when(
          InfiltrationRate_inperhr <= 0.12 ~ 0, 
          InfiltrationRate_inperhr >= 0.12 ~ 1,
          is.na(InfiltrationRate_inperhr) ~ 0
        ),
        cur_column() == "Porous_Pavement_w_subsurface_infiltration" ~ case_when(
          InfiltrationRate_inperhr <= 0.12 ~ 0,
          InfiltrationRate_inperhr >= 0.12 ~ 0.1,
          is.na(InfiltrationRate_inperhr) ~ 0
        )
      )
    )
  ) %>%
  select(comid_form, all_of(Urban_BMPs))

#
#
## Format Urban Cost Coefficient Data & make implementation matrix -----
urban_cost_coeffs_dat <- urban_cost_coeffs %>%
  filter(comid %in% unlist(streamcat_subset_all)) %>%
  group_by(comid) %>%
  summarise(urban_cost_coef = mean(urban_cost_coef), .groups = "drop") %>%
  arrange('comid') %>%
  mutate(comid_form = paste0("'", comid, "'")) %>%
  select(comid_form, urban_cost_coef) 

### Make matrix of implementability (max parcels for each COMID for each class of septic upgrades) for septic BMPs

if(length(Septic_BMPs) > 0){
  
  # Define implementation potential for septic upgrades
  septic_implementationpotential_upgrade <- septic.upgrade%>%
    select(comid:parcels)%>%
    pivot_wider(
      names_from = SepticUpgradeClass,
      values_from = parcels
    ) %>%
    right_join(
      data.frame(comid = as.character(unique(unlist(streamcat_subset_tn, use.names = FALSE)))),
      by = "comid"
    )
  
  # rename cols to match BMPs
  colnames(septic_implementationpotential_upgrade)[2:9] <- 
    paste0("class_", colnames(septic_implementationpotential_upgrade)[2:9], "_upgrade")
  
  # replace NAs with 0s
  septic_implementationpotential_upgrade[is.na(septic_implementationpotential_upgrade)] <- 0
  
  # if sewer conversion is selected, add to implementation potential
  if(("Sewer_convert" %in% Septic_BMPs) == TRUE){
    
    # conversion implementation
    septic_implementationpotential_convert <- septic.conversion %>%
      # fix comid type to allow match
      mutate(comid = as.character(comid)) %>%
      right_join(
        data.frame(comid = unique(unlist(streamcat_subset_tn, use.names = FALSE))),
        by = "comid"
      ) %>%
      select(comid, Sewer_convert = parcels)
    
    # merge with septic implementation potential
    septic_bmp_implementationpotential <- merge(
      septic_implementationpotential_upgrade, 
      septic_implementationpotential_convert, 
      by = "comid", all = TRUE
    ) %>%
      mutate(comid_form = paste0("'", comid, "'"))
    
    # replace NA with 0
    septic_bmp_implementationpotential$Sewer_convert[is.na(septic_bmp_implementationpotential$Sewer_convert)] <-0
    
    # merge with totals
    septic_bmp_implementationpotential <- merge(
      septic_bmp_implementationpotential, 
      total_septic_totals %>% 
        select(comid, final_total),
      by = "comid", all.x = TRUE
    )%>%
      rename(total_parcels = final_total)
    
    # replace NAs with super high value so that the load/total = 0
    septic_bmp_implementationpotential <- septic_bmp_implementationpotential%>%
      mutate(
        total_parcels = case_when(
          is.na(total_parcels) ~ 1e25,
          total_parcels == 0 ~ 1e25,
          TRUE ~ total_parcels
        )
      )
    
  } else {
    
    # Sewer conversions not found, move forwar with just upgrades
    septic_bmp_implementationpotential <- septic_implementationpotential_upgrade %>%
      mutate(comid_form = paste0("'", comid, "'"))
    
    # merge with totals
    septic_bmp_implementationpotential <- merge(
      septic_bmp_implementationpotential, 
      septic.upgrade_totals %>% 
        select(comid, total_parcels),
      by = "comid", all.x = TRUE
    )%>%
      rename(total_parcels = total_parcels)
    
    # replace NAs with large value so that the load/total = 0
    septic_bmp_implementationpotential <- septic_bmp_implementationpotential%>%
      mutate(
        total_parcels = case_when(
          is.na(total_parcels) ~ 1e25,
          total_parcels == 0 ~ 1e25,
          TRUE ~ total_parcels
        )
      )
    # Would be able to replace this with 0 here too
  }
  
}

#
#
## Write AMPL data file to file -----

write( "# Tier 1 Data AMPL File", file = paste(OutPath,"STdata_seasonal.dat",sep = ""))

comid_vec_all <- paste0("'", streamcat_subset_all$comid,"'")
comid_vec_all[-length(comid_vec_all)] <- paste0(
  comid_vec_all[-length(comid_vec_all)], ','
)

comid_vec_all_N <- paste0(
  "'", unique(unlist(streamcat_subset_tn, use.names = FALSE)), "'"
)
comid_vec_all_N[-length(comid_vec_all_N)] <- paste0(
  comid_vec_all_N[-length(comid_vec_all_N)], ','
)

comid_vec_all_P <- paste0(
  "'", unique(unlist(streamcat_subset_tp, use.names = FALSE)), "'"
)
comid_vec_all_P[-length(comid_vec_all_P)] <- paste0(
  comid_vec_all_P[-length(comid_vec_all_P)], ','
)

comid_vec_N <- foreach(i = 1:length(streamcat_subset_tn)) %do% {
  
  comid_vec_N_tmp <- paste0("'", streamcat_subset_tn[[i]]$comid, "'")
  comid_vec_N_tmp[-length(comid_vec_N_tmp)] <- paste0(
    comid_vec_N_tmp[-length(comid_vec_N_tmp)], ','
  )
  comid_vec_N_tmp
}
comid_vec_P <- foreach(i = 1:length(streamcat_subset_tp)) %do% {
  
  comid_vec_P_tmp <- paste0("'", streamcat_subset_tp[[i]]$comid, "'")
  comid_vec_P_tmp[-length(comid_vec_P_tmp)] <- paste0(
    comid_vec_P_tmp[-length(comid_vec_P_tmp)], ','
  )
  comid_vec_P_tmp
}

### TN Loads by watershed ---- 
invisible(
  if ("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    foreach(i = 1:length(inc_tn_dat)) %do% {
      
      if (!is.null(inc_tn_dat[[i]]) && nrow(inc_tn_dat[[i]]) > 0) {
        temp_tn_dat <- inc_tn_dat[[i]]
        
        # Write the header for the table
        write(
          paste0("\nparam baseloads_N", i, " : 'point' 'urban' 'ag' 'septic' 'storage' 'other' :="), #*#,
          file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
          append = TRUE
        )
        
        # Prepare the data in the required format
        formatted_data <- temp_tn_dat %>%
          mutate(year = as.numeric(year)) %>%
          select(
            comid_form,
            season,
            year,
            point = in_poin,
            urban = in_urb,
            ag = in_ag,
            septic = in_septic,
            storage = in_storage,
            other = in_other
          ) %>%
          arrange(year, comid_form, season)
        
        # Write the formatted data to the file
        write.table(
          formatted_data,
          file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
          append = TRUE,
          sep = "\t",
          row.names = FALSE,
          col.names = FALSE,
          na = "",
          quote = FALSE
        )
        
        # Close the table with a semicolon
        write(";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = TRUE)
      }
    }
  }
)

### TP Loads by watershed ----
invisible(
  if ("TP" %in% user_specs_loadingtargets$TN_or_TP) {
    foreach(i = 1:length(inc_tp_dat)) %do% {
      if (!is.null(inc_tp_dat[[i]]) && nrow(inc_tp_dat[[i]]) > 0) {
        
        temp_tp_dat <- inc_tp_dat[[i]]
        
        # Write the header for the table
        write(
          paste0("\nparam baseloads_P", i, " : 'point' 'urban' 'ag' 'storage' 'other' :="), #*#
          file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
          append = TRUE
        )
        
        # Prepare the data in the required format
        formatted_data <- temp_tp_dat %>%
          mutate(year = as.numeric(as.character(year))) %>%
          select(
            comid_form,
            season,
            year,
            point = ip_poin,
            urban = ip_urb,
            ag = ip_ag,
            storage = ip_storage,
            other = ip_other
          ) %>%
          arrange(year, comid_form, season)
        
        # Write the formatted data to the file
        write.table(
          formatted_data,
          file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
          append = TRUE,
          sep = "\t",
          row.names = FALSE,
          col.names = FALSE,
          na = "",
          quote = FALSE
        )
        
        # Close the table with a semicolon
        write(";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = TRUE)
      }
    }
  }
)

### TN Storage Coefficients ------
invisible(
  if ("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    foreach(i = 1:length(sm_N_dat)) %do% {
      
      temp_sm_N_dat <- sm_N_dat[[i]]
      
      if (!is.null(temp_sm_N_dat) && nrow(temp_sm_N_dat) > 0) {
        
        # Write the header for the storage coefficients
        write(
          paste0("\nparam transfer_coefficients_N", i, " : 'coeff' :="),
          file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
          append = TRUE
        )
        
        # Replace NA, Inf, and NaN values with 0 before writing
        temp_sm_N_dat <- temp_sm_N_dat %>%
          mutate(
            year = as.numeric(year),
            coeff = ifelse(is.na(coeff) | is.infinite(coeff) | is.nan(coeff), 0, coeff)
          )
        
        # Write the data
        write.table(
          temp_sm_N_dat,
          file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
          append = TRUE,
          sep = "\t",
          row.names = FALSE,
          col.names = FALSE,
          na = "0",  # Changed from "" to "0" as additional safety
          quote = FALSE
        )
        
        # Close the table with a semicolon
        write(";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = TRUE)
      } else {
        # If the data is NULL or empty, write a message
        write(
          paste0("\nparam transfer_coefficients_N", i, " : 'coeff' :=\n;"),
          file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
          append = TRUE
        )
      }
    }
  }
)

### TP Storage Coefficients -----
invisible(
  if ("TP" %in% user_specs_loadingtargets$TN_or_TP) {
    foreach(i = 1:length(sm_P_dat)) %do% {
      
      temp_sm_P_dat <- sm_P_dat[[i]]
      
      if (!is.null(temp_sm_P_dat) && nrow(temp_sm_P_dat) > 0) {
        
        # Write the header for the storage coefficients
        write(
          paste0("\nparam transfer_coefficients_P", i, " : 'coeff' :="),
          file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
          append = TRUE
        )
        
        # Replace NA, Inf, and NaN values with 0 before writing
        temp_sm_P_dat <- temp_sm_P_dat %>%
          mutate(
            year = as.numeric(year),
            coeff = ifelse(is.na(coeff) | is.infinite(coeff) | is.nan(coeff), 0, coeff)
          )
        
        # Write the data
        write.table(
          temp_sm_P_dat,
          file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
          append = TRUE,
          sep = "\t",
          row.names = FALSE,
          col.names = FALSE,
          na = "0",  # Changed from "" to "0" as additional safety
          quote = FALSE
        )
        
        # Close the table with a semicolon
        write(";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = TRUE)
      } else {
        # If the data is NULL or empty, write a message
        write(
          paste0("\nparam transfer_coefficients_P", i, " : 'coeff' :=\n;"),
          file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
          append = TRUE
        )
      }
    }
  }
)

### TN Riparian Data ------
invisible(
  if ("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    foreach(i = 1:length(riparian_tn_dat)) %do% {
      if (!is.null(riparian_tn_dat[[i]]) && nrow(riparian_tn_dat[[i]]) > 0) {
        
        temp_ripar <- riparian_tn_dat[[i]]
        
        # Write the header for the table
        write(
          paste0("\nparam riparianload_N", i, " : 'riparian' :="), #*#,
          file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
          append = TRUE
        )
        
        # Prepare the data in the required format
        formatted_data <- temp_ripar %>%
          mutate(year = as.numeric(year)) %>%
          select(
            comid_form,
            season,
            year,
            riparian = in_riparian
          ) %>%
          arrange(year, comid_form, season) %>%
          # replace NA values with 0
          mutate(riparian = ifelse(is.na(riparian), 0, riparian))
        
        if(length(RiparianBuffer_BMPs) > 0) {
          # Write the formatted data to the file
          write.table(
            formatted_data,
            file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
            append = TRUE,
            sep = "\t",
            row.names = FALSE,
            col.names = FALSE,
            na = "",
            quote = FALSE
          )
        } else {
          write.table(
            ripbuf_bmp_dummy_tn[[i]] %>%
              select(comid, none = none), # renaming also forces the order incase they are disordered in processing code above
            file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
            append = T,
            sep = "\t",
            row.names = F,
            col.names = F,
            na = "",
            quote = F
          )
        }
        
        # Close the table with a semicolon
        write(";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = TRUE)
      }
    }
  }
)

### TP Riparian Data -----
invisible(
  if ("TP" %in% user_specs_loadingtargets$TN_or_TP) {
    foreach(i = 1:length(riparian_tp_dat)) %do% {
      
      if (!is.null(riparian_tp_dat[[i]]) && nrow(riparian_tp_dat[[i]]) > 0) {
        
        temp_ripar <- riparian_tp_dat[[i]]
        
        # Write the header for the table
        write(
          paste0("\nparam riparianload_P", i, " : 'riparian' :="), #*#,
          file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
          append = TRUE
        )
        
        # Prepare the data in the required format
        formatted_data <- temp_ripar %>%
          mutate(year = as.numeric(year)) %>%
          select(
            comid_form,
            season,
            year,
            riparian = ip_riparian
          ) %>%
          arrange(year, comid_form, season) %>%
          # replace NA values with 0
          mutate(riparian = ifelse(is.na(riparian), 0, riparian))
        
        if(length(RiparianBuffer_BMPs) > 0) {
          # Write the formatted data to the file
          write.table(
            formatted_data,
            file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
            append = TRUE,
            sep = "\t",
            row.names = FALSE,
            col.names = FALSE,
            na = "",
            quote = FALSE
          )
        } else {
          write.table(
            ripbuf_bmp_dummy_tp[[i]] %>%
              select(comid, none = none), # renaming also forces the order incase they are disordered in preprocessing code above
            file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
            append = T,
            sep = "\t",
            row.names = F,
            col.names = F,
            na = "",
            quote = F
          )
        }
        
        # Close the table with a semicolon
        write(";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = TRUE)
      }
    }
  }
)

### Urban BMP Cost Coefficients ------
write( 
  "\nparam area : 'urban' 'ag' :=", 
  file = paste(OutPath, "STdata_seasonal.dat", sep = ""), 
  append = T
)
write.table(
  area_dat %>% select(comid_form, urban = urban_ac, ag = ag_ac), # renaming also forces the order incase they are disordered in preprocessing code above
  file = paste(OutPath, "STdata_seasonal.dat" ,sep = "") , 
  append = T,
  sep = "\t",
  row.names = F,
  col.names = F,
  na = "",
  quote = F
)
write( ";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = T)

if(length(Urban_BMPs) > 0) {
  write(
    paste0(
      "\nparam urban_bmp_implementationpotential : ", 
      paste(bmp_urban_vec, collapse = "  "), 
      ":="
    ),
    file = paste0(OutPath, "STdata_seasonal.dat"),
    append = TRUE
  )
  write.table(
    urban_bmp_implementationpotential_dat %>% 
      select(comid_form, any_of(Urban_BMPs)), # selection forces the order incase they are disordered in preprocessing code above
    file = paste(OutPath, "STdata_seasonal.dat" ,sep = "") , 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na = "",
    quote = F
  )
} else {
  write(
    paste0("\nparam urban_bmp_implementationpotential: 'none' :="),
    file = paste0(OutPath, "STdata_seasonal.dat"),
    append = TRUE
  )
  write.table(
    urban_bmp_dummy %>% 
      select(comid, none), # selection forces the order incase they are disordered in preprocessing code above
    file = paste(OutPath, "STdata_seasonal.dat" ,sep = "") , 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na = "",
    quote = F
  )
}
write( ";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = T)

if(length(Septic_BMPs) > 0 & "TN" %in% user_specs_loadingtargets$TN_or_TP) {
  write(
    paste0(
      "\nparam septic_bmp_implementationpotential : ", 
      paste(bmp_septic_vec, collapse = "  "), 
      ":="
    ),
    file = paste0(OutPath, "STdata_seasonal.dat"),
    append = TRUE
  )
  write.table(
    septic_bmp_implementationpotential%>% 
      select(comid_form, any_of(Septic_BMPs)), # selection forces the order incase they are disordered in preprocessing code above
    file = paste(OutPath, "STdata_seasonal.dat" ,sep = "") , 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na = "",
    quote = F
  )
  write( ";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = T)
  write( 
    "\nparam septic_bmp_implementationtotal : 'total_parcels' :=", # new version taken from working component; still not working. receiving same NEOS error
    file = paste(OutPath, "STdata_seasonal.dat", sep = ""), 
    append = T
  )
  write.table(
    septic_bmp_implementationpotential%>% 
      select(comid_form, total_parcels), # selection forces the order in case they are disordered in preprocessing code above
    file = paste(OutPath, "STdata_seasonal.dat", sep = "") , 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na = "",
    quote = F
  )
  write( ";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = T)
} else {
  # Write dummy septic parameters if TN exists globally (mirrors TP behavior)
  if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    write(
      paste0("\nparam septic_bmp_implementationpotential: 'none' :="),
      file = paste0(OutPath, "STdata_seasonal.dat"),
      append = TRUE
    )
    write.table(
      septic_bmp_dummy %>% 
        select(comid_form, none), # selection forces the order incase they are disordered in preprocessing code above
      file = paste(OutPath, "STdata_seasonal.dat" ,sep = "") , 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
    write( ";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = T)
  }
}

if(length(RiparianBuffer_BMPs) > 0) {
  write( 
    paste0(
      "\nparam unbuffered_banklength : ",  
      paste(bmp_ripbuf_vec, collapse  = "  "), 
      " :="
    ), 
    file = paste(OutPath,"STdata_seasonal.dat",sep = ""), 
    append = T
  )
  write.table(
    streambanklength_available_dat %>% 
      select(comid_form, any_of(RiparianBuffer_BMPs)), # selection forces the order incase they are disordered in preprocessing code above
    file = paste(OutPath, "STdata_seasonal.dat" ,sep = "") , 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na = "",
    quote = F
  )
} else {
  write( 
    paste0("\nparam unbuffered_banklength: 'none' :="), 
    file = paste(OutPath,"STdata_seasonal.dat",sep = ""), 
    append = T
  )
  write.table(
    ripbuf_bmp_dummy %>% 
      select(comid, none), # selection forces the order incase they are disordered in preprocessing code above
    file = paste(OutPath, "STdata_seasonal.dat" ,sep = "") , 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na = "",
    quote = F
  )
}
write( ";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = T)

write( 
  paste0("\nparam total_banklength :="), 
  file = paste(OutPath,"STdata_seasonal.dat",sep = ""), 
  append = T
)
if(length(RiparianBuffer_BMPs) > 0) {
  write.table(
    streambanklength_total_dat %>% 
      select(comid_form, totalbanklength_ft), # selection forces the order incase they are disordered in preprocessing code above
    file = paste(OutPath, "STdata_seasonal.dat" ,sep = "") , 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na = "",
    quote = F
  )
} else {
  write.table(
    ripbuf_bmp_dummy %>% 
      select(comid, none), # selection forces the order incase they are disordered in preprocessing code above
    file = paste(OutPath, "STdata_seasonal.dat" ,sep = "") , 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na = "",
    quote = F
  )
}
write( ";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = T)

if(length(Ag_BMPs) > 0) {
  if("TN" %in% user_specs_loadingtargets$TN_or_TP & exists("ag_effic_dat_tn")) {
    cat(
      "\nparam ag_effic_N : ",bmp_ag_vec," :=	", 
      file = paste(OutPath,"STdata_seasonal.dat", sep = ""), 
      sep="", 
      append = T
    )
    cat(
      "\n", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), sep = "", append = T
    )
    write.table(
      ag_effic_dat_tn %>% select(comid_form, all_of(bmp_ag_vec_direct)), # selecting forces the order incase they are disordered in preprocessing code above
      file = paste(OutPath, "STdata_seasonal.dat", sep = "") , 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
    write( ";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = T)
  }
  
  if("TP" %in% user_specs_loadingtargets$TN_or_TP  & exists("ag_effic_dat_tp")) {
    cat(
      "\nparam ag_effic_P : ",bmp_ag_vec," :=	", 
      file = paste(OutPath,"STdata_seasonal.dat", sep = ""), 
      sep="", 
      append = T
    )
    cat(
      "\n", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), sep = "", append = T
    )
    write.table(
      ag_effic_dat_tp %>% select(comid_form, all_of(bmp_ag_vec_direct)),  # selecting forces the order incase they are disordered in preprocessing code above
      file = paste(OutPath, "STdata_seasonal.dat", sep = "") , 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
    write( ";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = T)
  }
} else {
  if("TN" %in% user_specs_loadingtargets$TN_or_TP & !exists("ag_effic_dat_tn")) {
    cat(
      "\nparam ag_effic_N : 'none' :=	", 
      file = paste(OutPath,"STdata_seasonal.dat", sep = ""), 
      sep="", 
      append = T
    )
    cat(
      "\n", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), sep = "", append = T
    )
    write.table(
      unique(do.call("rbind", ag_bmp_dummy_tn)) %>% select(comid, none), # selecting forces the order incase they are disordered in preprocessing code above
      file = paste(OutPath, "STdata_seasonal.dat", sep = "") , 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
    write( ";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = T)
  }
  
  if("TP" %in% user_specs_loadingtargets$TN_or_TP & !exists("ag_effic_dat_tp")) {
    cat(
      "\nparam ag_effic_P : 'none' :=	", 
      file = paste(OutPath,"STdata_seasonal.dat", sep = ""), 
      sep="", 
      append = T
    )
    cat(
      "\n", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), sep = "", append = T
    )
    write.table(
      unique(do.call("rbind", ag_bmp_dummy_tp)) %>% select(comid, none), # selecting forces the order incase they are disordered in preprocessing code above
      file = paste(OutPath, "STdata_seasonal.dat", sep = "") , 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
    write( ";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = T)
  }
}

if(length(Septic_BMPs) > 0) {
  if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    cat(
      paste0("\nparam septic_effic_N : ",paste(bmp_septic_vec, collapse = " ")," :=	"), 
      file = paste(OutPath,"STdata_seasonal.dat", sep = ""), 
      sep="", 
      append = T
    )
    cat(
      "\n", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), sep = "", append = T
    )
    write.table(
      septic_effic_dat_tn %>%
        select(comid_form, all_of(bmp_septic_vec_direct)),
      file = paste(OutPath, "STdata_seasonal.dat", sep = "") , 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
    write( ";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = T)
  }
  
} else {
  if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    cat(
      "\nparam septic_effic_N : 'none' :=	", 
      file = paste(OutPath,"STdata_seasonal.dat", sep = ""), 
      sep="", 
      append = T
    )
    cat(
      "\n", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), sep = "", append = T
    )
    write.table(
      unique(do.call("rbind", septic_bmp_dummy_tn)) %>% select(comid_form, none), # selecting forces the order incase they are disordered in preprocessing code above
      file = paste(OutPath, "STdata_seasonal.dat", sep = "") , 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
    write( ";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = T)
  }
  
}

# Check if WWTPs exist in current watershed
has_wwtp_tn <- exists("point_effic_dat_tn") && 
  nrow(point_effic_dat_tn %>% filter(comid_form %in% paste0("'", streamcat_subset_tn$comid, "'"))) > 0

has_wwtp_tp <- exists("point_effic_dat_tp") && 
  nrow(point_effic_dat_tp %>% filter(comid_form %in% paste0("'", streamcat_subset_tp$comid, "'"))) > 0

if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
  if(has_wwtp_tn) {
    write( 
      "\nparam point_effic_N : 'point' :=	", 
      file = paste(OutPath, "STdata_seasonal.dat", sep = ""), 
      append = T
    )
    write.table(
      unique(point_effic_dat_tn %>% 
        filter(comid_form %in% paste0("'", streamcat_subset_tn$comid, "'")) %>%  # Filter to only current watershed COMIDs
        select(comid_form, point = effic)), # renaming also forces the order incase they are disordered in preprocessing code above
      file = paste(OutPath, "STdata_seasonal.dat", sep = "") , 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
    write( ";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = T)
  } else {
    # No WWTPs in watershed - write dummy parameter with 'point' column header and zero efficiency
    cat(
      "\nparam point_effic_N : 'point' :=	", 
      file = paste(OutPath, "STdata_seasonal.dat", sep = ""), 
      sep = "", 
      append = T
    )
    cat(
      "\n", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), sep = "", append = T
    )
    write.table(
      unique(do.call("rbind", lapply(streamcat_subset_tn, function(x) data.frame(comid = x$comid)))) %>%
        mutate(comid_form = paste0("'", comid, "'"), point = 0) %>%
        select(comid_form, point),
      file = paste(OutPath, "STdata_seasonal.dat", sep = "") , 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
    write( ";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = T)
  }
}

if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
  if(has_wwtp_tp) {
    write( 
      "\nparam point_effic_P : 'point' :=	", 
      file = paste(OutPath, "STdata_seasonal.dat", sep = ""), 
      append = T
    )
    write.table(
      unique(point_effic_dat_tp %>% 
        filter(comid_form %in% paste0("'", streamcat_subset_tp$comid, "'")) %>%  # Filter to only current watershed COMIDs
        select(comid_form, point = effic)), # renaming also forces the order incase they are disordered in preprocessing code above
      file = paste(OutPath, "STdata_seasonal.dat", sep = "") , 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
    write( ";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = T)
  } else {
    # No WWTPs in watershed - write dummy parameter with 'point' column header and zero efficiency
    cat(
      "\nparam point_effic_P : 'point' :=	", 
      file = paste(OutPath, "STdata_seasonal.dat", sep = ""), 
      sep = "", 
      append = T
    )
    cat(
      "\n", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), sep = "", append = T
    )
    write.table(
      unique(do.call("rbind", lapply(streamcat_subset_tp, function(x) data.frame(comid = x$comid)))) %>%
        mutate(comid_form = paste0("'", comid, "'"), point = 0) %>%
        select(comid_form, point),
      file = paste(OutPath, "STdata_seasonal.dat", sep = "") , 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
    write( ";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = T)
  }
}

if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
  if(length(Urban_BMPs) > 0) {
    cat(
      paste0("\nparam urban_effic_N : ", paste(bmp_urban_vec, collapse = " "), " :="), 
      file = paste(OutPath,"STdata_seasonal.dat", sep = ""), 
      sep="", 
      append = T
    )
    cat(
      "\n", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), sep = "", append = T
    )
    write.table(
      unique(
        urban_effic_dat_tn %>% select(comid_form, all_of(bmp_urban_vec_direct))
      ), # selecting forces the order incase they are disordered in preprocessing code above
      file = paste(OutPath, "STdata_seasonal.dat", sep = "") , 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
  } else {
    cat(
      "\nparam urban_effic_N : 'none' :=", 
      file = paste0(OutPath, "STdata_seasonal.dat"), 
      sep = "", 
      append = T
    )
    cat(
      "\n", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), sep = "", append = T
    )
    write.table(
      unique(do.call("rbind", urban_bmp_dummy_tn) %>% select(comid, none)), # selecting forces the order incase they are disordered in preprocessing code above
      file = paste(OutPath, "STdata_seasonal.dat", sep = "") , 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
  }
  write( ";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = T)
}

if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
  if (length(Urban_BMPs) > 0) {
    if (nrow(urban_effic_dat_tp) > 1) {
      cat(
        paste0("\nparam urban_effic_P : ", paste(bmp_urban_vec, collapse = " "), " :="),
        file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
        sep = "",
        append = T
      )
      cat(
        "\n",
        file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
        sep = "",
        append = T
      )
      write.table(
        unique(urban_effic_dat_tp %>% select(
          comid_form, all_of(bmp_urban_vec_direct)
        )),
        # selecting forces the order incase they are disordered in preprocessing code above
        file = paste(OutPath, "STdata_seasonal.dat", sep = "") ,
        append = T,
        sep = "\t",
        row.names = F,
        col.names = F,
        na = "",
        quote = F
      )
    } else {
      cat(
        paste0("\nparam urban_effic_P : ", paste(bmp_urban_vec, collapse = " "), " :="),
        file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
        sep = "",
        append = T
      )
    }
    write(
      ";",
      file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
      append = T
    )
  }
}

### TN Riparian Removals -----
invisible(if ("TN" %in% user_specs_loadingtargets$TN_or_TP) {
  foreach(i = 1:length(param_loads_lim_tn)) %do% {
    
    temp_ripar_rem <- riparian_tn_removal[[i]]
    
    if (!is.null(temp_ripar_rem) && nrow(temp_ripar_rem) > 0) {
      write(
        paste0(
          "\nparam riparianremoval_N",
          i,
          " : 'Grassed_Buffer' 'Forested_Buffer':="
        ),
        file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
        append = T
      )
      
      # Prepare the data in the required format
      formatted_data <- temp_ripar_rem %>%
        mutate(year = as.numeric(year)) %>%
        select(
          comid_form,
          season,
          year,
          `'Grassed_Buffer'` = Grassed_Buffer,
          `'Forested_Buffer'` = Forested_Buffer
        ) %>%
        arrange(year, comid_form, season)
      
      # Write the formatted data to the file
      write.table(
        formatted_data,
        file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
        append = T,
        sep = "\t",
        row.names = FALSE,
        col.names = FALSE,
        na = "",
        quote = FALSE
      )
    } else {
      write(
        paste0("\nparam riparianremoval_N", i, " : 'none' :="),
        file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
        append = T
      )
      write.table(
        ripbuf_bmp_dummy_tn[[i]] %>%
          select(comid, none),
        # renaming also forces the order incase they are disordered in preprocessing code above
        file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
        append = T,
        sep = "\t",
        row.names = F,
        col.names = F,
        na = "",
        quote = F
      )
    }
    write(
      ";",
      file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
      append = T
    )
  }
})

### TP Riparian Removals -----
invisible(
  if ("TP" %in% user_specs_loadingtargets$TN_or_TP) {
    foreach(i = 1:length(param_loads_lim_tp)) %do% {
      
      temp_ripar_rem <- riparian_tp_removal[[i]]
      
      if (!is.null(temp_ripar_rem) && nrow(temp_ripar_rem) > 0) {
        write(
          paste0(
            "\nparam riparianremoval_P",
            i,
            " : 'Grassed_Buffer' 'Forested_Buffer':="
          ),
          file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
          append = T
        )
        
        # Prepare the data in the required format
        formatted_data <- temp_ripar_rem %>%
          mutate(year = as.numeric(year)) %>%
          select(
            comid_form,
            season,
            year,
            `'Grassed_Buffer'` = Grassed_Buffer,
            `'Forested_Buffer'` = Forested_Buffer
          ) %>%
          arrange(year, comid_form, season)
        
        # Write the formatted data to the file
        write.table(
          formatted_data,
          file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
          append = T,
          sep = "\t",
          row.names = FALSE,
          col.names = FALSE,
          na = "",
          quote = FALSE
        )
      } else {
        write(
          paste0("\nparam riparianremoval_P", i," : 'Grassed_Buffer' 'Forested_Buffer' :="),
          file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
          append = T
        )
      }
      write(
        ";",
        file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
        append = T
      )
    }
  }
)


### Urban Runoff Coefficients -----
write(  
  "\nparam runoff_coeff_urban : 'urban' :=", 
  file = paste(OutPath, "STdata_seasonal.dat", sep = ""), 
  append = T
)
write.table(
  runoffcoeff_dat %>% select(comid_form, urban = runoffcoeff),  # renaming also forces the order incase they are disordered in preprocessing code above
  file = paste(OutPath, "STdata_seasonal.dat", sep = "") , 
  append = T,
  sep = "\t",
  row.names = F,
  col.names = F,
  na = "",
  quote = F
)
write( ";", file = paste(OutPath, "STdata_seasonal.dat", sep=""), append = T)

### Agricultural BMP Costs ------
if(length(Ag_BMPs) > 0) {
  cat(
    "\nparam ag_costs_capital : ",bmp_ag_vec," :=	",
    file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
    sep = "", 
    append = T
  )
  cat(
    "\n",
    file= paste(OutPath, "STdata_seasonal.dat", sep = ""),
    sep = "", 
    append = T
  )
  write.table(
    ag_costs_cap_dat %>% select(comid_form, all_of(bmp_ag_vec_direct)), # selecting forces the order incase they are disordered in preprocessing code above
    file = paste(OutPath, "STdata_seasonal.dat" ,sep = "") , 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na="",
    quote = F
  )
  write( ";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = T)
  
  cat(
    "\nparam ag_costs_operations : ",bmp_ag_vec," :=	",
    file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
    sep = "", 
    append = T
  )
  cat(
    "\n",
    file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
    sep = "", 
    append = T
  )
  write.table(
    ag_costs_op_dat %>% select(comid_form, all_of(bmp_ag_vec_direct)), # selecting forces the order incase they are disordered in preprocessing code above
    file = paste(OutPath, "STdata_seasonal.dat", sep = "") , 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na = "",
    quote = F
  )
  write( ";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = T)
} else {
  cat(
    "\nparam ag_costs_capital : 'none' :=	",
    file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
    sep = "", 
    append = T
  )
  cat(
    "\n",
    file= paste(OutPath, "STdata_seasonal.dat", sep = ""),
    sep = "", 
    append = T
  )
  write.table(
    ag_bmp_dummy %>% select(comid, none), # selecting forces the order incase they are disordered in preprocessing code above
    file = paste(OutPath, "STdata_seasonal.dat" ,sep = "") , 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na="",
    quote = F
  )
  write( ";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = T)
  
  cat(
    "\nparam ag_costs_operations : 'none' :=	",
    file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
    sep = "", 
    append = T
  )
  cat(
    "\n",
    file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
    sep = "", 
    append = T
  )
  write.table(
    ag_bmp_dummy %>% select(comid, none), # selecting forces the order incase they are disordered in preprocessing code above
    file = paste(OutPath, "STdata_seasonal.dat", sep = "") , 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na = "",
    quote = F
  )
  write( ";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = T)
}

### Septic BMP Costs ------
if(length(Septic_BMPs) > 0 & "TN" %in% user_specs_loadingtargets$TN_or_TP) {
  cat(
    paste0("\nparam septic_costs_capital : ",paste(bmp_septic_vec, collapse = " ")," :=	"),
    file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
    sep = "", 
    append = T
  )
  cat(
    "\n",
    file= paste(OutPath, "STdata_seasonal.dat", sep = ""),
    sep = "", 
    append = T
  )
  write.table(
    septic_cap_costs %>% select(comid_form, all_of(bmp_septic_vec_direct)), # selecting forces the order incase they are disordered in preprocessing code above
    file = paste(OutPath, "STdata_seasonal.dat" ,sep = "") , 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na="",
    quote = F
  )
  write( ";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = T)
  
  cat(
    paste0("\nparam septic_costs_operations : ", paste(bmp_septic_vec, collapse = " ")," :=	"),
    file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
    sep = "", 
    append = T
  )
  cat(
    "\n",
    file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
    sep = "", 
    append = T
  )
  write.table(
    septic_op_costs %>% select(comid_form, all_of(bmp_septic_vec_direct)), # selecting forces the order incase they are disordered in preprocessing code above
    file = paste(OutPath, "STdata_seasonal.dat", sep = "") , 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na = "",
    quote = F
  )
  write( ";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = T)
} else {
  # Write dummy septic costs if TN exists globally (mirrors TP behavior)
  if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    cat(
      "\nparam septic_costs_capital : 'none' :=	",
      file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
      sep = "", 
      append = T
    )
    cat(
      "\n",
      file= paste(OutPath, "STdata_seasonal.dat", sep = ""),
      sep = "", 
      append = T
    )
    write.table(
      septic_bmp_dummy %>% select(comid_form, none), # selecting forces the order incase they are disordered in preprocessing code above
      file = paste(OutPath, "STdata_seasonal.dat" ,sep = "") , 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na="",
      quote = F
    )
    write( ";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = T)
    
    cat(
      "\nparam septic_costs_operations : 'none' :=	",
      file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
      sep = "", 
      append = T
    )
    cat(
      "\n",
      file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
      sep = "", 
      append = T
    )
    write.table(
      septic_bmp_dummy %>% select(comid_form, none), # selecting forces the order incase they are disordered in preprocessing code above
      file = paste(OutPath, "STdata_seasonal.dat", sep = "") , 
      append = T,
      sep = "\t",
      row.names = F,
      col.names = F,
      na = "",
      quote = F
    )
    write( ";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = T)
  }
}

### Point Cost Coefficients ------
write( 
  "\nparam point_costs : 'capital' 'operations' :=	", 
  file = paste(OutPath, "STdata_seasonal.dat", sep = ""), 
  append = T
)
write.table(
  point_costs_dat %>% select(comid_form, capital, operations), # selecting forces the order incase they are disordered in preprocessing code above
  file = paste(OutPath, "STdata_seasonal.dat", sep = "") , 
  append = T,
  sep = "\t",
  row.names = F,
  col.names = F,
  na = "",
  quote = F
)
write( ";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = T)

write( 
  "\nparam urban_costs : 'capital' 'operations' :=	", 
  file = paste(OutPath, "STdata_seasonal.dat", sep = ""), 
  append = T
)
if(length(Urban_BMPs) > 0) {
  write.table(
    urban_costs_dat[, c("bmp_form", "capital", "operations")],
    file = paste(OutPath, "STdata_seasonal.dat", sep = ""), 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na = "",
    quote = F
  )
} else {
  write(
    "'none'      0      0",
    file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
    append = T
  )
}
write( ";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = T)

### Riparian BMP Cost Coefficients -----
if(length(RiparianBuffer_BMPs) > 0) {
  cat(
    "\nparam ripbuf_costs_capital : ", bmp_ripbuf_vec, " :=	",
    file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
    sep = "", 
    append = T
  )
  cat(
    "\n",
    file= paste(OutPath, "STdata_seasonal.dat", sep = ""),
    sep = "", 
    append = T
  )
  write.table(
    ripbuf_costs_cap_dat %>% select(comid_form, all_of(RiparianBuffer_BMPs)), # selecting forces the order incase they are disordered in preprocessing code above
    file = paste(OutPath, "STdata_seasonal.dat" ,sep = "") , 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na="",
    quote = F
  )
  write( ";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = T)
  
  cat(
    "\nparam ripbuf_costs_operations : ", bmp_ripbuf_vec," :=	",
    file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
    sep = "", 
    append = T
  )
  cat(
    "\n",
    file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
    sep = "", 
    append = T
  )
  write.table(
    ripbuf_costs_op_dat %>% select(comid_form, all_of(RiparianBuffer_BMPs)), # selecting forces the order incase they are disordered in preprocessing code above
    file = paste(OutPath, "STdata_seasonal.dat", sep = "") , 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na = "",
    quote = F
  )
  write( ";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = T)
} else {
  cat(
    "\nparam ripbuf_costs_capital : 'none' :=	",
    file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
    sep = "", 
    append = T
  )
  cat(
    "\n",
    file= paste(OutPath, "STdata_seasonal.dat", sep = ""),
    sep = "", 
    append = T
  )
  write.table(
    ripbuf_bmp_dummy %>% select(comid, none), # selecting forces the order incase they are disordered in preprocessing code above
    file = paste(OutPath, "STdata_seasonal.dat" ,sep = "") , 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na="",
    quote = F
  )
  write( ";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = T)
  
  cat(
    "\nparam ripbuf_costs_operations : 'none' :=	",
    file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
    sep = "", 
    append = T
  )
  cat(
    "\n",
    file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
    sep = "", 
    append = T
  )
  write.table(
    ripbuf_bmp_dummy %>% select(comid, none), # selecting forces the order incase they are disordered in preprocessing code above
    file = paste(OutPath, "STdata_seasonal.dat", sep = "") , 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na = "",
    quote = F
  )
  write( ";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = T)
}

### TN Load Limits -----

invisible(if ("TN" %in% user_specs_loadingtargets$TN_or_TP) {
  foreach(i = 1:length(param_loads_lim_tn)) %do% {
    
    temp_load_lim <- param_loads_lim_tn[[i]]
    
    if (nrow(temp_load_lim) > 0) {
      cat(
        paste0("\nparam loads_lim_N", i, " : 'limits' :=  \n"),
        file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
        sep = "",
        append = T
      )
      write.table(
        temp_load_lim %>%
          mutate(year = as.numeric(year)),
        # renaming also forces the order incase they are disordered in processing code above
        file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
        append = T,
        sep = "\t",
        row.names = F,
        col.names = F,
        na = "",
        quote = F
      )
    } else {
      cat(
        paste0("\nparam loads_lim_N", i, " : 'limits' :=  \n"),
        file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
        sep = "",
        append = T
      )
    } 
    write(";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = T)
  }
})

### TP Load Limits -----
invisible(if ("TP" %in% user_specs_loadingtargets$TN_or_TP) {
  foreach(i = 1:length(param_loads_lim_tp)) %do% {
    
    temp_load_lim <- param_loads_lim_tp[[i]]
    
    if (nrow(temp_load_lim) > 0) {
      cat(
        paste0("\nparam loads_lim_P", i, " : 'limits' :=  \n"),
        file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
        sep = "",
        append = T
      )
      write.table(
        temp_load_lim %>%
          mutate(year = as.numeric(year)),
        # renaming also forces the order incase they are disordered in processing code above
        file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
        append = T,
        sep = "\t",
        row.names = F,
        col.names = F,
        na = "",
        quote = F
      )
    } else {
      cat(
        paste0("\nparam loads_lim_P", i, " : 'limits' :=  \n"),
        file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
        sep = "",
        append = T
      )
    }
    write(";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = T)
  }
})

cat("\n", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), sep = "", append = T)

### Ag Cost Frac ------
cat(
  "\nparam agcost_frac := ",
  file = paste(OutPath, "STdata_seasonal.dat", sep = ""), 
  sep = "", 
  append = T
)
cat(
  agcost_frac, 
  file = paste(OutPath, "STdata_seasonal.dat", sep = ""), 
  sep = "", 
  append = T
)
cat(
  ";",file = paste(OutPath, "STdata_seasonal.dat", sep = ""), sep = "", append = T
)

cat(
  "
param acfttoft3 := 43559.9;
param pcp := 0.0833;
param agBMP_minarea := 0;",
  file = paste(OutPath, "STdata_seasonal.dat", sep = ""), sep = "\n", append = T
)

### Urban Implementation Fraction ------
write( 
  "\nparam: urban_frac_min urban_frac_max :=	", 
  file = paste(OutPath, "STdata_seasonal.dat", sep = ""), 
  append = T
)
if(length(Urban_BMPs) > 0) {
  write.table(
    urban_bmp_imp,
    file = paste(OutPath, "STdata_seasonal.dat", sep = ""), 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na = "",
    quote = F
  )
} else {
  write(
    "'none'     0     0",
    file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
    append = T
  )
}

write( ";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = T)

### Ag Implementation Fraction ------
write( 
  "\nparam: ag_frac_min ag_frac_max :=	", 
  file = paste(OutPath, "STdata_seasonal.dat", sep = ""), 
  append = T
)
if(length(Ag_BMPs) > 0) {
  write.table(
    ag_bmp_imp,
    file = paste(OutPath, "STdata_seasonal.dat", sep = ""), 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na = "",
    quote = F
  )
} else {
  write(
    "'none'     0     0",
    file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
    append = T
  )
}
write( ";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = T)

### Riparian Implementation Fraction -----
write( 
  "\nparam: ripbuf_frac_min ripbuf_frac_max :=	", 
  file = paste(OutPath, "STdata_seasonal.dat", sep = ""), 
  append = T
)
if(length(RiparianBuffer_BMPs) > 0) {
  write.table(
    ripbuf_bmp_imp,
    file = paste(OutPath, "STdata_seasonal.dat", sep = ""), 
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na = "",
    quote = F
  )
} else {
  write(
    "'none'     0     0",
    file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
    append = T
  )
}

write( ";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = T)

### Urban Design Depth -----
cat(
  "\nparam urban_design_depth :", bmp_urban_vec, " := \n",
  file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
  append = T
)
if(length(Urban_BMPs) > 0) {
  write.table(
    depth_by_state %>%
      # Switch comid_form to first column
      select(comid_form, all_of(bmp_urban_vec)), # selecting forces the order incase they are disordered in preprocessing code above
    file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
    append = T,
    sep = "\t",
    row.names = F,
    col.names = F,
    na = "",
    quote = F
  )
} else {
  write(
    "'none'     0",
    file = paste(OutPath, "STdata_seasonal.dat", sep = ""),
    append = T
  )
}
write( ";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = T)

### Urban Cost Adjustment Coefficients -----
write( 
  "\nparam: urban_cost_adjustment_coef :=	", 
  file = paste(OutPath, "STdata_seasonal.dat", sep = ""), 
  append = T
)
write.table(
  urban_cost_coeffs_dat,
  file = paste(OutPath, "STdata_seasonal.dat", sep = ""), 
  append = T,
  sep = "\t",
  row.names = F,
  col.names = F,
  na = "",
  quote = F
)
write( ";", file = paste(OutPath, "STdata_seasonal.dat", sep = ""), append = T)

#
#
## Write out model file to file -----

### Save COMIDs as vector ----
comid_vec_all <- paste0("'", streamcat_subset_all$comid,"'")
comid_vec_all[-length(comid_vec_all)] <- paste0(
  comid_vec_all[-length(comid_vec_all)], ','
)

comid_vec_all_N <- paste0(
  "'", unique(unlist(lapply(
    streamcat_subset_tn,
    function(x) x[!is.na(x$comid), ]
  ), use.names = FALSE)), "'"
)
comid_vec_all_N[-length(comid_vec_all_N)] <- paste0(
  comid_vec_all_N[-length(comid_vec_all_N)], ','
)

comid_vec_all_P <- paste0(
  "'", unique(unlist(lapply(
    streamcat_subset_tp,
    function(x) x[!is.na(x$comid), ]
  ), use.names = FALSE)), "'"
)
comid_vec_all_P[-length(comid_vec_all_P)] <- paste0(
  comid_vec_all_P[-length(comid_vec_all_P)], ','
)

comid_vec_N <- foreach(i = 1:length(streamcat_subset_tn)) %do% {
  
  comid_vec_N_tmp <- paste0("'", streamcat_subset_tn[[i]]$comid, "'")
  comid_vec_N_tmp[-length(comid_vec_N_tmp)] <- paste0(
    comid_vec_N_tmp[-length(comid_vec_N_tmp)], ','
  )
  comid_vec_N_tmp
}
comid_vec_P <- foreach(i = 1:length(streamcat_subset_tp)) %do% {
  
  comid_vec_P_tmp <- paste0("'", streamcat_subset_tp[[i]]$comid, "'")
  comid_vec_P_tmp[-length(comid_vec_P_tmp)] <- paste0(
    comid_vec_P_tmp[-length(comid_vec_P_tmp)], ','
  )
  comid_vec_P_tmp
}

### Write COMID sets ------

##### Creates new seasonal .mod file and begins printing AMPL
write(
  "set comid_all :=",
  file = paste(OutPath, "STmodel_seasonal.mod", sep = "")
)

cat(
  "{",comid_vec_all,"};",
  file = paste(OutPath, "STmodel_seasonal.mod", sep = ""), sep = "", append = T
)

cat("\n", file = paste(OutPath, "STmodel_seasonal.mod", sep = ""), sep = "", append = T)

if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
  cat(
    paste0("\nset comid_all_N within comid_all := "),
    file = paste0(OutPath, "STmodel_seasonal.mod"),
    sep = "",
    append = T
  )
  if (length(comid_vec_all_N) == 0 || all(comid_vec_all_N == "''") || all(comid_vec_all_N == "'NA'")) {
    cat(
      "{};",
      file = paste0(OutPath, "STmodel_seasonal.mod"),
      sep = "",
      append = T
    )
  } else {
    cat(
      "{",unique(comid_vec_all_N),"};",
      file = paste(OutPath, "STmodel_seasonal.mod", sep = ""), sep = "", append = T
    )
  }
  cat(
    "\n", file = paste(OutPath, "STmodel_seasonal.mod", sep = ""), sep = "", append = T
  )
}

if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
  cat(
    paste0("\nset comid_all_P within comid_all := "),
    file = paste0(OutPath, "STmodel_seasonal.mod"),
    sep = "",
    append = T
  )
  if (length(comid_vec_all_P) == 0 || all(comid_vec_all_P == "''") || all(comid_vec_all_P == "'NA'")) {
    cat(
      "{};",
      file = paste0(OutPath, "STmodel_seasonal.mod"),
      sep = "",
      append = T
    )
  } else {
    cat(
      "{",unique(comid_vec_all_P),"};",
      file = paste(OutPath, "STmodel_seasonal.mod", sep = ""), sep = "", append = T
    )
  }
  cat(
    "\n", file = paste(OutPath, "STmodel_seasonal.mod", sep = ""), sep = "", append = T
  )
}

invisible(
  if ("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    foreach(i = 1:length(comid_vec_N)) %do% {
      cat(
        paste0("\nset comid_N", i, " within comid_all_N := "),
        file = paste0(OutPath, "STmodel_seasonal.mod"),
        sep = "",
        append = T
      )
      # Check if comid_vec_N[[i]] is empty or contains only empty strings
      if (length(comid_vec_N[[i]]) == 0 || all(comid_vec_N[[i]] == "''") || all(comid_vec_N == "'NA'")) {
        cat(
          "{};",
          file = paste0(OutPath, "STmodel_seasonal.mod"),
          sep = "",
          append = T
        )
      } else {
        cat(
          "{", comid_vec_N[[i]], "};",
          file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
          sep = "",
          append = T
        )
      }
      cat(
        "\n",
        file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
        sep = "",
        append = T
      )
    }
  }
)

invisible(
  if ("TP" %in% user_specs_loadingtargets$TN_or_TP) {
    foreach(i = 1:length(comid_vec_P)) %do% {
      cat(
        paste0("\nset comid_P", i, " within comid_all_P := "),
        file = paste0(OutPath, "STmodel_seasonal.mod"),
        sep = "",
        append = T
      )
      # Check if comid_vec_P[[i]] is empty or contains only empty strings
      if (length(comid_vec_P[[i]]) == 0 || all(comid_vec_P[[i]] == "''") || all(comid_vec_P[[i]] == "'NA'")) {
        cat(
          "{};",
          file = paste0(OutPath, "STmodel_seasonal.mod"),
          sep = "",
          append = T
        )
      } else {
        cat(
          "{", comid_vec_P[[i]], "};",
          file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
          sep = "",
          append = T
        )
      }
      cat(
        "\n",
        file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
        sep = "",
        append = T
      )
    }
  }
)

### Write Sets for Seasons, Years, and BMPs -----
cat(
  "\nset seasons := 1..4 ordered; ",
  file = paste(OutPath, 'STmodel_seasonal.mod', sep = ""),
  sep = "\n",
  append = T
)

# Extract unique years from the 'year' column
unique_years <- unique(as.numeric(inc_tn$year))

min_year <- min(unique_years)
max_year <- max(unique_years)

# Create the AMPL set syntax
ampl_years_set <- paste0("\nset years := ",min_year,"..",max_year," ordered;") #  ordered

# Print the result
cat(ampl_years_set,
    file = paste(OutPath, 'STmodel_seasonal.mod', sep = ""),
    sep = "\n",
    append = T
)

cat(
  "\n\nset urban_bmp :=",
  file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
  sep = "\n", 
  append = T
)

if(length(Urban_BMPs) > 0) {
  cat(
    "{",bmp_urban_vec_comma,"};",
    file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
    sep = "", 
    append = T
  ) 
} else {
  cat(
    "{'none'};",
    file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
    sep = "", 
    append = T
  ) 
}

cat(
  "\n\nset urban_pervbmp within urban_bmp :=",
  file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
  sep = "\n", 
  append = T
)

if(length(Urban_BMPs) > 0) {
  if(!grepl("''", paste0(pervbmp_urban_vec_comma, collapse = ""))) {
    cat(
      "{",pervbmp_urban_vec_comma,"};",
      file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
      sep = "", 
      append = T
    )  
  } else {
    cat(
      "{};",
      file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
      sep = "", 
      append = T
    )  
  }
} else {
  cat(
    "{'none'};",
    file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
    sep = "", 
    append = T
  )  
}

cat(
  "\n\nset ag_bmp :=",
  file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
  sep = "\n", 
  append = T
)

if(length(Ag_BMPs) > 0) {
  cat(
    "{",bmp_ag_vec_comma,"};",
    file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
    sep = "", 
    append = T
  )
} else {
  cat(
    "{'none'};",
    file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
    sep = "", 
    append = T
  )
}

### Adds septic BMPs to model file ----
cat(
  "\n\nset septic_bmp :=",
  file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
  sep = "\n", 
  append = T
)

if(length(Septic_BMPs) > 0) {
  cat(
    "{", paste(bmp_septic_vec_comma, collapse = " "), "};",
    file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
    sep = "",
    append = T
  )
  
} else {
  cat(
    "{'none'};",
    file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
    sep = "", 
    append = T
  )
}

### ND: Define a filtered set that is empty unless Sewer_convert exists
cat(
  "\n\nset sewer_convert_in_model within septic_bmp := {e in septic_bmp: e = 'Sewer_convert'};",
  file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
  sep = "\n", 
  append = T
)

cat(
  "\n\nset ripbuf_bmp :=",
  file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
  sep = "\n", 
  append = T
)

if(length(RiparianBuffer_BMPs) > 0) {
  cat(
    "{",bmp_ripbuf_vec_comma,"};",
    file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
    sep = "", 
    append = T
  ) 
} else {
  cat(
    "{'none'};",
    file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
    sep = "", 
    append = T
  ) 
}



cat(
  "\n\nset loads_N :=
{'point', 'urban', 'ag', 'septic', 'storage', 'other'};

\nset loads_P :=
{'point', 'urban', 'ag', 'storage', 'other'};
  
set area_sub :=
{'urban', 'ag'};
  
set urban_imp :=
{'urban'};
  
set urban_c :=
{'urban'};
  
set point_c :=
{'point'};
  
set cost_type :=
{'capital', 'operations'};",
  file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
  sep = "\n", 
  append = T
)

cat(
  "\nset coeff := \n {'coeff'};\n",
  file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
  sep = "",
  append = T
)

cat(
  "\nset loads_riparian := \n {'riparian'};\n",
  file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
  sep = "",
  append = T
)

cat(
  "\nset limits := \n {'limits'};\n",
  file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
  sep = "",
  append = T
)

cat(
  "\nset other_loads := \n {'other_load'};\n",
  file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
  sep = "",
  append = T
)

cat(
  "\nset septic_bmp_totals := \n {'total_parcels'};\n",
  file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
  sep = "",
  append = T
)
cat("\n", file = paste(OutPath, "STmodel_seasonal.mod", sep = ""), sep = "", append = T)

### Create Parameters ----- 
invisible(
  if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    foreach(i = 1:length(comid_vec_N)) %do% {
      cat(
        paste0("param baseloads_N", i, " {comid_N", i, ", seasons, years, loads_N} >= 0;"),
        file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
        sep = "\n",
        append = T
      )
    }
  }
)

invisible(
  if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
    foreach(i = 1:length(comid_vec_P)) %do% {
      cat(
        paste0("param baseloads_P", i, " {comid_P", i, ", seasons, years, loads_P} >= 0;"),
        file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
        sep = "\n",
        append = T
      )
    }
  }
)

invisible(
  if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    foreach(i = 1:length(comid_vec_N)) %do% {
      cat(
        paste0("param riparianload_N", i, " {comid_N", i, ", seasons, years, loads_riparian} >= 0;"),
        file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
        sep = "\n",
        append = T
      )
    }
  }
)

invisible(
  if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
    foreach(i = 1:length(comid_vec_P)) %do% {
      cat(
        paste0("param riparianload_P", i, " {comid_P", i, ", seasons, years, loads_riparian} >= 0;"),
        file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
        sep = "\n",
        append = T
      )
    }
  }
)

cat(
  "\nparam area {comid_all,area_sub} >=0;

param urban_bmp_implementationpotential {comid_all, urban_bmp} >= 0;",
  file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
  sep = "\n",
  append = T
) 

if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
  cat(
    "\nparam septic_bmp_implementationpotential {comid_all_N, septic_bmp} >= 0;
param septic_bmp_implementationtotal {comid_all_N, septic_bmp_totals} >= 0;",
    file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
    sep = "\n",
    append = T
  )
}

cat(
  "\nparam unbuffered_banklength {comid_all, ripbuf_bmp} >= 0;
param total_banklength {comid_all} >= 0;
param ag_costs_capital {comid_all,ag_bmp};
param ag_costs_operations {comid_all,ag_bmp};
param point_costs {comid_all,cost_type};
param urban_costs {urban_bmp,cost_type};
param ripbuf_costs_capital {comid_all, ripbuf_bmp};
param ripbuf_costs_operations {comid_all, ripbuf_bmp};
param runoff_coeff_urban {comid_all,urban_imp} >=0;
param urban_cost_adjustment_coef {comid_all} >= 0;",
  file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
  sep = "\n",
  append = T
) 

if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
  cat(
    "\nparam septic_costs_capital {comid_all_N, septic_bmp} >= 0;
param septic_costs_operations {comid_all_N, septic_bmp} >= 0;",
    file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
    sep = "\n",
    append = T
  )
}

cat(
  "\n",
  file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
  sep = "\n",
  append = T
) 

if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
  cat(
    "\nparam ag_effic_N {comid_all_N ,ag_bmp};
param point_effic_N {comid_all_N, point_c};
param urban_effic_N {comid_all_N, urban_bmp};
param septic_effic_N {comid_all_N, septic_bmp};\n",
    file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
    sep = "\n", 
    append = T
  )
}

if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
  cat(
    "\nparam ag_effic_P {comid_all_P ,ag_bmp};
param point_effic_P {comid_all_P, point_c};
param urban_effic_P {comid_all_P, urban_bmp};\n",
    file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
    sep = "\n", 
    append = T
  )
}

invisible(
  if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    foreach(i = 1:length(comid_vec_N)) %do% {
      cat(
        paste0(
          "param riparianremoval_N", 
          i, 
          " {c in comid_N", 
          i, 
          ", s in seasons, y in years, r in ripbuf_bmp};"
        ),
        file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
        sep = "\n",
        append = T
      )
    }
  }
)

invisible(
  if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
    foreach(i = 1:length(comid_vec_P)) %do% {
      cat(
        paste0(
          "param riparianremoval_P", 
          i, 
          " {c in comid_P", 
          i, 
          ", s in seasons, y in years, r in ripbuf_bmp};"
        ),
        file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
        sep = "\n",
        append = T
      )
    }
  }
)

invisible(
  if("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    foreach(i = 1:length(param_loads_lim_tn)) %do% {
      cat(
        paste0("param loads_lim_N", i, " {seasons, years, limits} >= 0;"),
        file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
        sep = "\n",
        append = T
      )
    }
  }
)

cat("\n", file = paste(OutPath, "STmodel_seasonal.mod", sep = ""), append = T)

invisible(
  if("TP" %in% user_specs_loadingtargets$TN_or_TP) {
    foreach(i = 1:length(param_loads_lim_tp)) %do% {
      cat(
        paste0("param loads_lim_P", i, " {seasons, years, limits} >= 0;"),
        file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
        sep = "\n",
        append = T
      )
    }
  }
)

cat("\n", file = paste(OutPath, "STmodel_seasonal.mod", sep = ""), append = T)

invisible(
  foreach(i = 1:length(sm_N_dat)) %do% {
    cat(
      paste0("param transfer_coefficients_N", i, " {comid_N", i, ", seasons, years, coeff};"),
      file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
      sep = "\n",
      append = T
    )
  }
)

cat("\n", file = paste(OutPath, "STmodel_seasonal.mod", sep = ""), append = T)

invisible(
  foreach(i = 1:length(sm_P_dat)) %do% {
    cat(
      paste0("param transfer_coefficients_P", i, " {comid_P", i, ", seasons, years, coeff};"),
      file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
      sep = "\n",
      append = T
    )
  }
)

cat("\n", file = paste(OutPath, "STmodel_seasonal.mod", sep = ""), append = T)

cat(
  "param agcost_frac >=0;
param acfttoft3 >=0;
param pcp >=0;
param agBMP_minarea;

param urban_frac_min {urban_bmp} >=0, <= 1;
param urban_frac_max {u in urban_bmp} >= urban_frac_min[u], <= 1;

param ag_frac_min {ag_bmp} >= 0, <= 1;
param ag_frac_max {a in ag_bmp} >= ag_frac_min[a], <= 1;

param ripbuf_frac_min {ripbuf_bmp} >= 0, <= 1;
param ripbuf_frac_max {r in ripbuf_bmp} >= ripbuf_frac_min[r], <= 1;

param urban_design_depth {c in comid_all, u in urban_bmp} >= 0;
  
param ps_coef {c in comid_all} := point_costs[c,'capital'] + 
   point_costs[c,'operations'];",
  file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
  sep = "\n",
  append = T
)

# Add septic coefficient only if septic BMPs are enabled
if(length(Septic_BMPs) > 0) {
  cat(
    "   
param septic_coef {c in comid_all, e in septic_bmp} :=
   septic_costs_capital[c,e] + septic_costs_operations[c,e];",
    file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
    sep = "\n",
    append = T
  )
}

cat(
  "
param urban_coef {c in comid_all, u in urban_bmp} := acfttoft3 * pcp *
   urban_design_depth[c,u] * runoff_coeff_urban[c,'urban'] * area[c,'urban'] *
   (urban_costs[u,'capital'] + urban_costs[u,'operations']) *
   urban_cost_adjustment_coef[c];

param ag_coef {c in comid_all, a in ag_bmp} := agcost_frac * area[c,'ag'] * 
(ag_costs_capital[c,a] + ag_costs_operations[c,a]) ;

param ripbuf_coef {c in comid_all, r in ripbuf_bmp} := 

agcost_frac * (ripbuf_costs_capital[c, r] + ripbuf_costs_operations[c, r]);
 \n",
  file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
  sep = "\n",
  append = T
)

### Create Variables -----
cat(
  "var agBMP_bin {comid_all, ag_bmp} binary;
var urbanBMP_bin {comid_all, urban_bmp} binary;
var point_dec {comid_all} binary;",
  file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
  sep = "\n",
  append = T
)

# Add septic variables only if septic BMPs are enabled
if(length(Septic_BMPs) > 0) {
  cat(
    "var septic_bin {comid_all_N, septic_bmp} binary;",
    file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
    sep = "\n",
    append = T
  )
}

cat(
  "var urban_frac {c in comid_all, u in urban_bmp} 
   >= urban_frac_min[u] * urban_bmp_implementationpotential[c, u]  
   <= urban_frac_max[u] * urban_bmp_implementationpotential[c, u] :=0;
var ag_frac {comid_all, a in ag_bmp} >= ag_frac_min[a] <= ag_frac_max[a] :=0;",
  file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
  sep = "\n",
  append = T
)

# Add septic fraction variable only if septic BMPs are enabled
if(length(Septic_BMPs) > 0) {
  cat(
    "var septic_frac {c in comid_all_N, e in septic_bmp} 
   >= 0 
   <= septic_bmp_implementationpotential[c,e] := 0;",
    file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
    sep = "\n",
    append = T
  )
}

cat(
  "var ripbuf_length {c in comid_all, r in ripbuf_bmp} 
   >= 0 
   <= unbuffered_banklength[c, r] * ripbuf_frac_max[r] := 0;
   \n",
  file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
  sep = "\n",
  append = T
)

invisible(
  foreach(i = 1:length(param_loads_lim_tn)) %do% {
    cat(
      paste0("var inc_storage_load_N", i, "{c in comid_N", i, ", s in seasons, y in years} >= 0;"),
      file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
      sep = "\n",
      append = T
    )
  }
)

invisible(
  foreach(i = 1:length(param_loads_lim_tp)) %do% {
    cat(
      paste0("var inc_storage_load_P", i, "{c in comid_P", i, ", s in seasons, y in years} >= 0;"),
      file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
      sep = "\n",
      append = T
    )
  }
)

cat("\n", file = paste(OutPath, "STmodel_seasonal.mod", sep = ""), append = T)

### Create Optimization Equation ------
cat(
  "minimize cost: sum {c in comid_all} (ps_coef[c] * point_dec[c]) + 
sum {c in comid_all, u in urban_bmp} (urban_coef[c,u] * urban_frac[c,u]) + 
sum {c in comid_all, a in ag_bmp} (ag_coef[c,a] * ag_frac[c,a]) +",
  file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
  sep = "\n", 
  append = T
)

# Add septic cost component only if septic BMPs are enabled
if(length(Septic_BMPs) > 0) {
  cat(
    "sum {c in comid_all, e in septic_bmp} (septic_coef[c,e] * septic_frac[c,e]) +",
    file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
    sep = "\n", 
    append = T
  )
}

cat(
  "sum {c in comid_all, r in ripbuf_bmp} (ripbuf_coef[c, r] * ripbuf_length[c, r]);
  \n",
  file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
  sep = "\n", 
  append = T
)


### Incremental Storage Load Equations -----

# Function to create septic terms conditionally
# ND added 
create_septic_terms <- function() {
  if(length(Septic_BMPs) > 0) {
    return(paste0(
      "(baseloads_N", "PLACEHOLDER_I", "[c,s,y,'point'] + sum {e in sewer_convert_in_model} ((baseloads_N", "PLACEHOLDER_I", "[c,s,y,'septic']/septic_bmp_implementationtotal[c, 'total_parcels']) * septic_frac[c,'Sewer_convert'] * septic_effic_N[c,'Sewer_convert'])) * (1 - (point_effic_N[c,'point'] * point_dec[c])) + \n",
      "          (baseloads_N", "PLACEHOLDER_I", "[c,s,y,'septic'] - sum {e in septic_bmp} (baseloads_N", "PLACEHOLDER_I", "[c,s,y,'septic']/septic_bmp_implementationtotal[c, 'total_parcels']) * septic_frac[c,e] * septic_effic_N[c,e])"
    ))
  } else {
    return("baseloads_NPLACEHOLDER_I[c,s,y,'point'] * (1 - (point_effic_N[c,'point'] * point_dec[c]))")
  }
}

if(actionable_load == TRUE){
  invisible(
    if ("TN" %in% user_specs_loadingtargets$TN_or_TP) {
      foreach(i = 1:length(param_loads_lim_tn)) %do% {
        
        # Create septic terms for this watershed
        septic_terms <- create_septic_terms()
        septic_terms <- gsub("PLACEHOLDER_I", i, septic_terms)
        
        cat(
          paste0(
            "subject to calc_inc_storage_load_N", i, "{c in comid_N", i, ", s in seasons, y in years}:\n
          inc_storage_load_N", i, "[c,s,y] = if s=first(seasons) then (if y = first(years) then ((baseloads_N", i, "[c,s,y,'storage'] + \n
          baseloads_N", i, "[c,s,y,'ag'] * (1 - sum {a in ag_bmp}(ag_effic_N[c,a] * ag_frac[c,a])) + \n
          baseloads_N", i, "[c,s,y,'urban'] * (1 - sum{u in urban_bmp}(urban_effic_N[c,u] * urban_frac[c,u])) + \n
          ", septic_terms, " - \n
          sum {r in ripbuf_bmp} (ripbuf_length[c,r] * riparianremoval_N", i, "[c,s,y,r]))) else ((inc_storage_load_N", i, "[c,4,y-1] * transfer_coefficients_N", i, "[c,4,y-1,'coeff']) + \n
          baseloads_N", i, "[c,s,y,'ag'] * (1 - sum {a in ag_bmp}(ag_effic_N[c,a] * ag_frac[c,a])) + \n
          baseloads_N", i, "[c,s,y,'urban'] * (1 - sum{u in urban_bmp}(urban_effic_N[c,u] * urban_frac[c,u])) + \n
          ", septic_terms, " -  \n
          sum {r in ripbuf_bmp} (ripbuf_length[c,r] * riparianremoval_N", i, "[c,s,y,r]))) else ((inc_storage_load_N", i, "[c,s-1,y] * transfer_coefficients_N", i, "[c,s-1,y,'coeff']) + \n
          baseloads_N", i, "[c,s,y,'ag'] * (1 - sum {a in ag_bmp}(ag_effic_N[c,a] * ag_frac[c,a])) + \n
          baseloads_N", i, "[c,s,y,'urban'] * (1 - sum{u in urban_bmp}(urban_effic_N[c,u] * urban_frac[c,u])) + \n
          ", septic_terms, " -  \n
          sum {r in ripbuf_bmp} (ripbuf_length[c,r] * riparianremoval_N", i, "[c,s,y,r]));\n"
          ),
          file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
          sep = "\n", 
          append = T
        )
      }
    }
  ) #equation version without other load component
  print("Basing TN incremental equations only on actionable load components")
}else{
  print("Basing TN incremental equations on all load components")
}
if(actionable_load == FALSE){
invisible(
  if ("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    foreach(i = 1:length(param_loads_lim_tn)) %do% {
      # Create conditional septic terms for complex equation
      if(length(Septic_BMPs) > 0) {
        septic_point_term <- paste0("((baseloads_N", i, "[c,s,y,'septic']/septic_bmp_implementationtotal[c, 'total_parcels']) * septic_frac[c,'Sewer_convert'] * septic_effic_N[c,'Sewer_convert'])")
        septic_reduction_term <- paste0("(baseloads_N", i, "[c,s,y,'septic'] - sum {e in septic_bmp} (baseloads_N", i, "[c,s,y,'septic']/septic_bmp_implementationtotal[c, 'total_parcels']) * septic_frac[c,e] * septic_effic_N[c,e])")
      } else {
        septic_point_term <- "0"
        septic_reduction_term <- "0"
      }
      
      cat(
        paste0(
          "subject to calc_inc_storage_load_N", i, "{c in comid_N", i, ", s in seasons, y in years}:\n
          inc_storage_load_N", i, "[c,s,y] = if s=first(seasons) then (if y = first(years) then ((baseloads_N", i, "[c,s,y,'storage'] + \n
          baseloads_N", i, "[c,s,y,'other'] + \n
          baseloads_N", i, "[c,s,y,'ag'] * (1 - sum {a in ag_bmp}(ag_effic_N[c,a] * ag_frac[c,a])) + \n
          baseloads_N", i, "[c,s,y,'urban'] * (1 - sum{u in urban_bmp}(urban_effic_N[c,u] * urban_frac[c,u])) + \n
          (baseloads_N", i, "[c,s,y,'point'] + ", septic_point_term, ") * (1 - (point_effic_N[c,'point'] * point_dec[c])) + \n
          ", septic_reduction_term, " - \n
          sum {r in ripbuf_bmp} (ripbuf_length[c,r] * riparianremoval_N", i, "[c,s,y,r]))) else ((inc_storage_load_N", i, "[c,4,y-1] * transfer_coefficients_N", i, "[c,4,y-1,'coeff']) + \n
          baseloads_N", i, "[c,s,y,'other'] + \n
          baseloads_N", i, "[c,s,y,'ag'] * (1 - sum {a in ag_bmp}(ag_effic_N[c,a] * ag_frac[c,a])) + \n
          baseloads_N", i, "[c,s,y,'urban'] * (1 - sum{u in urban_bmp}(urban_effic_N[c,u] * urban_frac[c,u])) + \n
          (baseloads_N", i, "[c,s,y,'point'] + ", septic_point_term, ") * (1 - (point_effic_N[c,'point'] * point_dec[c])) + \n
          ", septic_reduction_term, " -  \n
          sum {r in ripbuf_bmp} (ripbuf_length[c,r] * riparianremoval_N", i, "[c,s,y,r]))) else ((inc_storage_load_N", i, "[c,s-1,y] * transfer_coefficients_N", i, "[c,s-1,y,'coeff']) + \n
          baseloads_N", i, "[c,s,y,'other'] + \n
          baseloads_N", i, "[c,s,y,'ag'] * (1 - sum {a in ag_bmp}(ag_effic_N[c,a] * ag_frac[c,a])) + \n
          baseloads_N", i, "[c,s,y,'urban'] * (1 - sum{u in urban_bmp}(urban_effic_N[c,u] * urban_frac[c,u])) + \n
          (baseloads_N", i, "[c,s,y,'point'] + ", septic_point_term, ") * (1 - (point_effic_N[c,'point'] * point_dec[c])) + \n
          ", septic_reduction_term, " -  \n
          sum {r in ripbuf_bmp} (ripbuf_length[c,r] * riparianremoval_N", i, "[c,s,y,r]));\n"
        ),
        file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
        sep = "\n", 
        append = T
      )
    }
  }
  )#equation version with other load component
  print("TN Incremental equations based on all load components")
}else{
  print("TN Incremental equations based only on actionable load components")
}
if(actionable_load == TRUE){
  invisible(
    if ("TP" %in% user_specs_loadingtargets$TN_or_TP) {
      foreach(i = 1:length(param_loads_lim_tp)) %do% {
        cat(
          paste0(
            "subject to calc_inc_storage_load_P", i, "{c in comid_P", i, ", s in seasons, y in years}:\n
      inc_storage_load_P", i, "[c,s,y] = if s=first(seasons) then (if y = first(years) then (baseloads_P", i, "[c,s,y,'storage'] + \n
      baseloads_P", i, "[c,s,y,'ag'] * (1 - sum {a in ag_bmp}(ag_effic_P[c,a] * ag_frac[c,a])) + \n
      baseloads_P", i, "[c,s,y,'urban'] * (1 - sum{u in urban_bmp}(urban_effic_P[c,u] * urban_frac[c,u])) + \n 
      baseloads_P", i, "[c,s,y,'point'] * (1 - (point_effic_P[c,'point'] * point_dec[c])) - \n
      sum {r in ripbuf_bmp} (ripbuf_length[c,r] * riparianremoval_P", i, "[c,s,y,r])) else ((inc_storage_load_P", i, "[c,4,y-1] * transfer_coefficients_P", i, "[c,4,y-1,'coeff']) + \n
      baseloads_P", i, "[c,s,y,'ag'] * (1 - sum {a in ag_bmp}(ag_effic_P[c,a] * ag_frac[c,a])) + \n
      baseloads_P", i, "[c,s,y,'urban'] * (1 - sum{u in urban_bmp}(urban_effic_P[c,u] * urban_frac[c,u])) + \n
      baseloads_P", i, "[c,s,y,'point'] * (1 - (point_effic_P[c,'point'] * point_dec[c])) - \n
      sum {r in ripbuf_bmp} (ripbuf_length[c,r] * riparianremoval_P", i, "[c,s,y,r]))) else ((inc_storage_load_P", i, "[c,s-1,y] * transfer_coefficients_P", i, "[c,s-1,y,'coeff']) + \n
      baseloads_P", i, "[c,s,y,'ag'] * (1 - sum {a in ag_bmp}(ag_effic_P[c,a] * ag_frac[c,a])) + \n
      baseloads_P", i, "[c,s,y,'urban'] * (1 - sum{u in urban_bmp}(urban_effic_P[c,u] * urban_frac[c,u])) + \n
      baseloads_P", i, "[c,s,y,'point'] * (1 - (point_effic_P[c,'point'] * point_dec[c])) - \n
      sum {r in ripbuf_bmp} (ripbuf_length[c,r] * riparianremoval_P", i, "[c,s,y,r]));\n"
          ),
          file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
          sep = "\n", 
          append = T
        )
      }
    }
  ) #equation version without other load component
  print("Basing TP incremental equations only on actionable load components")
}else{
  print("Basing TP incremental equations on all load components")
}

if(actionable_load == FALSE){
invisible(
  if ("TP" %in% user_specs_loadingtargets$TN_or_TP) {
    foreach(i = 1:length(param_loads_lim_tp)) %do% {
      cat(
        paste0(
          "subject to calc_inc_storage_load_P", i, "{c in comid_P", i, ", s in seasons, y in years}:\n
      inc_storage_load_P", i, "[c,s,y] = if s=first(seasons) then (if y = first(years) then (baseloads_P", i, "[c,s,y,'storage'] + \n
      baseloads_P", i, "[c,s,y,'other'] + \n
      baseloads_P", i, "[c,s,y,'ag'] * (1 - sum {a in ag_bmp}(ag_effic_P[c,a] * ag_frac[c,a])) + \n
      baseloads_P", i, "[c,s,y,'urban'] * (1 - sum{u in urban_bmp}(urban_effic_P[c,u] * urban_frac[c,u])) + \n 
      baseloads_P", i, "[c,s,y,'point'] * (1 - (point_effic_P[c,'point'] * point_dec[c])) - \n
      sum {r in ripbuf_bmp} (ripbuf_length[c,r] * riparianremoval_P", i, "[c,s,y,r])) else ((inc_storage_load_P", i, "[c,4,y-1] * transfer_coefficients_P", i, "[c,4,y-1,'coeff']) + \n
      baseloads_P", i, "[c,s,y,'other'] + \n
      baseloads_P", i, "[c,s,y,'ag'] * (1 - sum {a in ag_bmp}(ag_effic_P[c,a] * ag_frac[c,a])) + \n
      baseloads_P", i, "[c,s,y,'urban'] * (1 - sum{u in urban_bmp}(urban_effic_P[c,u] * urban_frac[c,u])) + \n
      baseloads_P", i, "[c,s,y,'point'] * (1 - (point_effic_P[c,'point'] * point_dec[c])) - \n
      sum {r in ripbuf_bmp} (ripbuf_length[c,r] * riparianremoval_P", i, "[c,s,y,r]))) else ((inc_storage_load_P", i, "[c,s-1,y] * transfer_coefficients_P", i, "[c,s-1,y,'coeff']) + \n
      baseloads_P", i, "[c,s,y,'other'] + \n
      baseloads_P", i, "[c,s,y,'ag'] * (1 - sum {a in ag_bmp}(ag_effic_P[c,a] * ag_frac[c,a])) + \n
      baseloads_P", i, "[c,s,y,'urban'] * (1 - sum{u in urban_bmp}(urban_effic_P[c,u] * urban_frac[c,u])) + \n
      baseloads_P", i, "[c,s,y,'point'] * (1 - (point_effic_P[c,'point'] * point_dec[c])) - \n
      sum {r in ripbuf_bmp} (ripbuf_length[c,r] * riparianremoval_P", i, "[c,s,y,r]));\n"
        ),
        file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
        sep = "\n", 
        append = T
      )
    }
  }
  ) #equation version with other load component
  print("Incremental TP equations based on all load components")
}else{
  print("Incremental TP equations based only on actionable load components")
}


### Total Load Equations -----

invisible(if ("TN" %in% user_specs_loadingtargets$TN_or_TP) {
  foreach(i = 1:length(param_loads_lim_tn)) %do% {
    if (nrow(param_loads_lim_tn[[i]]) > 0) {
      cat(
        paste0(
          "subject to total_loads_N",
          i,
          "{s in seasons, y in years}:\n",
          "sum{c in comid_N",
          i,
          "} (inc_storage_load_N",
          i,
          "[c,s,y])",
          
          " <= loads_lim_N",
          i,
          "[s,y,'limits'];\n"
        ),
        file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
        sep = "\n",
        append = T
      )
    }
  }
})


invisible(if ("TP" %in% user_specs_loadingtargets$TN_or_TP) {
  foreach(i = 1:length(param_loads_lim_tp)) %do% {
    if (nrow(param_loads_lim_tp[[i]]) > 0) {
      cat(
        paste0(
          "subject to total_loads_P",
          i,
          "{s in seasons, y in years}:\n",
          "sum{c in comid_P",
          i,
          "} (inc_storage_load_P",
          i,
          "[c,s,y])",
          " <= loads_lim_P",
          i,
          "[s,y,'limits'];\n"
        ),
        file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
        sep = "\n",
        append = T
      )
    }
  }
})


### Riparian Load Equations -----
invisible(
  if ("TN" %in% user_specs_loadingtargets$TN_or_TP) {
    foreach(i = 1:length(param_loads_lim_tn)) %do% {
      cat(
        paste0(
          "subject to riparian_loads_N", i, " {c in comid_N", i, ", s in seasons, y in years}:\n",
          "sum {r in ripbuf_bmp} (ripbuf_length[c, r] * riparianremoval_N", i, 
          "[c,s,y,r]) <= riparianload_N", i, "[c,s,y,'riparian']; \n"
        ),
        file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
        sep = "\n", 
        append = T
      )
    }
  }
)

invisible(
  if ("TP" %in% user_specs_loadingtargets$TN_or_TP) {
    foreach(i = 1:length(param_loads_lim_tp)) %do% {
      cat(
        paste0(
          "subject to riparian_loads_P", i, " {c in comid_P", i, ", s in seasons, y in years}:\n",
          "sum {r in ripbuf_bmp} (ripbuf_length[c, r] * riparianremoval_P", i, 
          "[c,s,y,r]) <= riparianload_P", i, "[c,s,y,'riparian']; \n"
        ),
        file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
        sep = "\n", 
        append = T
      )
    }
  }
)

### BMP Implementation Constraints -------
cat(
  "subject to ag_treat_min {c in comid_all, a in ag_bmp}:
ag_frac[c,a] * area[c,'ag'] >= agBMP_minarea * agBMP_bin[c,a];
  
subject to ag_frac_const {c in comid_all, a in ag_bmp}:
ag_frac[c,a] <= agBMP_bin[c,a];
  
subject to urban_frac_const {c in comid_all, u in urban_bmp}:
urban_frac[c,u] <= urbanBMP_bin[c,u];

subject to ag_frac_limit {c in comid_all}: 
sum {a in ag_bmp} ag_frac[c,a] <= 1;
# prevents multiple BMPs being implemented on the same area

subject to urban_frac_limit {c in comid_all}: 
sum {u in urban_bmp} urban_frac[c,u] <= 1;
# prevents multiple BMPs being implemented on the same area

subject to total_banks {c in comid_all}:
sum {r in ripbuf_bmp} ripbuf_length[c,r] <= max {r in ripbuf_bmp} 
   unbuffered_banklength[c,r];
",
  file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
  sep = "\n", 
  append = T
)

### Conditional Urban BMP Implementation Constraints ------
if(!grepl("''", paste0(pervbmp_urban_vec_comma, collapse = ""))){
  cat(
    "
subject to roads_and_parkinglots {c in comid_all}:
sum{up in urban_pervbmp} urban_frac[c, up] <= max {up in urban_pervbmp} 
   urban_bmp_implementationpotential[c,up];
",
    file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
    sep = "\n", 
    append = T
  )
}

print(
  paste(
    "RBEROST has finished writing AMPL scripts without uncertainty at",
    Sys.time()
  )
)

### Septic Load Reduction Constraints
if(length(Septic_BMPs) > 0) {
  invisible(
    if ("TN" %in% user_specs_loadingtargets$TN_or_TP) {
      foreach(i = 1:length(param_loads_lim_tn)) %do% {
        cat(
          paste0(
            "subject to septic_reduction_limit_N", i, " {c in comid_N", i, ", s in seasons, y in years}:\n",
            "sum {e in septic_bmp} (baseloads_N", i, "[c,s,y,'septic']/septic_bmp_implementationtotal[c, 'total_parcels']) * septic_frac[c,e] * septic_effic_N[c,e] <= baseloads_N", i, "[c,s,y,'septic'];\n"
          ),
          file = paste(OutPath, "STmodel_seasonal.mod", sep = ""),
          sep = "\n", 
          append = T
        )
      }
    }
  )
}