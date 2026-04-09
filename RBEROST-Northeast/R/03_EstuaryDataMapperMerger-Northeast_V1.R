#   Estuary Data Mapper Preparation Code
#   Date Created: 4/2/2024 (Hunter Parker)
#   Date Updated: 10/31/2025 (Sam Ennett)
#   Purpose: This code helps prepare the files for the Estuary Data Mapper portion of the RBEROST TO.
#
#   UPDATE NOTES (10/31/2025):
#   - Updated to align with actual preprocessing code files:
#     * 01_Optimization_Preprocessing-Northeast-Seasonal_V6_[INDEV].R
#     * 01_Optimization_Preprocessing+Uncertainty-Northeast_Seasonal_[INDEV].R
#   - Changed from old SPARROW output files (ne_sparrow_model_output_tn/tp.csv) 
#     to new files (Predict_N.csv, Predict_P.csv)
#   - Added SPARROW standard error files (ne_dynamic_sparrow_se_tn/tp.csv)
#   - Added StreamCat data (CT_NH_MA_VT_NY_RI_StreamCatData_2019.csv)
#   - Added unclassified septic parcels (Unclassified_Septic_Parcels_LIS.xlsx)
#   - Updated septic file names to match preprocessing (01_RevSepticUpgrde_ParcelEff.csv, 01_RevSepticConv_ParcelEff.csv)
#   - Updated ACRE file names to include ICF25 suffix
#   - Updated riparian loading file to ICF25 version
#   - Removed files not used in preprocessing: NdepChange_2012_2019.csv, 
#     01_Additional_Wasteload.csv, 01_SepticConversion_EC.csv, 01_SepticUpgrade_EC.csv, WWTP_COMIDs_BslnRemoval.csv
#   - Separated temporal data (SPARROW outputs with year/season) into separate export files
#   - Added proper handling of comid_time parsing for SPARROW output files
#   - Total input files tracked: 25 (23 standard preprocessing + 2 uncertainty)

# SET SETTINGS,PATHS AND PACKAGES ----

### Settings
rm(list=ls())
options(stringsAsFactors=FALSE)
user <- "56407"

### Packages
pacman::p_load(openxlsx, tigris, tidyverse, readxl, sf, dplyr, janitor, fuzzyjoin, beepr, nhdplusTools, ggplot2, spData, leaflet, dataRetrieval, stringr)

nor_inputs <- "C:/RBEROST/Tier_1_Optimization-SSWR.5.3.2/RBEROST-Northeast/Preprocessing/Inputs/"

list_path <- "C:/RBEROST/Tier_1_Optimization-SSWR.5.3.2/RBEROST-Northeast/Estuary Data Mapper/"


# Get updated lists of files to merge together -----
lis_known <- rbind(read.csv(paste0(list_path,"Known 01_Preprocessing Inputs_Northeast_YesUncert.csv")),
                   read.csv(paste0(list_path,"Known 01_Preprocessing Inputs_Northeast_NoUncert.csv")))

all_known <- lis_known

# Northeast Data work -----

### Load in data sets -----
#1 checked - User specs BMPs
lis_user_bmpswcs <- read_csv(paste0(nor_inputs, "01_UserSpecs_BMPs_wCS.csv"))

#2 checked - User specs loading targets
lis_user_loadingtargets <- read_csv(paste0(nor_inputs, "01_UserSpecs_loadingtargets.csv"))%>%
  rename("comid"="ComID")%>%
  mutate(
    terminal_crosswalk=paste0(comid,watershed_name)
  ) 

#3 checked - Terminal COMIDs
terminal_comid_lis <- read_csv(paste0(nor_inputs, "01_Preprocessing_Terminal_COMID.csv"))%>%
  mutate(
    terminal_crosswalk=paste0(comid,watershed_name),
    terminal_flag = 1
  ) 

#4 checked - Upstream COMIDs
upstream_comid_lis <- read_csv(paste0(nor_inputs, "01_Preprocessing_Upstream_COMID.csv"))%>%
  rename("comid"="catchment_comid")%>%
  mutate(
    terminal_crosswalk=paste0(comid,watershed_name)
  ) 

#5 checked - SPARROW model inputs
lis_sparrow_inputs <- read.csv(paste0(nor_inputs, "ne_sparrow_model_input.csv"))%>%
  rename("comid"="ComID")

#6 checked - SPARROW TN predictions (Predict_N.csv)
lis_sparrow_out_tn <- data.table::fread(paste0(nor_inputs, "Predict_NoBFlowN_WSeptic.csv"))

#7 checked - SPARROW TP predictions (Predict_P.csv)
lis_sparrow_out_tp <- data.table::fread(paste0(nor_inputs, "Predict_P.csv"))

#8 checked - SPARROW TN standard errors
lis_sparrow_se_tn <- read_csv(paste0(nor_inputs, "ne_dynamic_sparrow_se_tn.csv"))

#9 checked - SPARROW TP standard errors
lis_sparrow_se_tp <- read_csv(paste0(nor_inputs, "ne_dynamic_sparrow_se_tp.csv"))

#10 checked - StreamCat data
streamcat_data <- read_csv(paste0(nor_inputs, "CT_NH_MA_VT_NY_RI_StreamCatData_2019.csv"))

#11 checked - Unclassified septic parcels
unclassified_septic <- readxl::read_xlsx(paste0(nor_inputs, "Unclassified_Septic_Parcels_LIS.xlsx"))

#12 checked - Septic upgrades
septic_upgrades <- read_csv(paste0(nor_inputs, "01_RevSepticUpgrde_ParcelEff.csv"))

#13 checked - Septic conversions
septic_conversion <- read_csv(paste0(nor_inputs, "01_RevSepticConv_ParcelEff.csv"))

#14 checked and updated - Riparian loadings
lis_riparian_loadings <- read_csv(paste0(nor_inputs, "RiparianLoadings_ICF25.csv")) %>%
    rename("comid"="COMID")

#15 checked and updated - Length in buffer
lengthbuffer_lis <- read_csv(paste0(nor_inputs, "LengthinBuffer_ICF24.csv"))

#16 checked and updated - Riparian efficiencies
lis_riparian_efficiencies <- read_csv(paste0(nor_inputs, "RiparianEfficiencies_ICF24.csv"))

#17 checked - WWTP COMIDs
lis_wwtp_comids <- read_csv(paste0(nor_inputs, "WWTP_COMIDs.csv"))%>%
  rename("comid"="COMID")

#18 checked - Agricultural BMP efficiencies
AgBMPEffic_fertmanure <- read_csv(paste0(nor_inputs, "AgBMPEffic_FertManure.csv"))

#19 checked - ACRE no practice comparison
ACRE_nopractice <- read_csv(paste0(nor_inputs, "ACRE_HUC12_HRU_Summary_compareNoPractice_ICF25.csv"))

#20 checked - ACRE baseline comparison
ACRE_baseline <- read_csv(paste0(nor_inputs, "ACRE_HUC12_HRU_Summary_compareBaseline_ICF25.csv"))

#21 checked - WWTP removal efficiencies
lis_wwtp_removaleffic <- read_csv(paste0(nor_inputs, "WWTP_RemovalEffic_upd.csv"))

#22 checked - Urban BMP performance curves
urbanBMPcurves <- read_csv(paste0(nor_inputs, "UrbanBMPPerformanceCurves.csv"))

#23 checked - Infiltration rates
infiltrationrates <- read_csv(paste0(nor_inputs, "NHD+infiltrationrates.csv"))

#24 (uncertainty) checked - EQIP costs over years
eqip_overyrs_lis <- read_csv(paste0(nor_inputs, "EQIPcosts_overyears.csv"))

#25 (uncertainty) checked - WWTP baseline removal fine grain
wwtp_finergrain <- read_csv(paste0(nor_inputs, "WWTP_BaselineRemoval_Finergrain.csv"))

### Merge Northeast data together -----

# Ensure comid is character in all datasets for proper merging
upstream_comid_lis$comid <- as.character(upstream_comid_lis$comid)
lis_user_loadingtargets$comid <- as.character(lis_user_loadingtargets$comid)
septic_conversion$comid <- as.character(septic_conversion$comid)
septic_upgrades$comid <- as.character(septic_upgrades$comid)
lengthbuffer_lis$comid <- as.character(lengthbuffer_lis$comid)
lis_sparrow_inputs$comid <- as.character(lis_sparrow_inputs$comid)
lis_riparian_efficiencies$comid <- as.character(lis_riparian_efficiencies$comid)
lis_riparian_loadings$comid <- as.character(lis_riparian_loadings$comid)
wwtp_finergrain$comid <- as.character(wwtp_finergrain$comid)
infiltrationrates$comid <- as.character(infiltrationrates$comid)
lis_wwtp_comids$comid <- as.character(lis_wwtp_comids$comid)

# Note: SPARROW output files (Predict_N.csv, Predict_P.csv) contain comid_time which needs parsing
# Extract comid from comid_time in SPARROW output files if needed
if("comid_time" %in% names(lis_sparrow_out_tn)) {
  lis_sparrow_out_tn$comid <- as.character(stringr::str_sub(lis_sparrow_out_tn$comid_time, 1, -4))
  lis_sparrow_out_tn$year <- as.integer(stringr::str_sub(lis_sparrow_out_tn$comid_time, -3, -2))
  lis_sparrow_out_tn$season <- as.integer(stringr::str_sub(lis_sparrow_out_tn$comid_time, -1))
}

if("comid_time" %in% names(lis_sparrow_out_tp)) {
  lis_sparrow_out_tp$comid <- as.character(stringr::str_sub(lis_sparrow_out_tp$comid_time, 1, -4))
    lis_sparrow_out_tp$year <- as.integer(stringr::str_sub(lis_sparrow_out_tp$comid_time, -3, -2))
    lis_sparrow_out_tp$season <- as.integer(stringr::str_sub(lis_sparrow_out_tp$comid_time, -1))
}

if("comid_time" %in% names(lis_sparrow_se_tn)) {
  lis_sparrow_se_tn$comid <- as.character(stringr::str_sub(lis_sparrow_se_tn$comid_time, 1, -4))
  lis_sparrow_se_tn$year <- as.integer(stringr::str_sub(lis_sparrow_se_tn$comid_time, -3, -2))
    lis_sparrow_se_tn$season <- as.integer(stringr::str_sub(lis_sparrow_se_tn$comid_time, -1))
}
if("comid_time" %in% names(lis_sparrow_se_tp)) {
  lis_sparrow_se_tp$comid <- as.character(stringr::str_sub(lis_sparrow_se_tp$comid_time, 1, -4))
    lis_sparrow_se_tp$year <- as.integer(stringr::str_sub(lis_sparrow_se_tp$comid_time, -3, -2))
        lis_sparrow_se_tp$season <- as.integer(stringr::str_sub(lis_sparrow_se_tp$comid_time, -1))
}

# StreamCat data
if("comid" %in% names(streamcat_data)) {
  streamcat_data$comid <- as.character(streamcat_data$comid)
}

length(unique(upstream_comid_lis$comid))  # Check number of unique COMIDs

# Single row per COMID merge - merging static datasets
northeast_merge <- upstream_comid_lis%>%
  mutate(
    terminal_flag=ifelse(terminal_crosswalk %in% terminal_comid_lis$terminal_crosswalk,1,0)
  )%>%
  merge(.,lis_user_loadingtargets%>%select(-watershed_name, -comid) ,by="terminal_crosswalk", all.x=TRUE)%>%
  select(-terminal_crosswalk)%>%
  merge(.,septic_conversion, by="comid", all.x=TRUE)%>%
  merge(.,septic_upgrades ,by="comid", all.x=TRUE)%>%
  merge(.,lengthbuffer_lis ,by="comid", all.x=TRUE)%>%
  merge(.,lis_sparrow_inputs ,by="comid", all.x=TRUE)%>%
  merge(.,lis_riparian_efficiencies ,by="comid", all.x=TRUE)%>%
  merge(.,wwtp_finergrain ,by="comid", all.x=TRUE)%>%
  merge(.,infiltrationrates ,by="comid",all.x=TRUE)%>%
  merge(.,lis_wwtp_comids ,by="comid", all.x=TRUE)

# Note: SPARROW output files and riparian loadings contain temporal data (year/season)
# These should be exported separately to preserve the time dimension
# Aggregating them would lose important temporal information

# Create separate temporal datasets for SPARROW outputs
sparrow_tn_temporal <- lis_sparrow_out_tn %>%
  select(comid, comid_season, year, season, contains(c("PLOAD", "in_", "DEL_FRAC")))

sparrow_tp_temporal <- lis_sparrow_out_tp %>%
  select(comid, comid_season, year, season, contains(c("PLOAD", "ip_", "ip", "DEL_FRAC")))

sparrow_tn_se_temporal <- lis_sparrow_se_tn %>%
  select(comid, year, season, contains("SE_"))

sparrow_tp_se_temporal <- lis_sparrow_se_tp %>%
  select(comid, year, season, contains("SE_"))

riparian_loadings_temporal <- lis_riparian_loadings %>%
  select(comid, year, season, contains("riparian"))

# StreamCat data merge (single row per COMID)
if(nrow(streamcat_data) > 0) {
  northeast_merge <- northeast_merge %>%
    merge(., streamcat_data, by="comid", all.x=TRUE)
}

#datasets that cannot be joined by COMID (BMP-specific or scenario-specific)
#  AgBMPEffic_fertmanure      # no comid, by BMP
#  lis_user_bmpswcs           # no comid, by BMP
#  ACRE_baseline              # no comid, by scenario/HUC
#  ACRE_nopractice            # no comid, by scenario/HUC
#  eqip_overyrs_lis           # no comid, by BMP
#  urbanBMPcurves             # no comid, by BMP
#  lis_wwtp_removaleffic      # no comid, by BMP
#  unclassified_septic        # May have comid - check structure

# QA Checks with skimr -----
pacman::p_load(skimr)
View(skim(northeast_merge))
View(skim(sparrow_tn_temporal))
View(skim(sparrow_tp_temporal))
View(skim(sparrow_tn_se_temporal))
View(skim(sparrow_tp_se_temporal))
View(skim(riparian_loadings_temporal))

# write out skmir reports
write.csv(skim(northeast_merge), paste0(list_path,"QA_Northeast_Merge_SkimReport.csv"))
write.csv(skim(sparrow_tn_temporal), paste0(list_path,"QA_SPARROW_TN_Temporal_SkimReport.csv"))
write.csv(skim(sparrow_tp_temporal), paste0(list_path,"QA_SPARROW_TP_Temporal_SkimReport.csv"))
write.csv(skim(sparrow_tn_se_temporal), paste0(list_path,"QA_SPARROW_TN_SE_Temporal_SkimReport.csv"))
write.csv(skim(sparrow_tp_se_temporal), paste0(list_path,"QA_SPARROW_TP_SE_Temporal_SkimReport.csv"))
write.csv(skim(riparian_loadings_temporal), paste0(list_path,"QA_Riparian_Loadings_Temporal_SkimReport.csv"))

### Write Northeast Data files ------

### EXPORT DATASETS (CANNOT COMBINE INTO SINGLE CSV SINCE THEY HAVE DIFFERENCES IN VARIABLES)

# Primary dataset - static data by COMID
write_csv(northeast_merge,paste0("C:/RBEROST/Tier_1_Optimization-SSWR.5.3.2/RBEROST-Northeast/Estuary Data Mapper/Outputs/Northeast_EDM_Primary.csv"))

# Temporal datasets - SPARROW outputs with year/season dimensions
write_csv(sparrow_tn_temporal, paste0("C:/RBEROST/Tier_1_Optimization-SSWR.5.3.2/RBEROST-Northeast/Estuary Data Mapper/Outputs/EDM_SPARROW_TN_Output.csv"))
write_csv(sparrow_tp_temporal, paste0("C:/RBEROST/Tier_1_Optimization-SSWR.5.3.2/RBEROST-Northeast/Estuary Data Mapper/Outputs/EDM_SPARROW_TP_Output.csv"))
write_csv(sparrow_tn_se_temporal, paste0("C:/RBEROST/Tier_1_Optimization-SSWR.5.3.2/RBEROST-Northeast/Estuary Data Mapper/Outputs/EDM_SPARROW_TN_SE.csv"))
write_csv(sparrow_tp_se_temporal, paste0("C:/RBEROST/Tier_1_Optimization-SSWR.5.3.2/RBEROST-Northeast/Estuary Data Mapper/Outputs/EDM_SPARROW_TP_SE.csv"))
write_csv(riparian_loadings_temporal, paste0("C:/RBEROST/Tier_1_Optimization-SSWR.5.3.2/RBEROST-Northeast/Estuary Data Mapper/Outputs/EDM_Riparian_Loadings.csv"))

# Write out datasets that cannot be reasonably added to bulk dataset (BMP-specific or scenario-specific)
write_csv(lis_user_bmpswcs, paste0("C:/RBEROST/Tier_1_Optimization-SSWR.5.3.2/RBEROST-Northeast/Estuary Data Mapper/Outputs/EDM_01_UserSpecs_BMPs.csv"))
write_csv(ACRE_baseline,paste0("C:/RBEROST/Tier_1_Optimization-SSWR.5.3.2/RBEROST-Northeast/Estuary Data Mapper/Outputs/EDM_ACRE_HUC12_HRU_Summary_compareBaseline.csv"))
write_csv(ACRE_nopractice,paste0("C:/RBEROST/Tier_1_Optimization-SSWR.5.3.2/RBEROST-Northeast/Estuary Data Mapper/Outputs/EDM_ACRE_HUC12_HRU_Summary_compareNoPractice.csv"))
write_csv(AgBMPEffic_fertmanure,paste0("C:/RBEROST/Tier_1_Optimization-SSWR.5.3.2/RBEROST-Northeast/Estuary Data Mapper/Outputs/EDM_AgBMPEffic_FertManure.csv"))
write_csv(eqip_overyrs_lis,paste0("C:/RBEROST/Tier_1_Optimization-SSWR.5.3.2/RBEROST-Northeast/Estuary Data Mapper/Outputs/EDM_EQIPcosts_overyears.csv"))
write_csv(urbanBMPcurves,paste0("C:/RBEROST/Tier_1_Optimization-SSWR.5.3.2/RBEROST-Northeast/Estuary Data Mapper/Outputs/EDM_UrbanBMPPerformanceCurves.csv"))
write_csv(lis_wwtp_removaleffic,paste0("C:/RBEROST/Tier_1_Optimization-SSWR.5.3.2/RBEROST-Northeast/Estuary Data Mapper/Outputs/EDM_WWTP_RemovalEffic_upd.csv"))

# Write out unclassified septic if it has data
if(exists("unclassified_septic") && nrow(unclassified_septic) > 0) {
  write_csv(unclassified_septic, paste0("C:/RBEROST/Tier_1_Optimization-SSWR.5.3.2/RBEROST-Northeast/Estuary Data Mapper/Outputs/EDM_Unclassified_Septic_Parcels.csv"))
}

### Clean Environment -----
rm(list=setdiff(ls(), c("user", "pac_inputs", "nor_inputs")))

#
#
#
#

# End Code -----
beepr::beep(sound=2)
