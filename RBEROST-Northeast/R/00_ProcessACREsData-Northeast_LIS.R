  
###########################################################################################
# PURPOSE: Calculate BMP Nitrogen & Phosphorus Removal Efficiencies  ####
# BY: Yishen Li, Alyssa Le, Cathy Chamberlin ####
# ORIGINAL DATE: 5/27/2020 ####
# Updated by Sam Ennett for all LIS ACRES data 7-1-2025
# Updated by Naomi Detenbeck (EPA) to include Pawcatuck R after updating HUC12 numbering
# in ACREs database 2026

###########################################################################################

# The purpose of this code is to calculate removal efficiencies based on the ACRE database.
# Lines of code tagged with #*# may need to be edited by the user.

##########################################################
# 1. Setup #####
##########################################################

# install.packages("dplyr") #*# #May need to be run if packages are not installed
# install.packages("stringr") #*# #May need to be run if packages are not installed

library("tidyverse")
library("stringr")
library("sf")
library("foreign")
options(stringsAsFactors = FALSE)
rm(list=ls())

##########################################################
# 2. Read CSV files ####
##########################################################

# Working directory
setwd("./") #*# This defaults to the project directory folder if run in an RProject in RStudio
#setwd("C:/Users/ndetenbe/OneDrive - Environmental Protection Agency (EPA)/RBEROSTV1D/")
# This dataset is a subset of the ACRE database for HUCs within the LIS basin.
ACRE_rawdata <- read.csv(
  "Data/ACRE_LIScorr.csv", #*# ND corrected dataset to include Pawcatuck
  header=TRUE,
  colClasses = c(
    "NULL",
    "character",
    rep("NULL",4),
    "character",
    rep("NULL",3),
    "character",
    rep("NULL",5),
    "character",
    rep("NULL",2),
    "character",
    "character",
    rep("NULL",31),
    "character",
    rep("NULL",11),
    "character",
    rep("NULL",6)
    )
  )

# Read in set of HUC12s for LIS basin.
# sparrow_in <- read.csv("RBEROST-Northeast/Preprocessing/Inputs/ne_sparrow_model_input.csv")
# NOTE: USGS dataset had missing HUC12 values so need to substitute
HUC12_NESPARROW <- read.dbf("Data/lic_flowline_centroidinside_NAD83.dbf", as.is = TRUE) %>%
  rename(HUC_12 = huc12) %>%
  mutate(HUC12_Char = str_pad(HUC_12, 12, pad = "0", side = "left"),
         HUC4 = substring(HUC12_Char, 1, 4))

# Subset HUC12s to just those in the Northeast SPARROW model.
#HUC12_NESPARROW <- sparrow_in %>%
#  mutate(HUC12_Char = str_pad(HUC_12, 12, pad = "0", side = "left"),
#         HUC4 = substring(HUC12_Char, 1, 4)) %>%
  # HUC4s here confirmed by Naomi's email on 6/17/25.
#  filter(HUC4 == "0110" | HUC4 == "0108" | HUC4 == "0203")

# Download the WBD file if necessary
url <- "https://prd-tnm.s3.amazonaws.com/StagedProducts/Hydrography/WBD/National/GDB/WBD_National_GDB.zip" #*#
zip_file <- "Data/WBD_National_GDB.zip" #*#
gdb_path <- "Data/WBD_National_GDB.gdb" #*#

# Check if geodatabase already exists
if (!dir.exists(gdb_path)) {
  cat("Downloading WBD National GDB (this may take a while - ~2.6 GB file)...\n")
  
  # Set timeout to 30 minutes (1800 seconds) and increase buffer size
  options(timeout = 1800)
  options(download.file.method = "auto")
  
  # Try download with error handling
  tryCatch({
    download.file(url, zip_file, mode = "wb", method = "auto")
    cat("Download complete.\n")
    
    # Verify file size
    file_size <- file.info(zip_file)$size
    cat(sprintf("Downloaded file size: %.1f MB\n", file_size / 1024^2))
    
    cat("Extracting geodatabase...\n")
    unzip(zip_file, exdir = "Data")
    cat("Extraction complete.\n")
    
  }, error = function(e) {
    cat("Download failed with error:", e$message, "\n")
    cat("You may need to download the file manually from:\n")
    cat(url, "\n")
    cat("And place it in the Data/ folder.\n")
    
    # Clean up partial download
    if (file.exists(zip_file)) {
      file.remove(zip_file)
    }
  })
  
} else {
  cat("Geodatabase already exists, skipping download and extraction.\n")
}

# Clean up zip file (only if extraction was successful)
if (file.exists(zip_file) && dir.exists(gdb_path)) {
  file.remove(zip_file)
}

##########################################################
# 3. Calculate nitrogen and phosphorus removal efficiency ####
##########################################################

# Rename column names
names(ACRE_rawdata) <- c(
  "HUC12", "LULC", "Slope", "Scenario", "TP", "TN", "STD_Archive", "Hyd_Group","HUC12b"
  )

# TN column had some "<" symbols in it so need to convert it from character to numeric.
ACRE_rawdata$TP <- as.numeric(str_replace_all(ACRE_rawdata$TP, "<", ""))
ACRE_rawdata$TN <- as.numeric(str_replace_all(ACRE_rawdata$TN, "<", ""))
ACRE_rawdata$Hyd_Group <- str_replace_all(ACRE_rawdata$Hyd_Group, "<", "")

 ACRE_rawdataQA <- ACRE_rawdata %>%
  filter(str_detect(TP, "<") | str_detect(TN, "<") | str_detect(Hyd_Group, "<"))

# Subset the data to just take out "no practice" & "baseline" runs 
baseline_subset_temp_bsln <- filter(ACRE_rawdata, Scenario == "Baseline")
baseline_subset_bsln <- baseline_subset_temp_bsln %>% select(HUC12, Scenario, TP, TN, STD_Archive)
names(baseline_subset_bsln) <- c(
  "HUC12", "Scenario_Baseline", "Baseline_TP", "Baseline_TN", "STD_Archive"
  )

baseline_subset_temp_noprac <- filter(ACRE_rawdata, Scenario == "No Practice")
baseline_subset_noprac <- baseline_subset_temp_noprac %>% select(HUC12, Scenario, TP, TN, STD_Archive)
names(baseline_subset_noprac) <- c(
  "HUC12", "Scenario_Baseline", "Baseline_TP", "Baseline_TN", "STD_Archive"
  )

# Subset the data for all runs except the "no practice" & "baseline" runs
scenarios_subset_bsln <- filter(ACRE_rawdata, !(Scenario == "Baseline"))
names(scenarios_subset_bsln) <- c(
  "HUC12",
  "LULC", 
  "Slope",
  "Scenario", 
  "Scenario_TP", 
  "Scenario_TN", 
  "STD_Archive", 
  "Hyd_Group"
  )

scenarios_subset_noprac <- filter(ACRE_rawdata, !(Scenario == "No Practice"))
names(scenarios_subset_noprac) <- c(
  "HUC12",
  "LULC", 
  "Slope",
  "Scenario", 
  "Scenario_TP", 
  "Scenario_TN", 
  "STD_Archive", 
  "Hyd_Group"
  )

# Merge the two dataframes together, by STD_Archive and HUC12 
efficiency_subset_bsln <- merge(
  baseline_subset_bsln, 
  scenarios_subset_bsln, 
  by = c("STD_Archive", "HUC12"), 
  all = TRUE
  )

efficiency_subset_noprac <- merge(
  baseline_subset_noprac, 
  scenarios_subset_noprac, 
  by = c("STD_Archive", "HUC12"), 
  all = TRUE
  )

# Add BMP Efficieny for TN, named "TN_efcy" 
efficiency_subset_bsln$TP_efcy <- with(
  efficiency_subset_bsln, (as.numeric(Baseline_TP) - as.numeric(Scenario_TP)) / as.numeric(Baseline_TP)
  )
efficiency_subset_bsln$TN_efcy <- with(
  efficiency_subset_bsln, (as.numeric(Baseline_TN) - as.numeric(Scenario_TN)) / as.numeric(Baseline_TN)
  )

efficiency_subset_noprac$TP_efcy <- with(
  efficiency_subset_noprac, (as.numeric(Baseline_TP) - as.numeric(Scenario_TP)) / as.numeric(Baseline_TP)
  )
efficiency_subset_noprac$TN_efcy <- with(
  efficiency_subset_noprac, (as.numeric(Baseline_TN) - as.numeric(Scenario_TN)) / as.numeric(Baseline_TN)
  )

# Create a single ponds BMP to merge back into the efficiency dataset.
efficiency_ponds_bsln <- efficiency_subset_bsln %>%
  select(
    STD_Archive, HUC12, LULC, Slope, Scenario, Hyd_Group, TP_efcy, TN_efcy
    ) %>%
  filter(Scenario %in% c("Ponds 25%", "Ponds 50%", "Ponds 75%"))

efficiency_ponds_noprac <- efficiency_subset_noprac %>%
  select(
    STD_Archive, HUC12, LULC, Slope, Scenario, Hyd_Group, TP_efcy, TN_efcy
    ) %>%
  filter(Scenario %in% c("Ponds 25%", "Ponds 50%", "Ponds 75%"))

# Adjust the efficiency values based on the percentage of land flowing to the pond.
efficiency_ponds_bsln$pond_perc <- 
  as.numeric(substr(efficiency_ponds_bsln$Scenario,7,8))
efficiency_ponds_bsln$TP_efcy <- efficiency_ponds_bsln$TP_efcy * (efficiency_ponds_bsln$pond_perc / 100)
efficiency_ponds_bsln$TN_efcy <- efficiency_ponds_bsln$TN_efcy * (efficiency_ponds_bsln$pond_perc / 100)
efficiency_ponds_bsln["pond_perc"] <- NULL

efficiency_ponds_noprac$pond_perc <- 
  as.numeric(substr(efficiency_ponds_noprac$Scenario,7,8))
efficiency_ponds_noprac$TP_efcy <- efficiency_ponds_noprac$TP_efcy * (efficiency_ponds_noprac$pond_perc / 100)
efficiency_ponds_noprac$TN_efcy <- efficiency_ponds_noprac$TN_efcy * (efficiency_ponds_noprac$pond_perc / 100)
efficiency_ponds_noprac["pond_perc"] <- NULL

# Average the pond efficiencies.
efficiency_pond_ave_bsln <- efficiency_ponds_bsln %>%
  group_by(STD_Archive, HUC12, LULC, Slope, Hyd_Group) %>%
  summarise(TN_efcy = mean(TN_efcy), TP_efcy = mean(TP_efcy))
efficiency_pond_ave_bsln$Scenario <- "Ponds"

efficiency_pond_ave_noprac <- efficiency_ponds_noprac %>%
  group_by(STD_Archive, HUC12, LULC, Slope, Hyd_Group) %>%
  summarise(TN_efcy = mean(TN_efcy), TP_efcy = mean(TP_efcy))
efficiency_pond_ave_noprac$Scenario <- "Ponds"

# Filter out the non-relevant scenarios from the ACRE database.
efficiency_subset_scen_bsln <- efficiency_subset_bsln %>%
  select(
    STD_Archive, HUC12, LULC, Slope, Scenario, Hyd_Group, TP_efcy, TN_efcy
    ) %>%
  filter(
    !Scenario %in% c(
      "Ponds 25%", "Ponds 50%", "Ponds 75%", "Fert 75%", "Fert 90%"
      )
    ) %>%
  filter(LULC %in% c("ALFA", "CORN", "CORN-SOYB", "RYE")) #*# 
# Alfalfa, corn, soybeans and rye are the crops plausibly grown in VT & NH. oats, peanuts, tobacco, tomatoes and wheat and sorgum are likely misclassifications

efficiency_subset_scen_noprac <- efficiency_subset_noprac %>%
  select(
    STD_Archive, HUC12, LULC, Slope, Scenario, Hyd_Group, TP_efcy, TN_efcy
    ) %>%
  filter(
    !Scenario %in% c(
      "Ponds 25%", "Ponds 50%", "Ponds 75%", "Fert 75%", "Fert 90%"
      )
    ) %>%
  filter(LULC %in% c("ALFA", "CORN", "CORN-SOYB", "RYE")) #*# 
# Alfalfa, corn, soybeans and rye are the crops plausibly grown in VT & NH. oats, peanuts, tobacco, tomatoes and wheat and sorgum are likely misclassifications


# Merge the filtered scenario dataset with the updated pond dataset.
efficiency_subset_final_bsln <- dplyr::bind_rows(
  efficiency_subset_scen_bsln, efficiency_pond_ave_bsln
  )

efficiency_subset_final_noprac <- dplyr::bind_rows(
  efficiency_subset_scen_noprac, efficiency_pond_ave_noprac
  )

##########################################################
# 4. Summarize by HUC12, LULC, Slope, and Hyd_Group ####
##########################################################

Eff_byHUC12_HRU_bsln <- efficiency_subset_final_bsln %>%
  select(STD_Archive, HUC12, Scenario, TP_efcy, TN_efcy) %>%
  group_by(HUC12,Scenario) %>% 
  summarise(
    MeanTP_Effic = mean(TP_efcy), 
    MeanTN_Effic = mean(TN_efcy),
    MeanTP_Effic_se = sd(TP_efcy)/sqrt(n()), 
    MeanTN_Effic_se = sd(TN_efcy)/sqrt(n())
    ) %>%
  mutate(across(contains("_se"), ~replace_na(., 0)))

Eff_byHUC12_HRU_bsln$HUC12 <- 
  str_pad(Eff_byHUC12_HRU_bsln$HUC12, width=12, pad="0")

Eff_byHUC12_HRU_noprac <- efficiency_subset_final_noprac %>%
  select(STD_Archive, HUC12, Scenario, TP_efcy, TN_efcy) %>%
  group_by(HUC12,Scenario) %>% 
  summarise(
    MeanTP_Effic = mean(TP_efcy), 
    MeanTN_Effic = mean(TN_efcy),
    MeanTP_Effic_se = sd(TP_efcy)/sqrt(n()), 
    MeanTN_Effic_se = sd(TN_efcy)/sqrt(n())
    ) %>%
  mutate(across(contains("_se"), ~replace_na(., 0)))

Eff_byHUC12_HRU_noprac$HUC12 <- 
  str_pad(Eff_byHUC12_HRU_noprac$HUC12, width=12, pad="0")

# Summarize by HUC10
Eff_byHUC12_HRU_bsln$HUC10 <- substring(Eff_byHUC12_HRU_bsln$HUC12,1,10)

Eff_byHUC10_HRU_bsln <- Eff_byHUC12_HRU_bsln %>%
  ungroup() %>%
  select(HUC10, HUC10_Scen = Scenario, MeanTP_Effic, MeanTN_Effic) %>%
  group_by(HUC10, HUC10_Scen) %>%
  summarise(
    HUC10TP_Effic = mean(MeanTP_Effic), 
    HUC10TN_Effic = mean(MeanTN_Effic),
    HUC10TP_Effic_se = sd(MeanTP_Effic)/sqrt(n()), 
    HUC10TN_Effic_se = sd(MeanTN_Effic)/sqrt(n())
    ) %>%
  mutate(across(contains("_se"), ~replace_na(., 0)))

Eff_byHUC12_HRU_noprac$HUC10 <- substring(Eff_byHUC12_HRU_noprac$HUC12,1,10)

Eff_byHUC10_HRU_noprac <- Eff_byHUC12_HRU_noprac %>%
  ungroup() %>%
  select(HUC10, HUC10_Scen = Scenario, MeanTP_Effic, MeanTN_Effic) %>%
  group_by(HUC10, HUC10_Scen) %>%
  summarise(
    HUC10TP_Effic = mean(MeanTP_Effic), 
    HUC10TN_Effic = mean(MeanTN_Effic),
    HUC10TP_Effic_se = sd(MeanTP_Effic)/sqrt(n()), 
    HUC10TN_Effic_se = sd(MeanTN_Effic)/sqrt(n())
    ) %>%
  mutate(across(contains("_se"), ~replace_na(., 0)))

# Summarize by HUC8
Eff_byHUC12_HRU_bsln$HUC8 <- substring(Eff_byHUC12_HRU_bsln$HUC12,1,8)

Eff_byHUC8_HRU_bsln <- Eff_byHUC12_HRU_bsln %>%
  ungroup() %>%
  select(HUC8, HUC8_Scen = Scenario, MeanTP_Effic, MeanTN_Effic) %>%
  group_by(HUC8, HUC8_Scen) %>%
  summarise(
    HUC8TP_Effic = mean(MeanTP_Effic),
    HUC8TN_Effic = mean(MeanTN_Effic),
    HUC8TP_Effic_se = sd(MeanTP_Effic)/sqrt(n()), 
    HUC8TN_Effic_se = sd(MeanTN_Effic)/sqrt(n())
    )%>%
  mutate(across(contains("_se"), ~replace_na(., 0)))

Eff_byHUC12_HRU_noprac$HUC8 <- substring(Eff_byHUC12_HRU_noprac$HUC12,1,8)

Eff_byHUC8_HRU_noprac <- Eff_byHUC12_HRU_noprac %>%
  ungroup() %>%
  select(HUC8, HUC8_Scen = Scenario, MeanTP_Effic, MeanTN_Effic) %>%
  group_by(HUC8, HUC8_Scen) %>%
  summarise(
    HUC8TP_Effic = mean(MeanTP_Effic),
    HUC8TN_Effic = mean(MeanTN_Effic),
    HUC8TP_Effic_se = sd(MeanTP_Effic)/sqrt(n()), 
    HUC8TN_Effic_se = sd(MeanTN_Effic)/sqrt(n())
    )%>%
  mutate(across(contains("_se"), ~replace_na(., 0)))

##########################################################
# 5. Fill in any HUC12 gaps ####
##########################################################

# Merge the HUC12 list and HUC12-level efficiency data.
HUC12 <- HUC12_NESPARROW[,"HUC12_Char"]

HUC12_List <- data.frame(unique(HUC12))

names(HUC12_List) <- c("HUC12")

Efficiency_HUC12_bsln <- 
  merge(HUC12_List, Eff_byHUC12_HRU_bsln, by="HUC12", all.x=TRUE)

summary(Efficiency_HUC12_bsln)

Efficiency_HUC12_noprac <- 
  merge(HUC12_List, Eff_byHUC12_HRU_noprac, by="HUC12", all.x=TRUE)

summary(Efficiency_HUC12_noprac)

#Merge the HUC10-level efficiency data.
Efficiency_HUC12_bsln$HUC10 <- substring(Efficiency_HUC12_bsln$HUC12,1,10)
Efficiency_HUC10_bsln <- merge(
  Efficiency_HUC12_bsln, Eff_byHUC10_HRU_bsln, by="HUC10", all = TRUE
  )

Efficiency_HUC10_bsln$MeanTP_Effic <- with(
  Efficiency_HUC10_bsln, 
  ifelse(is.na(MeanTP_Effic), HUC10TP_Effic, MeanTP_Effic)
  )
Efficiency_HUC10_bsln$MeanTN_Effic <- with(
  Efficiency_HUC10_bsln, 
  ifelse(is.na(MeanTN_Effic), HUC10TN_Effic, MeanTN_Effic)
  )
Efficiency_HUC10_bsln$MeanTP_Effic_se <- with(
  Efficiency_HUC10_bsln, 
  ifelse(is.na(MeanTP_Effic_se), HUC10TP_Effic_se, MeanTP_Effic_se)
  )
Efficiency_HUC10_bsln$MeanTN_Effic_se <- with(
  Efficiency_HUC10_bsln,
  ifelse(is.na(MeanTN_Effic_se), HUC10TN_Effic_se, MeanTN_Effic_se)
  )
Efficiency_HUC10_bsln$Scenario <- with(
  Efficiency_HUC10_bsln, ifelse(is.na(Scenario), HUC10_Scen, Scenario)
  )

summary(Efficiency_HUC10_bsln)

Efficiency_HUC12_noprac$HUC10 <- substring(Efficiency_HUC12_noprac$HUC12,1,10)
Efficiency_HUC10_noprac <- merge(
  Efficiency_HUC12_noprac, Eff_byHUC10_HRU_noprac, by="HUC10", all = TRUE
  )

Efficiency_HUC10_noprac$MeanTP_Effic <- with(
  Efficiency_HUC10_noprac, 
  ifelse(is.na(MeanTP_Effic), HUC10TP_Effic, MeanTP_Effic)
  )
Efficiency_HUC10_noprac$MeanTN_Effic <- with(
  Efficiency_HUC10_noprac, 
  ifelse(is.na(MeanTN_Effic), HUC10TN_Effic, MeanTN_Effic)
  )
Efficiency_HUC10_noprac$MeanTP_Effic_se <- with(
  Efficiency_HUC10_noprac, 
  ifelse(is.na(MeanTP_Effic_se), HUC10TP_Effic_se, MeanTP_Effic_se)
  )
Efficiency_HUC10_noprac$MeanTN_Effic_se <- with(
  Efficiency_HUC10_noprac,
  ifelse(is.na(MeanTN_Effic_se), HUC10TN_Effic_se, MeanTN_Effic_se)
  )
Efficiency_HUC10_noprac$Scenario <- with(
  Efficiency_HUC10_noprac, ifelse(is.na(Scenario), HUC10_Scen, Scenario)
  )

summary(Efficiency_HUC10_noprac)

# Merge the HUC8-level efficiency data.
Efficiency_HUC10_bsln$HUC8 <- substring(Efficiency_HUC10_bsln$HUC12,1,8)
Efficiency_HUC8_bsln <- merge(
  Efficiency_HUC10_bsln, Eff_byHUC8_HRU_bsln, by="HUC8", all = TRUE
  )

Efficiency_HUC8_bsln$MeanTP_Effic <- with(
  Efficiency_HUC8_bsln, ifelse(is.na(MeanTP_Effic), HUC8TP_Effic, MeanTP_Effic)
  )
Efficiency_HUC8_bsln$MeanTN_Effic <- with(
  Efficiency_HUC8_bsln, ifelse(is.na(MeanTN_Effic), HUC8TN_Effic, MeanTN_Effic)
  )
Efficiency_HUC8_bsln$MeanTP_Effic_se <- with(
  Efficiency_HUC8_bsln, 
  ifelse(is.na(MeanTP_Effic_se), HUC8TP_Effic_se, MeanTP_Effic_se)
  )
Efficiency_HUC8_bsln$MeanTN_Effic_se <- with(
  Efficiency_HUC8_bsln, 
  ifelse(is.na(MeanTN_Effic_se), HUC8TN_Effic_se, MeanTN_Effic_se)
  )
Efficiency_HUC8_bsln$Scenario <- with(
  Efficiency_HUC8_bsln, ifelse(is.na(Scenario), HUC8_Scen, Scenario)
  )

summary(Efficiency_HUC8_bsln)

Efficiency_HUC10_noprac$HUC8 <- substring(Efficiency_HUC10_noprac$HUC12,1,8)
Efficiency_HUC8_noprac <- merge(
  Efficiency_HUC10_noprac, Eff_byHUC8_HRU_noprac, by="HUC8", all = TRUE
  )

Efficiency_HUC8_noprac$MeanTP_Effic <- with(
  Efficiency_HUC8_noprac, 
  ifelse(is.na(MeanTP_Effic), HUC8TP_Effic, MeanTP_Effic)
  )
Efficiency_HUC8_noprac$MeanTN_Effic <- with(
  Efficiency_HUC8_noprac, 
  ifelse(is.na(MeanTN_Effic), HUC8TN_Effic, MeanTN_Effic)
  )
Efficiency_HUC8_noprac$MeanTP_Effic_se <- with(
  Efficiency_HUC8_noprac, 
  ifelse(is.na(MeanTP_Effic_se), HUC8TP_Effic_se, MeanTP_Effic_se)
  )
Efficiency_HUC8_noprac$MeanTN_Effic_se <- with(
  Efficiency_HUC8_noprac, 
  ifelse(is.na(MeanTN_Effic_se), HUC8TN_Effic_se, MeanTN_Effic_se)
  )
Efficiency_HUC8_noprac$Scenario <- with(
  Efficiency_HUC8_noprac, ifelse(is.na(Scenario), HUC8_Scen, Scenario)
  )

summary(Efficiency_HUC8_noprac)

# Are there any NA values in the efficiency data?
table(is.na(Efficiency_HUC8_bsln$MeanTN_Effic))
table(is.na(Efficiency_HUC8_bsln$MeanTN_Effic_se))
table(is.na(Efficiency_HUC8_bsln$MeanTP_Effic))
table(is.na(Efficiency_HUC8_bsln$MeanTP_Effic_se))

# Yes, which HUC8s are missing data?
HUC8_NA <- Efficiency_HUC8_bsln %>%
  filter(is.na(MeanTN_Effic) | is.na(MeanTP_Effic))

# Missing HUC8s
unique(HUC8_NA$HUC8) # "02030101" "02030102" "02030103" "02030104" "02030105" "02030201" "02030202" "02030203"

table(is.na(Efficiency_HUC8_noprac$MeanTN_Effic))
table(is.na(Efficiency_HUC8_noprac$MeanTN_Effic_se))
table(is.na(Efficiency_HUC8_noprac$MeanTP_Effic))
table(is.na(Efficiency_HUC8_noprac$MeanTP_Effic_se))

# Yes, which HUC8s are missing data?
HUC8_NA_noprac <- Efficiency_HUC8_noprac %>%
  filter(is.na(MeanTN_Effic) | is.na(MeanTP_Effic)) %>%
  select(HUC8, HUC8_Scen, MeanTN_Effic, MeanTP_Effic)

# Missing HUC8s
unique(HUC8_NA_noprac$HUC8) 

#@ SE: After revising the input crosswalk, the 0203 HUC4 watershed (Long Island) does not exist in the ACREs data provided by Naomi.

# Find Closest watersheds to missing watersheds to fill in missing data --------

## Baseline --------------------------------------------------------------------

# Find HUC8s with missing data
HUC8_NA <- Efficiency_HUC8_bsln %>%
  filter(is.na(MeanTN_Effic) | is.na(MeanTP_Effic))

# Import watershed boundaries
HUC8_bounds <- sf::st_read("Data/WBD_National_GDB.gdb", layer = "WBDHU8") #*#
HUC12_bounds <- sf::st_read("Data/WBD_National_GDB.gdb", layer = "WBDHU12") #*#

# Create centroids for HUC12s
HUC12_centroid <- sf::st_point_on_surface(HUC12_bounds)

# Filter HUC12 centroids to only those with valid efficiency data
HUC12_known <- Efficiency_HUC8_bsln %>% filter(!is.na(MeanTN_Effic) & !is.na(MeanTP_Effic))
HUC12_centroid_LIS <- HUC12_centroid %>%
  filter(huc12 %in% HUC12_known$HUC12)

# Get boundaries for missing HUC8s
HUC8_LIS_missing <- HUC8_bounds %>%
  filter(huc8 %in% HUC8_NA$HUC8)

# For each missing HUC8, find the closest HUC12 with data
nearest_huc12_idx <- sf::st_nearest_feature(sf::st_centroid(HUC8_LIS_missing), HUC12_centroid_LIS)
nearest_huc12 <- HUC12_centroid_LIS[nearest_huc12_idx, ]
nearest_huc12$HUC8_missing <- HUC8_LIS_missing$huc8

# Get efficiency values for the nearest HUC12s
fill_data <- left_join(
  nearest_huc12 %>% as.data.frame() %>% select(huc12, HUC8_missing),
  Efficiency_HUC8_bsln %>% select(HUC12, Scenario, MeanTP_Effic, MeanTN_Effic, MeanTP_Effic_se, MeanTN_Effic_se, contains("HUC10"), contains("HUC8")),
  by = c("huc12" = "HUC12")
)

# Fill in the missing HUC8s in Efficiency_HUC8_bsln
Efficiency_HUC8_bsln_filled <- Efficiency_HUC8_bsln

for(i in seq_len(nrow(fill_data))) {
  idx <- which(Efficiency_HUC8_bsln_filled$HUC8 == fill_data$HUC8_missing[i])
  if(length(idx) > 0) {
    Efficiency_HUC8_bsln_filled$Scenario[idx] <- fill_data$Scenario[i]
    Efficiency_HUC8_bsln_filled$MeanTP_Effic[idx] <- fill_data$MeanTP_Effic[i]
    Efficiency_HUC8_bsln_filled$MeanTN_Effic[idx] <- fill_data$MeanTN_Effic[i]
    Efficiency_HUC8_bsln_filled$MeanTP_Effic_se[idx] <- fill_data$MeanTP_Effic_se[i]
    Efficiency_HUC8_bsln_filled$MeanTN_Effic_se[idx] <- fill_data$MeanTN_Effic_se[i]
    Efficiency_HUC8_bsln_filled$HUC10_Scen[idx] <- fill_data$HUC10_Scen[i]
    Efficiency_HUC8_bsln_filled$HUC10TP_Effic[idx] <- fill_data$HUC10TP_Effic[i]
    Efficiency_HUC8_bsln_filled$HUC10TN_Effic[idx] <- fill_data$HUC10TN_Effic[i]
    Efficiency_HUC8_bsln_filled$HUC10TP_Effic_se[idx] <- fill_data$HUC10TP_Effic_se[i]
    Efficiency_HUC8_bsln_filled$HUC10TN_Effic_se[idx] <- fill_data$HUC10TN_Effic_se[i]
    Efficiency_HUC8_bsln_filled$HUC8_Scen[idx] <- fill_data$HUC8_Scen[i]
    Efficiency_HUC8_bsln_filled$HUC8TP_Effic[idx] <- fill_data$HUC8TP_Effic[i]
    Efficiency_HUC8_bsln_filled$HUC8TN_Effic[idx] <- fill_data$HUC8TN_Effic[i]
    Efficiency_HUC8_bsln_filled$HUC8TP_Effic_se[idx] <- fill_data$HUC8TP_Effic_se[i]
    Efficiency_HUC8_bsln_filled$HUC8TN_Effic_se[idx] <- fill_data$HUC8TN_Effic_se[i]
  }
}
  

## No Practice ----------------------------------------------------------------

# Find HUC8s with missing data
HUC8_NA_noprac <- Efficiency_HUC8_noprac %>%
  filter(is.na(MeanTN_Effic) | is.na(MeanTP_Effic))

# Filter HUC12 centroids to only those with valid efficiency data
HUC12_known_noprac <- Efficiency_HUC8_noprac %>% filter(!is.na(MeanTN_Effic) & !is.na(MeanTP_Effic))
HUC12_centroid_LIS_noprac <- HUC12_centroid %>%
  filter(huc12 %in% HUC12_known_noprac$HUC12)

# Get boundaries for missing HUC8s
HUC8_LIS_missing_noprac <- HUC8_bounds %>%
  filter(huc8 %in% HUC8_NA_noprac$HUC8)

# For each missing HUC8, find the closest HUC12 with data
nearest_huc12_idx_noprac <- sf::st_nearest_feature(sf::st_centroid(HUC8_LIS_missing_noprac), HUC12_centroid_LIS)
nearest_huc12_noprac <- HUC12_centroid_LIS[nearest_huc12_idx_noprac, ]
nearest_huc12_noprac$HUC8_missing <- HUC8_LIS_missing_noprac$huc8

# Get efficiency values for the nearest HUC12s
fill_data_noprac <- left_join(
  nearest_huc12_noprac %>% as.data.frame() %>% select(huc12, HUC8_missing),
  Efficiency_HUC8_noprac %>% select(HUC12, Scenario, MeanTP_Effic, MeanTN_Effic, MeanTP_Effic_se, MeanTN_Effic_se, contains("HUC10"), contains("HUC8")),
  by = c("huc12" = "HUC12")
)

# Fill in the missing HUC8s in Efficiency_HUC8_bsln
Efficiency_HUC8_noprac_filled <- Efficiency_HUC8_noprac

for(i in seq_len(nrow(fill_data))) {
  idx <- which(Efficiency_HUC8_noprac_filled$HUC8 == fill_data$HUC8_missing[i])
  if(length(idx) > 0) {
    Efficiency_HUC8_noprac_filled$Scenario[idx] <- fill_data_noprac$Scenario[i]
    Efficiency_HUC8_noprac_filled$MeanTP_Effic[idx] <- fill_data_noprac$MeanTP_Effic[i]
    Efficiency_HUC8_noprac_filled$MeanTN_Effic[idx] <- fill_data_noprac$MeanTN_Effic[i]
    Efficiency_HUC8_noprac_filled$MeanTP_Effic_se[idx] <- fill_data_noprac$MeanTP_Effic_se[i]
    Efficiency_HUC8_noprac_filled$MeanTN_Effic_se[idx] <- fill_data_noprac$MeanTN_Effic_se[i]
    Efficiency_HUC8_noprac_filled$HUC10_Scen[idx] <- fill_data_noprac$HUC10_Scen[i]
    Efficiency_HUC8_noprac_filled$HUC10TP_Effic[idx] <- fill_data_noprac$HUC10TP_Effic[i]
    Efficiency_HUC8_noprac_filled$HUC10TN_Effic[idx] <- fill_data_noprac$HUC10TN_Effic[i]
    Efficiency_HUC8_noprac_filled$HUC10TP_Effic_se[idx] <- fill_data_noprac$HUC10TP_Effic_se[i]
    Efficiency_HUC8_noprac_filled$HUC10TN_Effic_se[idx] <- fill_data_noprac$HUC10TN_Effic_se[i]
    Efficiency_HUC8_noprac_filled$HUC8_Scen[idx] <- fill_data_noprac$HUC8_Scen[i]
    Efficiency_HUC8_noprac_filled$HUC8TP_Effic[idx] <- fill_data_noprac$HUC8TP_Effic[i]
    Efficiency_HUC8_noprac_filled$HUC8TN_Effic[idx] <- fill_data_noprac$HUC8TN_Effic[i]
    Efficiency_HUC8_noprac_filled$HUC8TP_Effic_se[idx] <- fill_data_noprac$HUC8TP_Effic_se[i]
    Efficiency_HUC8_noprac_filled$HUC8TN_Effic_se[idx] <- fill_data_noprac$HUC8TN_Effic_se[i]
  }
}

# Reduce the data
drop_col <- c(
  "HUC10_Scen", 
  "HUC10TP_Effic", 
  "HUC10TN_Effic", 
  "HUC10TP_Effic_se", 
  "HUC10TN_Effic_se", 
  "HUC8_Scen", 
  "HUC8TP_Effic",
  "HUC8TN_Effic",
  "HUC8TP_Effic_se",
  "HUC8TN_Effic_se"
  )

Efficiency_Sum_bsln <- 
  Efficiency_HUC8_bsln_filled[,!names(Efficiency_HUC8_bsln_filled) %in% drop_col]
Efficiency_Sum_bsln <- unique(Efficiency_Sum_bsln)
Efficiency_Sum_bsln <- Efficiency_Sum_bsln[order(Efficiency_Sum_bsln$HUC12),]

Efficiency_Sum_bsln <- Efficiency_Sum_bsln %>% 
  mutate(
    MeanTN_Effic_se = case_when(
        MeanTN_Effic_se > 0 ~ MeanTN_Effic_se, 
        MeanTN_Effic_se == 0 ~ 0.1 * abs(MeanTN_Effic)
        ),
    MeanTP_Effic_se = case_when(
        MeanTP_Effic_se > 0 ~ MeanTP_Effic_se, 
        MeanTP_Effic_se == 0 ~ 0.1 * abs(MeanTP_Effic)
        ),
    # Limit extreme negative values in efficiency
    MeanTN_Effic = case_when(
        MeanTN_Effic < -1 ~ -1, 
        MeanTN_Effic > 1 ~ 1, 
        TRUE ~ MeanTN_Effic
        ),
    MeanTP_Effic = case_when(
        MeanTP_Effic < -1 ~ -1, 
        MeanTP_Effic > 1 ~ 1, 
        TRUE ~ MeanTP_Effic
        )
      )

Efficiency_Sum_noprac <- 
  Efficiency_HUC8_noprac_filled[,!names(Efficiency_HUC8_noprac) %in% drop_col]
Efficiency_Sum_noprac <- unique(Efficiency_Sum_noprac)
Efficiency_Sum_noprac <- 
  Efficiency_Sum_noprac[order(Efficiency_Sum_noprac$HUC12),]

Efficiency_Sum_noprac <- Efficiency_Sum_noprac %>% 
  mutate(
    MeanTN_Effic_se = case_when(
        MeanTN_Effic_se > 0 ~ MeanTN_Effic_se, 
        MeanTN_Effic_se == 0 ~ 0.1 * abs(MeanTN_Effic)
        ),
    MeanTP_Effic_se = case_when(
        MeanTP_Effic_se > 0 ~ MeanTP_Effic_se, 
        MeanTP_Effic_se == 0 ~ 0.1 * abs(MeanTP_Effic)
        ),
    # Limit extreme negative values in efficiency
    MeanTN_Effic = case_when(
      MeanTN_Effic < -1 ~ -1, 
      MeanTN_Effic > 1 ~ 1, 
      TRUE ~ MeanTN_Effic
    ),
    MeanTP_Effic = case_when(
      MeanTP_Effic < -1 ~ -1, 
      MeanTP_Effic > 1 ~ 1, 
      TRUE ~ MeanTP_Effic
    )
  )

# QA CHECKS
# Check that the HUC12s are the same in both datasets
#if(!all(Efficiency_Sum_bsln$HUC12 == Efficiency_Sum_noprac$HUC12)) {
#  stop("HUC12s do not match between baseline and no practice datasets.")
#}

# Check that the number of rows are the same in both datasets
if(nrow(Efficiency_Sum_bsln) != nrow(Efficiency_Sum_noprac)) {
  stop("Number of rows do not match between baseline and no practice datasets.")
}

# Check the efficiency values are within reasonable ranges (-1 to 1)
min(Efficiency_Sum_bsln$MeanTN_Effic, na.rm = T) # -1
max(Efficiency_Sum_bsln$MeanTN_Effic, na.rm = T) # 1

min(Efficiency_Sum_bsln$MeanTP_Effic, na.rm = T) # -1
max(Efficiency_Sum_bsln$MeanTP_Effic, na.rm = T) # 1

# Are there any watersheds without efficiency values?
if(any(is.na(Efficiency_Sum_bsln$MeanTN_Effic) | is.na(Efficiency_Sum_bsln$MeanTP_Effic))) {
  stop("There are NA values in the efficiency dataset.")
}

if(any(is.na(Efficiency_Sum_noprac$MeanTN_Effic) | is.na(Efficiency_Sum_noprac$MeanTP_Effic))) {
  stop("There are NA values in the no practice efficiency dataset.")
}

##########################################################
# 6. Add constant reduction efficiency for cover crop based on Griffith et al. 2020
# https://link.springer.com/article/10.1007/s11270-020-4443-z
##########################################################
CoverCropHUCs <- unique(Efficiency_Sum_bsln[c("HUC8", "HUC10","HUC12")]) %>%
  mutate(Scenario = 'Cover_crops', MeanTN_Effic = .628, MeanTN_Effic_se = 0,MeanTP_Effic = .677, MeanTP_Effic_se= 0)
Efficiency_Sum_bsln <- bind_rows(Efficiency_Sum_bsln,CoverCropHUCs)
Efficiency_Sum_noprac <- bind_rows(Efficiency_Sum_noprac,CoverCropHUCs)

##########################################################
# 7. Export the summarized database ####
##########################################################

write.csv(
  Efficiency_Sum_bsln, 
  "./RBEROST-Northeast/Preprocessing/Inputs/ACRE_HUC12_HRU_Summary_compareBaseline_ICF25ND.csv", #*#
  row.names=FALSE
)

write.csv(
  Efficiency_Sum_noprac, 
  "./RBEROST-Northeast/Preprocessing/Inputs/ACRE_HUC12_HRU_Summary_compareNoPractice_ICF25ND.csv", #*#
  row.names=FALSE
)
