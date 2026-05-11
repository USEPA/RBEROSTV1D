# PURPOSE: Calculate Loading and Limits on Riparian Buffers 
# BY: Cathy Chamberlin 
# DATE:  1/28/2021 
# UPDATED: Alyssa Le, Sam Ennett
# DATE: 5/20/2025
# Updated with S Shore watersheds: Naomi Detenbeck
# Date: 1/4/2026
# Updated with final SPARROW loading data - just run through #7 and save new riparian loads csv file
# Use old efficiency and length in buffer files
# Date: 4/25/26

# This step follows work in ArcGIS to calculate the least effective HSG in each reaches' riparian buffer, the slope in each reaches' riparian buffer, and the land use in each reaches' riparian buffer
# Lines of code tagged with #*# may need to be edited by the user.

# 1. Setup --------------------------------------
packages <- c('tidyverse', 'foreach', 'doParallel', 'sf', 'data.table', 'measurements')
# lapply(packages, install.packages) #*# # Run this code if packages are not installed
lapply(packages, library, character.only = T)
rm(list=ls())

# Import the environment
# load("RBEROST-Northeast/QA/riparianefficiencies_bycomid_tmp_dist.RData")
beepr::beep()

# source helper functions
# source("E:/Ellen/Detenbeck/LIS/R/Optimization_HelperFunctions-Northeast.R")
source("RBEROST-Northeast/R/Optimization_HelperFunctions-Northeast.R")

working_dir <- "C:/Users/ndetenbe/OneDrive - Environmental Protection Agency (EPA)/RBEROSTV1D/"

# 2. Load Data ----------------------------------

datafolder <- "Data/" #*#
out        <- paste0(working_dir,"RBEROST-Northeast/Preprocessing/Inputs/") #*#

# Download the NHD files if necessary
url <- "https://dmap-data-commons-ow.s3.amazonaws.com/NHDPlusV21/Data/" #*#
# Fixed naming of regions which was backwards - this should not affect outputs (ND)
region2 <- "NHDPlusMA/NHDPlusV21_MA_02_NHDSnapshot_04.7z"
region1 <- "NHDPlusNE/NHDPlusV21_NE_01_NHDSnapshot_04.7z"

# Create NHDFlowlines directory if it doesn't exist
if (!dir.exists("Data/NHDFlowlines")) {
  dir.create("Data/NHDFlowlines", recursive = TRUE)
}

# Function to download and extract NHD regions
download_nhd_region <- function(region_path, region_name) {
  
  full_url <- paste0(url, region_path)
  zip_file <- file.path("Data", basename(region_path))
  flowline_output <- file.path("Data/NHDFlowlines", paste0("NHDFlowline_", region_name, ".shp"))
  
  # Check if flowline shapefile already exists
  if (!file.exists(flowline_output)) {
    cat(sprintf("Processing NHD %s region...\n", region_name))
    
    # Set timeout for large files
    options(timeout = 1800)
    
    tryCatch({
      # Download the 7z file
      cat(sprintf("Downloading %s (this may take a while)...\n", basename(region_path)))
      download.file(full_url, zip_file, mode = "wb", method = "auto")
      cat("Download complete.\n")
      
      # Extract the 7z file (requires 7zip to be installed)
      cat("Extracting archive...\n")
      
      # Try different extraction methods
      if (Sys.which("7z") != "") {
        # Use 7zip command line
        system(sprintf('7z x "%s" -o"%s"', zip_file, "Data/temp_extract"))
      } else if (require(archive, quietly = TRUE)) {
        # Use R archive package
        archive::archive_extract(zip_file, dir = "Data/temp_extract")
      } else {
        stop("7zip or archive package required for .7z extraction. Please install 7zip or run: install.packages('archive')")
      }
      
      # Find the NHDFlowline.shp file in extracted folders
      flowline_files <- list.files("Data/temp_extract", 
                                   pattern = "NHDFlowline.*\\.shp$", 
                                   recursive = TRUE, 
                                   full.names = TRUE)
      
      if (length(flowline_files) > 0) {
        # Copy the flowline shapefile and associated files
        flowline_dir <- dirname(flowline_files[1])
        flowline_base <- tools::file_path_sans_ext(basename(flowline_files[1]))
        
        # Copy all shapefile components (.shp, .shx, .dbf, .prj, etc.)
        file_extensions <- c(".shp", ".shx", ".dbf", ".prj", ".cpg", ".xml")
        for (ext in file_extensions) {
          src_file <- file.path(flowline_dir, paste0(flowline_base, ext))
          if (file.exists(src_file)) {
            dest_file <- file.path("Data/NHDFlowlines", paste0("NHDFlowline_", region_name, ext))
            file.copy(src_file, dest_file, overwrite = TRUE)
          }
        }
        
        cat(sprintf("Extracted NHDFlowline_%s.shp successfully.\n", region_name))
      } else {
        stop("Could not find NHDFlowline.shp in extracted files")
      }
      
      # Clean up temporary files
      unlink("Data/temp_extract", recursive = TRUE)
      file.remove(zip_file)
      
    }, error = function(e) {
      cat("Download/extraction failed with error:", e$message, "\n")
      cat("You may need to download the file manually from:\n")
      cat(full_url, "\n")
      
      # Clean up partial downloads
      if (file.exists(zip_file)) file.remove(zip_file)
      if (dir.exists("Data/temp_extract")) unlink("Data/temp_extract", recursive = TRUE)
    })
    
  } else {
    cat(sprintf("NHDFlowline_%s.shp already exists, skipping download.\n", region_name))
  }
}

# Download both regions
download_nhd_region(region1, "Region1")
download_nhd_region(region2, "Region2")

# AL: Note: there will need to be a programmatic way to set the year.
#year       <- "2001" #*#
year       <- "2019" # switched to 2019 to be consistent with documentation
buffer.options <- c(20, 40, 60, 80, 100, 300, 400)
# year.options <- c(2001, 2004, 2006, 2008, 2011, 2013, 2016, 2019)

# Updating flowlines for LIS area
region1_flowlines <- read_sf(paste0(datafolder, "NHDFlowlines/", "NHDFlowline_Region1.shp")) %>%
  mutate(lengthkm_se = sqrt(
    (conv_unit(sqrt(0.02^2 + 0.003^2) * 100000, "inch", "km")) ^ 2 + # error in each direction for each point consists of a 90% tolerance of 0.02 in (to map scale). The digitized maps have a tolerance of 0.003 in (to map scale). Not knowing exactly how these tolerances interact, I will assume that the uncertainty is similar to if they were added together. The model is using the medium resolution NHD+ data, which has a resolution of 1:100,000. Inches are then converted to kilometers.
      (conv_unit(sqrt(0.02^2 + 0.003^2) * 100000, "inch", "km")) ^ 2 + 
      (conv_unit(sqrt(0.02^2 + 0.003^2) * 100000, "inch", "km")) ^ 2 + 
      (conv_unit(sqrt(0.02^2 + 0.003^2) * 100000, "inch", "km")) ^ 2 )
    ) # error in stream length depends on error in the location of the start and end point, in the x and y directions, so 4 total components.

region2_flowlines <- read_sf(paste0(datafolder, "NHDFlowlines/", "NHDFlowline_Region2.shp")) %>%
  mutate(lengthkm_se = sqrt(
    (conv_unit(sqrt(0.02^2 + 0.003^2) * 100000, "inch", "km")) ^ 2 + # error in each direction for each point consists of a 90% tolerance of 0.02 in (to map scale). The digitized maps have a tolerance of 0.003 in (to map scale). Not knowing exactly how these tolerances interact, I will assume that the uncertainty is similar to if they were added together. The model is using the medium resolution NHD+ data, which has a resolution of 1:100,000. Inches are then converted to kilometers.
      (conv_unit(sqrt(0.02^2 + 0.003^2) * 100000, "inch", "km")) ^ 2 + 
      (conv_unit(sqrt(0.02^2 + 0.003^2) * 100000, "inch", "km")) ^ 2 + 
      (conv_unit(sqrt(0.02^2 + 0.003^2) * 100000, "inch", "km")) ^ 2 )
  )

LIS_flowlines <- rbind(region1_flowlines, region2_flowlines) %>%
  select(COMID = COMID, lengthkm = LENGTHKM, lengthkm_se, reachcode = REACHCODE) %>%
  # Reproject to Albers Equal Area
  st_transform("ESRI:102003")

rm(region1_flowlines, region2_flowlines)

# Slope, NATSGO and NLCD data are read in below in parallel processing. Users can edit file paths tagged with #*#

## TN Model -------------------------------------------------------------------
sparrow_cons_out_tn <- fread(
#  paste(out,"Predict_NoBFlowN_WSeptic.csv",sep=""), data.table = FALSE) #*# %>%  
  paste(out,"Predict_NoBFlowN_WSeptic_final.csv",sep=""), data.table = FALSE) %>%
#  paste(out,"Predict_NoBFlowN_WSeptic_CTC.csv",sep=""), data.table = FALSE) %>%  
  # TN model is missing a total incremental load variable, create one:
  mutate(
    PLOAD_INC_TOTAL = rowSums(
      cbind(
        PLOAD_INC_PMN, PLOAD_INC_ATN, PLOAD_INC_SCS, PLOAD_INC_URB,
        PLOAD_INC_BFN, PLOAD_INC_AFN, PLOAD_INC_DFN, PLOAD_INC_AMN,
        PLOAD_INC_ST, PLOAD_INC_STO, SepticLoadtoReachkgN
      ),
      na.rm = TRUE
    ),
    year_season = str_sub(comid_time, -3),
    year = str_sub(year_season, 1, 2)
  ) %>%
  rename(in_total = PLOAD_INC_TOTAL,
         # Point Source Loads
         in_poin = PLOAD_INC_PMN,
         # Agricultural Loads 
         # in_fert_ag is zero because n.s. in SPARROW model so consider in_manu to be combined
         # fert + manure
         in_fert_ag = PLOAD_INC_AFN, #@ SE: ag fertilizer will be applied to a land, dev fertilizer will be applied to urban areas
         in_manu = PLOAD_INC_AMN,
         # Atmospheric Loads
         in_atmo = PLOAD_INC_ATN,
         # Urban Land Loads
         in_urb = PLOAD_INC_URB,
         in_fert_dev = PLOAD_INC_DFN,
         # Septic load
         in_septic = SepticLoadtoReachkgN,
         COMID = comid
  ) %>%
# Calculate standard error for in_fert_ag by COMID
  group_by(COMID) %>%
  mutate(
    in_fert_ag_se = sd(in_fert_ag, na.rm = TRUE) / sqrt(n())
  ) %>%
  ungroup()

# Create correct season column
if (any(is.na(sparrow_cons_out_tn$season))) {
  sparrow_cons_out_tn$season <- str_sub(sparrow_cons_out_tn$year_season, 3)
}

# Create correct comid column
if (any(is.na(sparrow_cons_out_tn$COMID)) | !is.character(sparrow_cons_out_tn$COMID)) {
  sparrow_cons_out_tn$COMID <- as.integer(str_sub(sparrow_cons_out_tn$comid_time, 1, -4))
}

## TP Model -------------------------------------------------------------------
sparrow_cons_out_tp <- fread(
#  paste(out,"Predict_P.csv",sep=""), data.table = FALSE) #*# %>%
  paste(out,"Predict_P_final.csv",sep=""), data.table = FALSE) %>%
#  paste(out,"Predict_P_CTC.csv",sep=""), data.table = FALSE) %>% 
  
#  mutate(ip = PLOAD_INC_PMP+PLOAD_INC_SED+PLOAD_INC_SCS+PLOAD_INC_URB+PLOAD_INC_FOR+PLOAD_INC_AFP+PLOAD_INC_ATN+PLOAD_INC_AMP+PLOAD_INC_ST+PLOAD_INC_STO, 
#         year_season = str_sub(comid_time, -3), #PLOAD_INC_SED removed, no longer significant
  mutate(ip = PLOAD_INC_PMP+PLOAD_INC_SCS+PLOAD_INC_URB+PLOAD_INC_FOR+PLOAD_INC_AFP+PLOAD_INC_ATN+PLOAD_INC_AMP+PLOAD_INC_ST+PLOAD_INC_STO, 
                year_season = str_sub(comid_time, -3),
         year = str_sub(year_season, 1, 2)) %>%
  rename(ip_poin = PLOAD_INC_PMP,
         ip_fert = PLOAD_INC_AFP,
         ip_manu = PLOAD_INC_AMP,
#         ip_rock = PLOAD_INC_SED,
         ip_rock = 0,         
         ip_forest = PLOAD_INC_FOR,
         ip_atmo = PLOAD_INC_ATN,
         ip_urb = PLOAD_INC_URB) %>% #*#
  # Calculate standard error for ip_fert and ip_forest by COMID
  mutate(
    ip_fert_se = sd(ip_fert, na.rm = TRUE) / sqrt(n()),
    ip_forest_se = sd(ip_forest, na.rm = TRUE) / sqrt(n())
  )

# Create correct season column
if (any(is.na(sparrow_cons_out_tp$season))) {
  sparrow_cons_out_tp$season <- str_sub(sparrow_cons_out_tp$year_season, 3)
}

# Create correct comid column
if (any(is.na(sparrow_cons_out_tp$comid)) | !is.character(sparrow_cons_out_tp$comid)) {
  sparrow_cons_out_tp$COMID <- as.integer(str_sub(sparrow_cons_out_tp$comid_time, 1, -4))
}

# 3. Summarize landcover by buffer widths --------

# AL: Have other data years, but using 2001 to align with the seasonal model
# ND: Switched to 2019 to be consistent with documentation for dynamic LIS RBEROST
landcovers_bycomid_tmp <-fread(paste0(datafolder, "LIS_by_comidtables/landcovers_", year, "_bycomid_tmp.csv"))#*#

landcovers_bycomid <- landcovers_bycomid_tmp %>%
  tidyr::complete(COMID, buffersize, landcover) %>%
  group_by(COMID, landcover) %>%
  arrange(buffersize) %>%
  fill(value, .direction = "downup") %>%
  pivot_wider(
    id_cols = "COMID",
    names_from = c("landcover", "buffersize"),
    values_from = "value"
  ) %>%
  rename_with(
    .fn = ~paste0(
      lapply(str_split(., "_"), "[[", 1),
      "_",
      buffer.options[as.numeric(lapply(str_split(., "_"), "[[", 2))],
      "ft"
    ),
    .cols = !COMID
  ) %>%
 mutate(cv_numpixels = 0.17) # Wickham et al 2017 report overall 83% accuracy in level II data (including all categories, such as Med Development, High Development, etc.)

# 4. Choose slope category for each comid -----------

# This parallel process will load each slope data file from the data folder, format it as necessary, then combine all the slope data

slopes_bycomid <- foreach(
  i = 1:length(buffer.options), .combine = "rbind"
) %do% {
  
  dat <- read.csv(
    paste0(datafolder, "LIS_by_comidtables/Slope_", buffer.options[i], "ft_bycomid_featurd.csv") #*#
  )
  
  dat.munged <- dat %>%
    mutate(
      value = case_when(
        between(MEAN, 0, 5) ~ "0-5%", 
        between(MEAN, 5, 10) ~ "5-10%", 
        between(MEAN, 10, 15) ~ "10-15%",
        MEAN > 15 ~ ">15%"
      ),
      buffersize = buffer.options[i]
    ) %>%
    select(COMID, value, buffersize) 
  
  dat.munged
} %>%
  tidyr::complete(COMID, buffersize) %>%
  group_by(COMID) %>%
  arrange(buffersize) %>%
  fill(value, .direction = "downup") %>%
  pivot_wider(
    id_cols = "COMID", names_from = "buffersize", names_glue = "meanslope_{buffersize}ft", values_from = "value"
  )

slopes_bycomid_dist <- foreach(
  i = 1:length(buffer.options), .combine = "rbind"
) %do% {
  
  dat <- read.csv(
    paste0(datafolder, "LIS_by_comidtables/Slope_", buffer.options[i], "ft_bycomid_featurd.csv") #*#
  )
  
  dat.munged <- dat %>%
    rowwise() %>%
    mutate(resampledslopes = list(rnorm(n = 10, mean = MEAN, sd = STD))) %>%
    mutate(
      value = list(
        case_when(
          resampledslopes < 5 ~ "0-5%",
          between(resampledslopes, 5, 10) ~ "5-10%",
          between(resampledslopes, 10, 15) ~ "10-15%",
          resampledslopes > 15 ~ ">15%"
        )
      ),
      buffersize = buffer.options[i]
    ) %>%
    select(COMID, value, buffersize)
  
  dat.munged
} %>%
  tidyr::complete(COMID, buffersize) %>%
  group_by(COMID) %>%
  arrange(buffersize) %>%
  fill(value, .direction = "downup") %>%
  pivot_wider(
    id_cols = "COMID", names_from = "buffersize", names_glue = "meanslope_resampled_{buffersize}ft", values_from = "value"
  ) 

# 5. Summarize Hydrologic Soil Group by buffer widths --------

# This parallel process will load each soils data file from the data folder, format it as necessary, then combine all the NATSGO data

## Pasted in from modified script
hsg_bycomid_tmp <- foreach(
  i = 1:length(buffer.options), .combine = "rbind"
) %do% {
  
  dat <- fread(
    paste0(datafolder, "LIS_by_comidtables/soils_", buffer.options[i], "ft_bycomid_featurd.csv") #*#
  )
  
  dat.munged <- as.data.frame(t(dat)) %>%
    slice(-2:-1) %>%
    rename_with(.cols = everything(), .fn = ~ as.character(dat$LABEL)) %>%
    rownames_to_column(var = "id") %>%
    mutate(COMID = as.numeric(gsub("featu_", "", id))) %>%
    select(-id) %>%
    pivot_longer(cols = "1":"4", names_to = "HSG") %>%
    mutate(
      buffersize = i,
      HSG = case_when(
        HSG == "1" ~ "A",
        HSG == "2" ~ "B",
        HSG == "3" ~ "C",
        HSG == "4" ~ "D"
      )
  )

  dat.munged
  
} 

# Pivot the data wider to make it more human - readable
hsg_bycomid <- hsg_bycomid_tmp %>%
  tidyr::complete(COMID, buffersize, HSG) %>%
  group_by(COMID, HSG) %>%
  arrange(buffersize) %>%
  fill(value, .direction = "downup") %>%
  pivot_wider(
    id_cols = "COMID", 
    names_from = c("HSG", "buffersize"), 
    values_from = "value"
  ) %>%
  rename_with(
    .fn = ~paste0(
      lapply(str_split(., "_"), "[[", 1), 
      "_", 
      buffer.options[as.numeric(lapply(str_split(., "_"), "[[", 2))], 
      "ft"
    ), 
    .cols = !COMID
  )

hsg_bycomid_toresample <- hsg_bycomid_tmp  %>%
  tidyr::complete(COMID, buffersize, HSG) %>%
  group_by(COMID, HSG) %>%
  arrange(buffersize) %>%
  fill(value, .direction = "downup") %>%
  ungroup() %>%
  group_by(COMID, buffersize) %>%
  summarize(resampledist = list(c(rep(x = HSG, times = value))))

# 6. Compute bank lengths for each comid ----------------------------

river.lengths <- LIS_flowlines %>%
  mutate(
    totalbanklength_km = lengthkm * 2, 
    totalbanklength_ft = conv_unit(totalbanklength_km, "km", "ft"),
    totalbanklength_km_se = lengthkm_se * 2,
    totalbanklength_ft_se = conv_unit(totalbanklength_km_se, "km", "ft")
  ) %>%
  st_drop_geometry()

# 6.5 Calculate Uncertainty in PLER ---------------------------------

## Table 2 in Green_Credit_Report_Final.pdf gives Pollutant Load Export Rates (PLER) by land use. The values are based on Table 3-1 in Attachment 3 of Appendix F of the 2017 NH MS4 permit

P.export.rates <- data.frame(
  P_SourceCategory_LandUse = c(
    "COM+IND", 
    "MFR+HDR", 
    "MDR", 
    "LDR",
    "HWY", 
    "FOR", 
    "OPEN", 
    "AG", 
    "DevPERV_A", 
    "DevPERV_B", 
    "DevPERV_C", 
    "DevPERV_CD",
    "DevPERV_D"
    ),
  Ploadexport_lb_acreyear = c(
    1.78, 2.32, 1.96, 1.52, 1.34, 1.52, 1.52, 1.52, 0.03, 0.12, 0.21, 0.29, 0.37
    ),
  LandUse = c(
    "Commercial+Transportation", 
    "Residential", 
    "Residential", 
    "LowResidential",
    "Commercial+Transportation", 
    NA_character_, 
    NA_character_, 
    NA_character_, 
    "DevPERV_A", 
    "DevPERV_B", 
    "DevPERV_C",
    "DevPERV_CD",
    "DevPERV_D"
    )
  ) %>%
  pivot_wider(
    id_cols = LandUse, 
    names_from = P_SourceCategory_LandUse, 
    values_from = Ploadexport_lb_acreyear
    ) %>%
  fill(contains("DevPERV"), .direction = "updown") %>%
  select(-c(FOR, OPEN, AG)) %>%
  pivot_longer(
    cols = -c("LandUse", contains("DevPERV")), 
    names_to = "P_SourceCategory_LandUse", 
    values_to = "Ploadexport_lb_acreyear"
    ) %>%
  drop_na() %>%
  pivot_longer(
    cols = contains("DevPERV"), 
    names_to = "DevPERV_P_SourceCategory_LandUse", 
    values_to = "DevPERV_Ploadexport_lb_acreyear"
    ) 

N.export.rates <- data.frame(
  N_SourceCategory_LandUse = c(
    "COM+IND", 
    "AllRes", 
    "AllRes", 
    "HWY", 
    "FOR", 
    "OPEN", 
    "AG", 
    "DevPERV_A", 
    "DevPERV_B", 
    "DevPERV_C", 
    "DevPERV_CD",
    "DevPERV_D"
    ),
  Nloadexport_lb_acreyear = c(
    15, 14.1, 14.1, 10.5, 11.3, 11.3, 11.3, 0.3, 1.2, 2.4, 3.1, 3.6
    ),
  LandUse = c(
    "Commercial+Transportation", 
    "Residential", 
    "LowResidential",
    "Commercial+Transportation", 
    NA_character_, 
    NA_character_, 
    NA_character_, 
    "DevPERV_A", 
    "DevPERV_B", 
    "DevPERV_C",
    "DevPERV_CD",
    "DevPERV_D"
    )
  ) %>%
  pivot_wider(
    id_cols = LandUse, 
    names_from = N_SourceCategory_LandUse, 
    values_from = Nloadexport_lb_acreyear
    ) %>%
  fill(contains("DevPERV"), .direction = "updown") %>%
  select(-c(FOR, OPEN, AG)) %>%
  pivot_longer(
    cols = -c("LandUse", contains("DevPERV")), 
    names_to = "N_SourceCategory_LandUse", 
    values_to = "Nloadexport_lb_acreyear"
    ) %>%
  drop_na() %>%
  pivot_longer(
    cols = contains("DevPERV"), 
    names_to = "DevPERV_N_SourceCategory_LandUse", 
    values_to = "DevPERV_Nloadexport_lb_acreyear"
    ) 

GC_landuses <- data.frame(
  LandUse = c(
    rep("LowResidential", 36), 
    rep("Residential", 25), 
    rep("Commercial+Transportation", 40)
    ),
  PercDensityofIC = 0:100 / 100
  ) %>%
  mutate(PercDevPERV = (1 - PercDensityofIC)) 

P.exports <- GC_landuses%>%
  full_join(., P.export.rates, by = "LandUse") %>%
  mutate(
    PLER = 
      PercDensityofIC * Ploadexport_lb_acreyear + 
      PercDevPERV * DevPERV_Ploadexport_lb_acreyear
    )

N.exports <- GC_landuses%>%
  full_join(., N.export.rates, by = "LandUse") %>%
  mutate(
    PLER = 
      PercDensityofIC * Nloadexport_lb_acreyear + 
      PercDevPERV * DevPERV_Nloadexport_lb_acreyear
    )

GC_PLER_estimates = data.frame(
  LandUse = c("LowResidential", "Residential", "Commercial+Transportation"),
  PLER_TP = c(0.55, 1.07, 1.16),
  PLER_TN = c(3.8, 6.2, 9.3)
  ) 

P_export_ses <- P.exports %>%
  group_by(LandUse) %>%
  summarize(standarddev = sd(PLER)/ sqrt(n()))

N_export_ses <- N.exports %>%
  group_by(LandUse) %>%
  summarize(standarddev = sd(PLER) / sqrt(n()))

# 7.Calculate Riparian Loading for each comid -------------------------------

  # QA: Are there COMIDs reflected in landcovers_bycomid that are not in SPARROW?
  landcovers_bycomid_QA <- landcovers_bycomid %>%
    filter(!COMID %in% sparrow_cons_out_tn$COMID)

  # QA: Are there COMIDs in LIS flowlines that are not in landcovers_bycomid and also in the SPARROW model?
  landcovers_bycomid_missingNHD <- LIS_flowlines %>%
    filter(COMID %in% sparrow_cons_out_tn$COMID) %>%
    filter(!COMID %in% landcovers_bycomid$COMID)

  write_csv(landcovers_bycomid_missingNHD, "RBEROST-Northeast/QA/landcover_missing_from_NHD_ICF25.csv")

# Calculate loading based on Green_Credits_Report_Final.pdf
# Incorporating dynamic SPARROW loads to effect seasonality
  # TN: in_fert_ag
  # TP: ip_fert, ip_forest
riparianloadings <- landcovers_bycomid %>%
  select(COMID, Lowdev_400ft, Meddev_300ft, Highdev_100ft, cv_numpixels) %>%
  mutate(
    contrib_lowres_m2 = as.numeric(Lowdev_400ft) * 30 * 30, # pixels are 30 m x 30 m
    contrib_lowres_acres = conv_unit(contrib_lowres_m2, "m2", "acre"), 
    contrib_res_m2 = as.numeric(Meddev_300ft) * 30 * 30,# pixels are 30 m x 30 m
    contrib_res_acres = conv_unit(contrib_res_m2, "m2", "acre"), 
    contrib_commerc_m2 = as.numeric(Highdev_100ft) * 30 * 30,# pixels are 30 m x 30 m
    contrib_commerc_acres = conv_unit(contrib_commerc_m2, "m2", "acre"),
    contrib_lowres_acres_se = 
      conv_unit(as.numeric(Lowdev_400ft) * cv_numpixels * 30 * 30, "m2", "acre"),
    contrib_res_acres_se = 
      conv_unit(as.numeric(Meddev_300ft) * cv_numpixels * 30 * 30, "m2", "acre"),
    contrib_commerc_acres_se = 
      conv_unit(as.numeric(Highdev_100ft) * cv_numpixels * 30 * 30, "m2", "acre")
  ) %>% 
  mutate(
    P_lowres_PLER_lb_acyr = 0.55, 
    P_res_PLER_lb_acyr = 1.07,
    P_commerc_PLER_lb_acyr = 1.16,
    N_lowres_PLER_lb_acyr = 3.8,
    N_res_PLER_lb_acyr = 6.2,
    N_commerc_PLER_lb_acyr = 9.3,
    P_lowres_PLER_lb_acyr_se = P_export_ses[[2,2]],
    P_res_PLER_lb_acyr_se = P_export_ses[[3,2]],
    P_commerc_PLER_lb_acyr_se = P_export_ses[[1,2]],
    N_lowres_PLER_lb_acyr_se = N_export_ses[[2,2]],
    N_res_PLER_lb_acyr_se = N_export_ses[[3,2]],
    N_commerc_PLER_lb_acyr_se = N_export_ses[[1,2]]
  ) %>%
  mutate(
    P_PLER_lb_yr = contrib_lowres_acres * P_lowres_PLER_lb_acyr + 
      contrib_res_acres * P_res_PLER_lb_acyr + 
      contrib_commerc_acres * P_commerc_PLER_lb_acyr,
    N_PLER_lb_yr = contrib_lowres_acres * N_lowres_PLER_lb_acyr + 
      contrib_res_acres * N_res_PLER_lb_acyr + 
      contrib_commerc_acres * N_commerc_PLER_lb_acyr,
    P_PLER_kg_yr = conv_unit(P_PLER_lb_yr, "lbs", "kg"),
    N_PLER_kg_yr = conv_unit(N_PLER_lb_yr, "lbs", "kg")
  ) %>%
  rowwise() %>%
  mutate(
    P_PLER_lb_yr_se = my_propogateerror(
      vals = list(
        if(contrib_lowres_acres > 0) {
          c(
            contrib_lowres_acres * P_lowres_PLER_lb_acyr, 
            my_propogateerror(
              vals = list(
                c(contrib_lowres_acres, contrib_lowres_acres_se), 
                c(P_lowres_PLER_lb_acyr, P_lowres_PLER_lb_acyr_se)
                ),
              method = "mult"
              )
            )
          } else {c(1,0)},
        if(contrib_res_acres > 0) {
          c(
            contrib_res_acres * P_res_PLER_lb_acyr,           
            my_propogateerror(
              vals = list(
                c(contrib_res_acres, contrib_res_acres_se), 
                c(P_res_PLER_lb_acyr, P_res_PLER_lb_acyr_se)
                ),
              method = "mult"
              )
            )
          } else {c(1,0)},
        if(contrib_commerc_acres > 0) {
          c(
            contrib_commerc_acres * P_commerc_PLER_lb_acyr,          
            my_propogateerror(
              vals = list(
                c(contrib_commerc_acres, contrib_commerc_acres_se), 
                c(P_commerc_PLER_lb_acyr, P_commerc_PLER_lb_acyr_se)
                ),
              method = "mult"
              )
            )
        } else{c(1,0)}
        ), 
      method = "addsub"
      ),
    N_PLER_lb_yr_se = my_propogateerror(
      vals = list(
        if(contrib_lowres_acres > 0) {
          c(
            contrib_lowres_acres * N_lowres_PLER_lb_acyr, 
            my_propogateerror(
              vals = list(
                c(contrib_lowres_acres, contrib_lowres_acres_se), 
                c(N_lowres_PLER_lb_acyr, N_lowres_PLER_lb_acyr_se)
                ),
              method = "mult"
              )
            )
          } else {c(1,0)},
        if(contrib_res_acres > 0) {
          c(
            contrib_res_acres * N_res_PLER_lb_acyr,           
            my_propogateerror(
              vals = list(
                c(contrib_res_acres, contrib_res_acres_se), 
                c(N_res_PLER_lb_acyr, N_res_PLER_lb_acyr_se)
                ),
              method = "mult"
              )
            )
          } else {c(1,0)},
        if(contrib_commerc_acres > 0) {
          c(
            contrib_commerc_acres * N_commerc_PLER_lb_acyr,          
            my_propogateerror(
              vals = list(
                c(contrib_commerc_acres, contrib_commerc_acres_se), 
                c(N_commerc_PLER_lb_acyr, N_commerc_PLER_lb_acyr_se)
                ),
              method = "mult"
              )
            )
        } else{c(1,0)}
        ), 
      method = "addsub"
      )
  ) %>%
  select(COMID,P_PLER_lb_yr_se,N_PLER_lb_yr_se,N_PLER_kg_yr,P_PLER_kg_yr) %>%
  mutate(
    P_PLER_kg_yr_se = conv_unit(P_PLER_lb_yr_se, "lbs", "kg"),
    N_PLER_kg_yr_se = conv_unit(N_PLER_lb_yr_se, "lbs", "kg"),
  ) %>%
  replace_na(
    list(
      N_PLER_kg_yr = 0, 
      P_PLER_kg_yr = 0, 
      P_PLER_kg_yr_se = 0,
      N_PLER_kg_yr_se = 0
      )
    ) %>%
  select(
    COMID, 
    N_riparian_developed_kgyr = N_PLER_kg_yr, 
    P_riparian_developed_kgyr = P_PLER_kg_yr,
    N_riparian_developed_kgyr_se = N_PLER_kg_yr_se, 
    P_riparian_developed_kgyr_se = P_PLER_kg_yr_se
  )  

riparianloadings_seasonal <- riparianloadings %>%
  left_join(sparrow_cons_out_tn %>% select(comid_time,COMID, season, year, in_total, in_fert_ag, in_fert_ag_se), by="COMID") %>% 
  left_join(sparrow_cons_out_tp %>% select(comid_time,ip, ip_fert, ip_forest, ip_fert_se, ip_forest_se), by="comid_time") %>%
  mutate(
    N_riparian_kgyr = N_riparian_developed_kgyr + in_fert_ag,
    N_riparian_kgyr_se = my_propogateerror(
      vals = list(
        c(N_riparian_developed_kgyr, N_riparian_developed_kgyr_se), 
        c(in_fert_ag, in_fert_ag_se)
      ),
      method = "addsub"
    )
  ) %>%
  mutate(
    P_riparian_kgyr = P_riparian_developed_kgyr + ip_fert + ip_forest,
    P_riparian_kgyr_se = my_propogateerror(
      vals = list(
        c(P_riparian_developed_kgyr, P_riparian_developed_kgyr_se), 
        c(ip_fert, ip_fert_se),
        c(ip_forest, ip_forest_se)
      ),
      method = "addsub"
    )
  ) %>%
  select(
    COMID,
    season,
    year,
    N_riparian_kgyr, 
    P_riparian_kgyr, 
    N_riparian_kgyr_se, 
    P_riparian_kgyr_se
  )

beepr::beep()

# 8.Calculate current river length in buffer already -----------------------------

# Use the landcover data and calculate total area in forest and herbaceous.
# Divide total area by buffer width to get the bank length

bufferedlengths_tmp <- foreach(
  i = 1:length(buffer.options), .combine = "rbind", .packages = "tidyverse"
  ) %do% {
  
  bufwid <- buffer.options[i]
  
  landcovers_bycomid %>%
    select(COMID, contains(paste0(bufwid,"ft")), cv_numpixels) %>%
    mutate(across(contains(paste0(bufwid,"ft")), ~ as.numeric(.))) %>%
    rename_with(
      .fn = ~gsub(paste0("_", bufwid, "ft"), '', .), 
      .cols = contains(paste(bufwid))
    ) %>%
    right_join(., river.lengths, by = "COMID") %>%
    select(
      COMID, 
      Forest, 
      Grass, 
      totalbanklength_ft, 
      cv_numpixels, 
      totalbanklength_ft_se
      ) %>%
    mutate(
      bufferwidth = bufwid,
      Forest = case_when(is.na(Forest) ~ 0, TRUE ~ Forest),
      Grass = case_when(is.na(Grass) ~ 0, TRUE ~ Grass)
    ) %>%
    mutate(
      Forest_area_m2 = Forest * 30 * 30,
      Grass_area_m2 = Grass * 30 * 30,
      Forest_area_ft2 = conv_unit(Forest_area_m2, "m2", "ft2"),
      Grass_area_ft2 = conv_unit(Grass_area_m2, "m2", "ft2"),
      Forest_area_ft2_se = Forest_area_ft2 * cv_numpixels,
      Grass_area_ft2_se = Grass_area_ft2 * cv_numpixels
    ) %>%
    mutate(
      Forest_bufferlength_ft = Forest_area_ft2 / bufferwidth,
      Grass_bufferlength_ft = Grass_area_ft2 / bufferwidth,
      Forest_bufferlength_ft_se = my_propogateerror(
        vals = list(
          if(Forest_area_ft2 > 0) {
            c(Forest_area_ft2, Forest_area_ft2_se)
            } else {c(1,0)}, 
          c(bufferwidth, 0)
          ), 
        method = "div"
        ),
      Grass_bufferlength_ft_se = my_propogateerror(
        vals = list(
          if(Grass_area_ft2 >0) {
            c(Grass_area_ft2, Grass_area_ft2_se)
            } else {c(1,0)}, 
          c(bufferwidth, 0)
          ), 
        method = "div"
        )
    ) %>%
    mutate(
      Forest_bufferlength_ft_rev = case_when(
        Forest_bufferlength_ft > totalbanklength_ft ~ totalbanklength_ft,
        Forest_bufferlength_ft <= totalbanklength_ft ~ Forest_bufferlength_ft
      ),
      Forest_bufferlength_ft_rev_se = case_when(
        Forest_bufferlength_ft > totalbanklength_ft ~ totalbanklength_ft_se,
        Forest_bufferlength_ft <= totalbanklength_ft ~ Forest_bufferlength_ft_se
      ),
      Grass_bufferlength_ft_rev = case_when(
        Grass_bufferlength_ft > 
          totalbanklength_ft - Forest_bufferlength_ft_rev ~ 
          totalbanklength_ft - Forest_bufferlength_ft_rev,
        Grass_bufferlength_ft <= 
          totalbanklength_ft - Forest_bufferlength_ft_rev ~ 
          Grass_bufferlength_ft
      ),
      Grass_bufferlength_ft_rev_se = case_when(
        Grass_bufferlength_ft > 
          totalbanklength_ft - Forest_bufferlength_ft_rev ~ 
          my_propogateerror(
            vals = list(
              c(totalbanklength_ft, totalbanklength_ft_se), 
              c(Forest_bufferlength_ft_rev, Forest_bufferlength_ft_rev_se)
              ), 
            method = "addsub"
            ),
        Grass_bufferlength_ft <= 
          totalbanklength_ft - Forest_bufferlength_ft_rev ~ 
          Grass_bufferlength_ft_se
      )
    )
} 

# Pivot to a wider format that is more human-readible 
bufferedlengths <- bufferedlengths_tmp %>%
  select(
    COMID, 
    bufferwidth_ft = bufferwidth, 
    Grass_buffer_ft = Grass_bufferlength_ft_rev, 
    Forest_buffer_ft = Forest_bufferlength_ft_rev,
    Grass_buffer_ft_se = Grass_bufferlength_ft_rev_se,
    Forest_buffer_ft_se = Forest_bufferlength_ft_rev_se
    ) %>%
  pivot_wider(
    #id_cols = c(bufferwidth_ft, COMID), 
    id_cols = c(COMID),
    names_from = bufferwidth_ft, 
    values_from = c(
      Grass_buffer_ft, Forest_buffer_ft, Grass_buffer_ft_se, Forest_buffer_ft_se
      )
    ) %>%
    right_join(
      ., 
      river.lengths %>% 
        select(COMID, totalbanklength_ft, totalbanklength_ft_se), 
      by = "COMID"
      ) %>% 
    rename_with(
      .fn = ~paste0(gsub("_ft", "", .), "ft_ft"), .cols = contains("buffer")
      )

beepr::beep()

# 9. Enter Efficiency Data ------------------------------------------------

# The data entered here was read off of Green_Credit_Report_Final.pdf

# Create the empty dataframe
base_curve_data_empty <- data.frame(
  expand.grid(
    HSG = c("A", "B", "C", "D"),
    Width = c(20, 35, 100),
    Nutrient = c("N", "P"),
    VegType = c("Grass", "Forest")
  ),
  Efficiency = NA_real_
) 

# Enter data
base_curve_data <-   mutate(
  base_curve_data_empty,
    Efficiency = case_when(
      Width == 100 ~ case_when(
        VegType == "Forest" ~ case_when(
          Nutrient == "N" ~ case_when(
            HSG == "A" ~ 18/100,
            HSG == "B" ~ 28/100,
            HSG == "C" ~ 48/100,
            HSG == "D" ~ 65/100
          ),
          Nutrient == "P" ~ case_when(
            HSG == "A" ~ 45/100,
            HSG == "B" ~ 34/100,
            HSG == "C" ~ 20/100,
            HSG == "D" ~ 11/100
          )
        ),
        VegType == "Grass" ~ case_when(
          Nutrient == "N" ~ case_when(
            HSG == "A" ~ 13/100,
            HSG == "B" ~ 23/100,
            HSG == "C" ~ 39/100,
            HSG == "D" ~ 52/100
          ),
          Nutrient == "P" ~ case_when(
            HSG == "A" ~ 36/100,
            HSG == "B" ~ 27/100,
            HSG == "C" ~ 16/100,
            HSG == "D" ~ 9/100
          )
        )
      ),
      Width == 35 ~ case_when(
        VegType == "Forest" ~ case_when(
          Nutrient == "N" ~ case_when(
            HSG == "A" ~ 5/100,
            HSG == "B" ~ 8/100,
            HSG == "C" ~ 14/100,
            HSG == "D" ~ 20/100
          ),
          Nutrient == "P" ~ case_when(
            HSG == "A" ~ 30/100,
            HSG == "B" ~ 22/100,
            HSG == "C" ~ 13/100,
            HSG == "D" ~ 8/100
          )
        ),
        VegType == "Grass" ~ case_when(
          Nutrient == "N" ~ case_when(
            HSG == "A" ~ 4/100,
            HSG == "B" ~ 6/100,
            HSG == "C" ~ 12/100,
            HSG == "D" ~ 15/100
          ),
          Nutrient == "P" ~ case_when(
            HSG == "A" ~ 24/100,
            HSG == "B" ~ 18/100,
            HSG == "C" ~ 10/100,
            HSG == "D" ~ 6/100
          )
        )
      ),
      Width == 20 ~ case_when(
        VegType == "Forest" ~ case_when(
          Nutrient == "N" ~ case_when(
            HSG == "A" ~ 2/100,
            HSG == "B" ~ 4/100,
            HSG == "C" ~ 8/100,
            HSG == "D" ~ 10/100
          ),
          Nutrient == "P" ~ case_when(
            HSG == "A" ~ 20/100,
            HSG == "B" ~ 13/100,
            HSG == "C" ~ 9/100,
            HSG == "D" ~ 5/100
          )
        ),
        VegType == "Grass" ~ case_when(
          Nutrient == "N" ~ case_when(
            HSG == "A" ~ 2/100,
            HSG == "B" ~ 4/100,
            HSG == "C" ~ 6/100,
            HSG == "D" ~ 8/100
          ),
          Nutrient == "P" ~ case_when(
            HSG == "A" ~ 16/100,
            HSG == "B" ~ 12/100,
            HSG == "C" ~ 7/100,
            HSG == "D" ~ 4/100
          )
        )
      )
    )
  )  

# 10. Compute efficiency curves --------------------------------------

# Phosphorus curves are best fit with an exponential function and Nitrogen curves with a linear function

basecurves <- tibble(
  foreach(
    i = unique(base_curve_data$HSG), 
    .combine = "rbind",
    .packages = c("foreach", "tidyverse"),
    .export = "base_curve_data"
    ) %do% {
    foreach(j = unique(base_curve_data$VegType), .combine = "rbind") %do% {
      foreach(k = unique(base_curve_data$Nutrient), .combine = "rbind") %do% {
        mod1 <- with(
          
          base_curve_data[
            with(
              base_curve_data, which(HSG == i & VegType == j & Nutrient == k)
              ), 
            ],
          if(k == "N") {
            lm(Efficiency ~ (Width))
          } else if(k == "P") {
            lm(Efficiency ~ log(Width))
          }
        )
        
        adj.rsquared <- summary(mod1)$adj.r.squared
        
        curve.form <- if(k == "N") {
          "coef.3 * (coef.1 + coef.2 * x)"
        } else if(k == "P") {
          " coef.3 * (coef.1 + coef.2 * log(x))"
        }
        
        tibble(
          HSG = i, 
          VegType = j, 
          Nutrient = k, 
          Slope = c("0-5%", "5-10%", "10-15%", ">15%"),
          coef.1 = coef(mod1)[1], 
          coef.2 = coef(mod1)[2],
          coef.3 = c(1, 0.75, 0.5, 0),
          curve.form = curve.form,
          adj.r.squared = adj.rsquared
        ) %>%
            rowwise() %>%
  mutate(
    Efficiency = gsub(
      "coef.3", coef.3, gsub(
        "coef.2", coef.2, gsub("coef.1", coef.1, curve.form)
        )
      )
    ) 
      }
    }
  }
)

# View modeled curves
#
# predictions <- expand.grid(
#   x = seq(20, 100),
#   y = NA_real_,
#   HSG = unique(base_curve_data$HSG),
#   VegType = unique(base_curve_data$VegType),
#   Nutrient = unique(base_curve_data$Nutrient),
#   Slope = c("0-5%", "5-10%", "10-15%", ">15%")
# ) %>%
#   rowwise() %>%
#   mutate(
#     coef.1 = basecurves$coef.1[
#       which(paste(HSG, VegType, Nutrient, Slope, sep = "_") ==
#               with(basecurves, paste(HSG, VegType, Nutrient, Slope, sep = "_")))
#     ],
#     coef.2 = basecurves$coef.2[
#       which(paste(HSG, VegType, Nutrient, Slope, sep = "_") ==
#               with(basecurves, paste(HSG, VegType, Nutrient, Slope, sep = "_")))
#     ],
#     coef.3 = basecurves$coef.3[
#       which(paste(HSG, VegType, Nutrient, Slope, sep = "_") ==
#               with(basecurves, paste(HSG, VegType, Nutrient, Slope, sep = "_")))
#     ],
#     formula = basecurves$curve.form[
#       which(paste(HSG, VegType, Nutrient, Slope, sep = "_") ==
#               with(basecurves, paste(HSG, VegType, Nutrient, Slope, sep = "_")))
#     ]
#   ) %>%
#   mutate(expression = gsub('y ~ ', '', formula)) %>%
#   rowwise() %>%
#   mutate(iter = 1, y = eval(parse(text = expression)))
# 
# ggplot(
#   data = predictions %>% filter(Slope == "0-5%"),
#   mapping = aes(
#     x = x,
#     y = y,
#     color = Nutrient,
#     lty = VegType
#   )
# ) + geom_line(lwd = 2) +
#   facet_grid("HSG") +
#   theme_bw() +
#   scale_color_manual(values = c("orange", "green")) +
#   scale_linetype_manual(values = c(9, 1)) +
#   scale_x_continuous(limits = c(20, 100)) +
#   scale_y_continuous(limits = c(0, 70))

# 11. Write efficiency curves and limits for each COMID -------------------------

  # QA: are there any flowlines without associated HSG data?
  hsg_bycomid_QA <- river.lengths %>%
  filter(!COMID %in% hsg_bycomid$COMID)

# These occur along shorelines and river corridors where due to coarser resolution of soils
# data, soils were classified as water or in Canada where HSG data aren't available or in Pawcatuck
# watershed which was left out

  # QA: are there flowlines in LIS flowlines that are missing from HSG data?
  hsg_bycomid_missingNHD <- LIS_flowlines %>%
    filter(COMID %in% sparrow_cons_out_tn$COMID) %>%
    filter(!COMID %in% hsg_bycomid$COMID)
  
  write_csv(hsg_bycomid_missingNHD, "RBEROST-Northeast/QA/hsg_missing_from_NHD_ICF25ND.csv")
  
  # QA: are there flowlines in LIS flowlines that are missing from slope data?
  slopes_bycomid_missingNHD <- LIS_flowlines %>%
    filter(COMID %in% sparrow_cons_out_tn$COMID) %>%
    filter(!COMID %in% slopes_bycomid$COMID)
  
  write_csv(slopes_bycomid_missingNHD, "RBEROST-Northeast/QA/slopes_missing_from_NHD_ICF25ND.csv")
  
  # Most missing data are associated with COMIDs with small areas < 1 km2, less than resolution
  # of slope grid or in Canada (no slope data) or in Pawcatuck (left out)
  
  input_data_QA <- Reduce(intersect, list(landcovers_bycomid_missingNHD$COMID, hsg_bycomid_missingNHD$COMID, slopes_bycomid_missingNHD$COMID))
  
  # Missing data mirror locations with missing hsg and slope - probably joined after slope and/or hsg

riparianefficiencies_bycomid_tmp <- foreach(
  i = 1:length(buffer.options), .combine = "rbind"
  ) %do% {
  
  bufwid <- buffer.options[i]
  
  riparian_efficiencies_tmp <- river.lengths %>%
    select(COMID) %>%
    left_join(
      ., 
      hsg_bycomid %>% 
        select(COMID, contains(paste0(bufwid,"ft"))) %>%
        mutate(across(contains(paste0(bufwid,"ft")), ~ as.numeric(.))) %>%
        rename_with(
          .fn = ~gsub(paste0("_", bufwid, "ft"), '', .), 
          .cols = contains(paste(bufwid))
          ), 
      by= "COMID"
      ) %>%
    mutate(
      HSG_N = case_when(
        A > 0 ~ "A", B > 0 ~ "B", C > 0 ~ "C", D > 0 ~ "D", TRUE ~ "A"
        ),
      HSG_P = case_when(
        D > 0 ~ "D", C > 0 ~ "C", B > 0 ~ "B", A > 0 ~ "A", TRUE ~ "D"
        ),
      bufferwidth_ft = bufwid
    ) %>%
    left_join(
      ., 
      slopes_bycomid %>%  
        select(COMID, contains(paste0(bufwid,"ft"))) %>%
        rename_with(
          .fn = ~gsub(paste0("_", bufwid, "ft"), '', .), 
          .cols = contains(paste(bufwid))
          ), 
      by = "COMID"
      ) 
  
  riparian_efficiencies_tmp
  
  } 

# format the curves for writing to csv

riparianefficiencies_bycomid_vals <- riparianefficiencies_bycomid_tmp %>%
  select(
    COMID, 
    bufferwidth_ft,
    HSG_N,
    HSG_P,
    Slope = meanslope
    ) %>%
  pivot_longer(
    cols = c(HSG_N, HSG_P), names_to = "Nutrient", values_to = "HSG"
    ) %>%
  mutate(
    Nutrient = gsub("HSG_", "", Nutrient),
    VegType = rep(c("Forest", "Grass"), length.out = nrow(.))
    ) %>%
  tidyr::complete(COMID, Nutrient, VegType, bufferwidth_ft) %>%
  group_by(COMID , Nutrient, bufferwidth_ft) %>%
  fill(HSG, Slope, .direction = "downup") %>%
  left_join(., basecurves, by = c("HSG", "VegType", "Nutrient", "Slope")) %>%
  rowwise() %>%
  mutate(
    Efficiency = gsub(
      "coef.3", coef.3, gsub(
        "coef.2", coef.2, gsub("coef.1", coef.1, curve.form)
        )
      )
    ) %>%
  select(COMID, Nutrient, VegType, bufferwidth_ft, Efficiency) %>%
  pivot_wider(
    #id_cols = c(COMID, Nutrient, VegType, bufferwidth_ft), 
    id_cols = c(COMID),
    names_from = c(Nutrient, VegType, bufferwidth_ft), 
    values_from = Efficiency
    ) %>%
  rename_with(.fn = ~paste0(., "ft"), .cols = -COMID)

# Combine all the necessary data to choose an efficiency curve

riparianefficiencies_bycomid_tmp_dist <- foreach(
  i = 1:length(buffer.options), .combine = "rbind", .packages = "tidyverse"
  ) %do% {
  
  bufwid <- buffer.options[i]
  
  temp <- river.lengths %>%
    select(COMID) %>%
    left_join(
      .,
      hsg_bycomid_toresample %>%
        filter(buffersize == i),
      by= "COMID"
      ) %>%
    rowwise() %>%
    mutate(
      HSG_N = list(
        tryCatch(
          sample(x = unlist(resampledist), size = 10, replace = TRUE),
          error = function(e) {list()}
          )
        ),
      HSG_P = list(
        tryCatch(
          sample(x = unlist(resampledist), size = 10, replace = TRUE),
          error = function(e) {list()})
        ),
      bufferwidth_ft = bufwid
    )  %>%
    left_join(
      .,
      slopes_bycomid_dist %>%
        select(COMID, contains(paste0(bufwid,"ft"))) %>%
        rename_with(
          .fn = ~gsub(paste0("_", bufwid, "ft"), '', .),
          .cols = contains(paste(bufwid))
          ),
      by = "COMID"
      )
  
  } 
  # format the curves for writing to csv


riparianefficiencies_bycomid_dist <- riparianefficiencies_bycomid_tmp_dist %>%
  select(
    COMID,
    bufferwidth_ft,
    HSG_N,
    HSG_P,
    Slope = meanslope_resampled
  ) %>%
  pivot_longer(
    cols = c(HSG_N, HSG_P), names_to = "Nutrient", values_to = "HSG"
  ) %>%
  mutate(
    Nutrient = gsub("HSG_", "", Nutrient),
    VegType = rep(c("Forest", "Grass"), length.out = nrow(.))
  ) %>%
  complete(COMID, Nutrient, VegType, bufferwidth_ft) %>%
  group_by(COMID, Nutrient, bufferwidth_ft) %>%
  fill(HSG, Slope, .direction = "downup") %>%
  rowwise %>%
  mutate(
    Efficiency_uncertainty = paste0("c('",
                                    paste(c(basecurves$Efficiency[
                                      foreach(i = 1:10, .combine = "c", .export = c("basecurves")) %do% {
                                        which(
                                          basecurves$Nutrient == Nutrient &
                                            basecurves$VegType == VegType &
                                            basecurves$HSG == HSG[i] &
                                            basecurves$Slope == Slope[i]
                                        )
                                      }
                                    ]), collapse = "', '"
                                    ),
                                    "')"
    )
  ) %>%
  select(COMID, Nutrient, VegType, bufferwidth_ft, Efficiency_uncertainty) %>%
  pivot_wider(
    id_cols = c(COMID),
    names_from = c(Nutrient, VegType, bufferwidth_ft),
    values_from = Efficiency_uncertainty
  ) %>%
  rename_with(.fn = ~paste0(., "ft_uncertainty"), .cols = -COMID)

riparianefficiencies_bycomid <- merge(
  riparianefficiencies_bycomid_vals, 
  riparianefficiencies_bycomid_dist,
  by = 'COMID',
  all = T
)

# 12. Write Files ------------------------------------------------------

write.csv(
  riparianloadings_seasonal, file = paste0(out,"RiparianLoadings_ICF25ND.csv") #*#
)

write.csv(
  bufferedlengths, file = paste0(out,"LengthinBuffer_ICF25ND.csv") #*#
)

write.csv(
  riparianefficiencies_bycomid, 
  file = paste0(out,"RiparianEfficiencies_ICF25ND.csv")
  )

beepr::beep(2)

# QA riparian loadings: spot check shows decent alignment
# ND significantly different loadings in 2025 version
# 253 cases (13 COMIDs) with zero loadings from 2026 version but positive values from 2024 version
# 83825 cases (4192 COMIDs) with positive loadings from 2026 version but zero values from 2024 version (added S Shore?)
# Highest noninfinity ratio is 2336 - possible result of using 2019 land use values instead of 2001 used in 2024
#old_riparianloadings <- fread(paste0(out, "RiparianLoadings_ICF24.csv")) %>%
old_riparianloadings <- fread(paste0(working_dir,"RBEROST-Northeast/Preprocessing/Inputs/","RiparianLoadings_ICF24.csv")) %>%
  select(-V1)

length(unique(riparianloadings$COMID)) # 20257
length(unique(riparianloadings_seasonal$COMID)) # 20257

riparianloadings_compare <- riparianloadings_seasonal %>%
  filter(COMID %in% old_riparianloadings$comid) %>%
  merge(., old_riparianloadings %>% rename_with(.fn = ~paste0(., "_OLD"), .cols = -comid), by.x="COMID", by.y="comid") %>%
  mutate(newoveroldTNload = N_riparian_kgyr/N_riparian_kgyr_OLD)
# QA buffered lengths: spot check shows decent alignment
old_bufferedlengths <- fread(paste0(out, "LengthinBuffer_ICF24.csv")) %>%
  select(-V1)

buffer_compare <- bufferedlengths %>%
  filter(COMID %in% old_bufferedlengths$comid) %>%
  merge(., old_bufferedlengths %>% rename_with(.fn = ~paste0(., "_OLD"), .cols = -comid), by.x="COMID", by.y="comid")
